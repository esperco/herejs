{

open Printf
open Lexing

type var = string

type heredoc_element =
  | Var of var
  | Text of string
  | Ignored_newline

type document_element =
  | Js of string
  | Heredoc of heredoc_element list

type document = document_element list

let pos1 lexbuf = lexbuf.Lexing.lex_start_p
let pos2 lexbuf = lexbuf.Lexing.lex_curr_p
let loc lexbuf = (pos1 lexbuf, pos2 lexbuf)

let init_fname lexbuf fname =
  lexbuf.lex_start_p <- {
    lexbuf.lex_start_p with
    pos_fname = fname;
  };
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
    pos_fname = fname;
  }

let newline lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum
  }

let string_of_loc (pos1, pos2) =
  let open Lexing in
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)

let error_at location msg =
  eprintf "%s:\n%s\n%!" (string_of_loc location) msg;
  failwith "Aborted"

let error lexbuf msg =
  error_at (loc lexbuf) msg

let read_file lexbuf fname =
  try
    let ic = open_in fname in
    let len = in_channel_length ic in
    let s = String.create len in
    really_input ic s 0 len;
    s
  with e ->
    error lexbuf
      (sprintf "Cannot include file %s: %s" fname (Printexc.to_string e))
}

let blank = [' ' '\t']
let space = [' ' '\t' '\r' '\n']

(*
  In a document, any sequence of 3 or more single quotes
  can be escaped by adding one more quote.
*)
rule document = parse
  | [^'\'' '\n']+ as s
                  { Js s :: document lexbuf }
  | '\n'          { newline lexbuf;
                    Js "\n" :: document lexbuf }
  | "'''"         { let x = heredoc (loc lexbuf) lexbuf in
                    Heredoc x :: document lexbuf }
  | "'" ("''" ['\'']+ as s)
                  { Js s :: document lexbuf }
  | _ as c        { Js (String.make 1 c) :: document lexbuf }
  | eof           { [] }

(*
  In a heredoc string, any sequence of 3 or more single quotes
  can be escaped by adding one more quote;
  other special characters are escaped using a traditional backslash.
*)
and heredoc start_loc = parse
  | [^'\'' '$' '\\' '\n']+ as s
                  { Text s :: heredoc start_loc lexbuf }
  | "'''"         { [] }
  | "'" ("''" ['\'']+ as s)
                  { Text s :: heredoc start_loc lexbuf }
  | "${" ([^'\n' '}']* as insert) "}"
                  { Var insert :: heredoc start_loc lexbuf }
  | "${"          { error lexbuf "Missing closing curly brace for ${" }
  | "\\$"         { Text "$" :: heredoc start_loc lexbuf }
  | "\\\\"        { Text "\\" :: heredoc start_loc lexbuf }
  | '\n'          { newline lexbuf;
                    Text "\n" :: heredoc start_loc lexbuf }
  | "\\\n" blank* { newline lexbuf;
                    Ignored_newline :: heredoc start_loc lexbuf }
  | _ as c        { Text (String.make 1 c) :: heredoc start_loc lexbuf }
  | eof           { error_at start_loc "Missing closing '''" }

{
  open Printf

  let quote_javascript_string s =
    let collapsed_newlines = ref 0 in
    let buf = Buffer.create (2 * String.length s) in
    Buffer.add_char buf '"';
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | '"' -> Buffer.add_string buf "\\\""
        | '\n' ->
            Buffer.add_string buf "\\n";
            incr collapsed_newlines
        | '\\' -> Buffer.add_string buf "\\\\"
        | c -> Buffer.add_char buf c
    done;
    Buffer.add_char buf '"';
    Buffer.contents buf, !collapsed_newlines

  let error source msg =
    eprintf "Error in file %s: %s\n%!" source msg;
    exit 1

  let parse_source source ic =
    let lexbuf = Lexing.from_channel ic in
    init_fname lexbuf source;
    document lexbuf

  let emit_js source oc l =
    fprintf oc "/* Auto-generated from %s by herejs. Do not edit. */ " source;
    List.iter (function
      | Js s ->
          fprintf oc "%s" s;
      | Heredoc [] ->
          fprintf oc "\"\"";
      | Heredoc l ->
          fprintf oc "(";
          let l, collapsed_newlines =
            List.fold_left (fun (l, n) x ->
              match x with
              | Text s0 ->
                  let s, newlines = quote_javascript_string s0 in
                  (s :: l, n + newlines)
              | Var s ->
                  (s :: l, n)
              | Ignored_newline ->
                  (l, n + 1)
            ) ([], 0) l
          in
          fprintf oc "%s" (String.concat " + " (List.rev l));
          fprintf oc "%s" (String.make collapsed_newlines '\n');
          fprintf oc ")"
    ) l

  let run source ic oc =
    let doc = parse_source source ic in
    emit_js source oc doc

  let main () =
    let out_file = ref None in
    let in_file = ref None in
    let options = [
      "-o",
      Arg.String (
        fun s ->
          if !out_file <> None then
            failwith "Multiple output files"
          else
            out_file := Some s
      ),
      "<file>
          Output file (default: output goes to stdout)";
    ]
    in
    let anon_fun s =
      if !in_file <> None then
        failwith "Multiple input files"
      else
        in_file := Some s
    in

    let usage_msg = sprintf "\
Usage: %s [input file] [options]

Convert 'heredoc' triple-quoted strings into JavaScript functions.

  '''Hello ${x}!'''

becomes:

  'Hello ' + x + '!'

Command-line options:
"
      Sys.argv.(0)
    in

    Arg.parse options anon_fun usage_msg;

    let ic, source =
      match !in_file with
          None -> stdin, "<stdin>"
        | Some file -> open_in file, file
    in
    let oc =
      match !out_file with
          None -> stdout
        | Some file -> open_out file
    in
    run source ic oc

  let () = main ()
}
