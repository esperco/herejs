function render(title, name) {
  var escaped = "3 single quotes: ''''";
  return '''<!doctype html>
<html>
<head>
  <title>${title}</title>
</head>
<body>
  <h1>${ title + ' ' + title.toUpperCase() }</h\
  1>
  <p>
    Hello ${name}!
  </p>
  <p>
    dollar open-curly a b c close-curly = \${abc}
  </p>
  <p>
    3 single quotes: ''''
  </p>
</body>
</html>
''';
}

function main() {
  console.log(render("Hello", "world"));
}

main();
