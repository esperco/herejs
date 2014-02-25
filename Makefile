herejs: herejs.mll
	ocamllex $<
	ocamlopt -o $@ herejs.ml

.PHONY: demo
demo:
	./herejs example.js -o example.out
	js example.out

ifndef PREFIX
  PREFIX = $(HOME)
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif

.PHONY: install
install:
	cp herejs $(BINDIR)

.PHONY: clean
clean:
	rm -f *.o *.cm* *~ herejs.ml herejs
	rm -f example.out
