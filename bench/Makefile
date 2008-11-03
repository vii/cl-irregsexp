LISP_IMPLEMENTATIONS = closure sbcl
LISP_TESTS = cl-irregsexp cl-ppcre
TESTS = perl.pl python.py ruby.rb pcre $(foreach impl,$(LISP_IMPLEMENTATIONS),$(LISP_TESTS:=.$(impl)))

RESULTS = $(TESTS:=-result)

pcre: pcre.cc
	$(CXX) -march=native -O3 `pcre++-config --libs` `pcre++-config --cflags` $< -o $@

%-result: %
	./$< > $@

%.sbcl-result: %.lisp
	sbcl --noinform --noprint --load $< > $@

%.clisp-result: %.lisp
	clisp --silent -ansi -C -i $< > $@

%.closure-result: %.lisp
	$(CLOSURE)  --load $< > $@

all: benchmark.table

%.table: $(RESULTS)
	for i in $^; do \
		echo $$(basename $$i -result) `cat $$i` ; \
		$(RM) $$i; \
	done > $@

clean:
	$(RM) pcre *.table $(RESULTS)

.PHONY: all clean