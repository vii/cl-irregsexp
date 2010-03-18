LISP_IMPLEMENTATIONS = clozure sbcl
LISP_TESTS = cl-irregsexp cl-ppcre
TESTS = perl.pl python.py ruby.rb pcre re2 $(foreach impl,$(LISP_IMPLEMENTATIONS),$(LISP_TESTS:=.$(impl)))

RESULTS = $(TESTS:=-result)

CXXFLAGS += -march=native -O10 -fwhole-program

SBCL ?= sbcl

all: benchmark.table

pcre: pcre.cc
	$(CXX) $(CXXFLAGS) `pcre++-config --libs` `pcre++-config --cflags` $< -o $@

re2: re2.cc
	$(CXX) $(CXXFLAGS) -L$(RE2PATH)/obj -I$(RE2PATH)/  $< -lre2 -lpthread -o $@


%-result: %
	./bench $@ ./$< 

%.sbcl-result: %.lisp
	./bench $@ $(SBCL) --noinform --noprint --load $< 

%.clisp-result: %.lisp
	./bench $@ clisp --silent -ansi -C -i $< 

%.clozure-result: %.lisp
	./bench $@ $(CLOZURE)  --load $< 

%.table: $(RESULTS)
	for i in $^; do \
		cat $$i ; \
		$(RM) $$i; \
	done > $@

clean:
	$(RM) pcre re2 *.table $(RESULTS)

.PHONY: all clean