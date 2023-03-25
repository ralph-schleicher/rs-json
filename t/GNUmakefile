## GNUmakefile --- make file for the RS-JSON library

# Copyright (C) 2023 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

SBCL = run-sbcl
CCL = run-ccl
CLISP = run-clisp

# Lisp systems under test.  CLISP is out of scope – it's
# simply too slow.
lisps = \
sbcl \
ccl \
$(nil)

# JSON libraries under test.
libraries = \
cl-json \
jonathan \
json-streams \
jsown \
jzon \
rs-json \
shasht \
st-json \
yason \
$(nil)

.PHONY: all
all:
	@echo ""
	@echo "Available Tests:"
	@echo ""
	@echo "     citm_catalog   Read/write ‘citm_catalog.json’ (1.6 MiB)."
	@echo "                    Creates files ‘citm_catalog.*’."
	@echo "     large          Read/write ‘large.json’ (100 MiB)."
	@echo "                    Creates files ‘large.*’."
	@echo "     test-suite     Run the JSON Test Suite, parsing only."
	@echo "                    Results in ‘json-test-suite/results/’."
	@echo ""

### Performance Tests

%.stamp: bench.m %.csv
	octave --eval "rehash; bench('$*')"
	echo timestamp > $@

.PRECIOUS: %.csv
%.csv: bench.pl %.sbcl.log %.ccl.log
	perl ./bench.pl $* > $@

.PRECIOUS: %.sbcl.log
%.sbcl.log: bench.lisp
	$(SBCL) --eval '(ql:quickload "rs-json/bench")' --eval '(rs-json-bench:$*)' --quit | tee $@

.PRECIOUS: %.ccl.log
%.ccl.log: bench.lisp
	$(CCL) --batch --eval '(ql:quickload "rs-json/bench")' --eval '(rs-json-bench:$*)' < /dev/null | tee $@

.PRECIOUS: %.clisp.log
%.clisp.log: bench.lisp
	$(CLISP) -x '(ql:quickload "rs-json/bench")' -x '(rs-json-bench:$*)' | tee $@

.PHONY: citm_catalog
citm_catalog: citm_catalog.stamp

.PHONY: citm_catalog-clean
citm_catalog-clean:
	rm -f citm_catalog.*

.PHONY: large
large: large.json
	for lisp in $(lisps) ; do \
	  rm -f large.$$lisp.log- ; \
	  for run in 1 2 3 ; do \
	    rm -f large.$$lisp.log ; \
	    $(MAKE) large.$$lisp.log ; \
	    cat large.$$lisp.log >> large.$$lisp.log- ; \
	  done ; \
	  mv -f large.$$lisp.log- large.$$lisp.log ; \
	done ; \
	$(MAKE) large.stamp

large_json_SOURCES = \
data/apache_builds.json \
data/canada.json \
data/citm_catalog.json \
data/github_events.json \
data/gsoc-2018.json \
data/instruments.json \
data/marine_ik.json \
data/mesh.json \
data/mesh.pretty.json \
data/numbers.json \
data/random.json \
data/twitter.json \
data/twitterescaped.json \
data/update-center.json \
$(nil)

$(large_json_SOURCES): data.stamp
data.stamp:
	$(MAKE) -C data sync
	echo timestamp > $@

# Running ‘time json-test-suite/parser rs-json large.json’ works, too.
large.json: data.stamp
	{ echo "[" ; \
	  for j in 1 2 3 4 5 6 7 ; do \
	    for f in $(large_json_SOURCES) ; do \
	      cat $$f ; echo "," ; \
	    done ; \
	  done ;  \
	  echo "null" ; \
	  echo "]" ; \
	} > $@

.PHONY: large-clean
large-clean:
	rm -f large.*

### JSON Test Suite

.PHONY: test-suite
test-suite: json-test-suite.html
json-test-suite.html: json-test-suite/results/parsing.html json-test-suite.css
	{ \
	  echo '<style type="text/css">' ; \
	  cat json-test-suite.css ; \
	  echo '</style>' ; \
	} > json-test-suite.tem
	sed -e '/href="style.css"/ {' \
	    -e 'r json-test-suite.tem' \
	    -e 'd' \
	    -e '}' \
	json-test-suite/results/parsing.html > $@
	rm -f json-test-suite.css json-test-suite.tem

# Add a final newline character to the compressed style sheet.
json-test-suite.css: json-test-suite/results/style.css
	lessc --clean-css='--s1 -b' $< $@
	echo >> $@

# See https://wkhtmltopdf.org/.
json-test-suite.pdf: json-test-suite.html
	wkhtmltopdf --page-width 210mm --page-height 2970mm --title 'JSON Test Suite' $< $@

json-test-suite.jpg: json-test-suite.pdf
	convert json-test-suite.pdf -crop 516x1908+0+256 json-test-suite.jpg

json-test-suite/results/parsing.html: json-test-suite/results/logs.txt
	cd json-test-suite && python3 ./run_tests.py --no-run-tests

json-test-suite/results/logs.txt: json-test-suite/parser
	cd json-test-suite && python3 ./run_tests.py

.PHONY: test-suite-exec
test-suite-exec: json-test-suite/parser
# To build the test program with Clozure CL, say
# ‘make test-suite-clean test-suite-exec SBCL=true’.
json-test-suite/parser: json-test-suite.asd json-test-suite.lisp bench.lisp
	rm -f $@
	if test ! -f $@ ; then \
	  $(SBCL) --noinform \
	  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"$$PWD\") :inherit-configuration))" \
	  --eval "(asdf:make \"json-test-suite/parser\")" \
	  --quit ; \
	fi
	if test ! -f $@ ; then \
	  $(CCL) --quiet --batch \
	  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"$$PWD\") :inherit-configuration))" \
	  --eval "(asdf:make \"json-test-suite/parser\")" ; \
	fi
	test -f $@

.PHONY: test-suite-clean
test-suite-clean:
	rm -f json-test-suite.html json-test-suite.css json-test-suite.pdf json-test-suite.jpg json-test-suite/parser

### Miscellaneous

.PHONY: clean
clean: citm_catalog-clean large-clean test-suite-clean

# Dependencies.
bench.lisp: data.stamp

## GNUmakefile ends here
