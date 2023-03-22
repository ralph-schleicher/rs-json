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

.PHONY: all
all: citm_catalog

.PHONY: citm_catalog
citm_catalog: citm_catalog.stamp

%.stamp: bench.m %.csv
	octave --eval "rehash; bench('$*')"
	echo timestamp > $@

.PRECIOUS: %.csv
%.csv: bench.pl %.sbcl.log %.ccl.log
	perl ./bench.pl $* > $@

.PRECIOUS: %.sbcl.log
%.sbcl.log: bench.lisp
	run-sbcl --load bench.lisp --eval '(rs-json-bench:$*)' --quit | tee $@

.PRECIOUS: %.ccl.log
%.ccl.log: bench.lisp
	run-ccl --batch --load bench.lisp --eval '(rs-json-bench:$*)' < /dev/null | tee $@

.PRECIOUS: %.clisp.log
%.clisp.log: bench.lisp
	run-clisp -x '(load "bench.lisp")' -x '(rs-json-bench:$*)' | tee $@

.PHONY: large
large: large.json
	for lisp in sbcl ccl ; do \
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

large.json: $(large_json_SOURCES)
	{ echo "[" ; \
	  for j in 1 2 3 4 5 6 7 ; do \
	    for f in $(large_json_SOURCES) ; do \
	      cat $$f ; echo "," ; \
	    done ; \
	  done ;  \
	  echo "null" ; \
	  echo "]" ; \
	} > $@

.PHONY: clean
clean:
	rm -f citm_catalog.* large.*

## GNUmakefile ends here
