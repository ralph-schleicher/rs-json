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

.PRECIOUS: citm_catalog.csv
%.csv: bench.pl %.sbcl.log %.ccl.log
	perl ./bench.pl $* > $@

.PRECIOUS: citm_catalog.sbcl.log
%.sbcl.log: bench.lisp
	run-sbcl \
	--load bench.lisp \
	--eval '(rs-json-bench:$*)' \
	--quit | \
	tee $@

.PRECIOUS: citm_catalog.ccl.log
%.ccl.log: bench.lisp
	run-ccl \
	--batch \
	--load bench.lisp \
	--eval '(rs-json-bench:$*)' < /dev/null | \
	tee $@

## GNUmakefile ends here
