## bench.pl --- measure performance of JSON libraries

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

use strict;

die ("Wrong number of arguments\n")
  if @ARGV != 1;

my $stem = $ARGV[0];

# Total run time.
my %run = ();
my %user = ();
my %sys = ();
# Bytes consed.
my %mem = ();

my $lisp;
my $lib;
my $job;

if (-f "$stem.sbcl.log")
  {
    open (STDIN, "<", "$stem.sbcl.log")
      || die ("$stem.sbcl.log: $!\n");

    $lisp = "SBCL";
    $lib = undef;
    $job = undef;

    while ($_ = <STDIN>)
      {
	s/\A\s+//;
	s/\s+\z//;

	if (m/\A([^;]+);(read|write);/i)
	  {
	    $lib = $1;
	    $job = $2;
	  }
	elsif (m/(\d+\.\d+) seconds of total run time \((\d+\.\d+) user, (\d+\.\d+) system/)
	  {
	    printf ("$lisp;$lib;$job;run;$1\n");
	    printf ("$lisp;$lib;$job;user;$2\n");
	    printf ("$lisp;$lib;$job;sys;$3\n");
	  }
	elsif (m/((?:\d+,)*\d+) bytes consed/)
	  {
	    my $tem = $1;
	    $tem =~ s/,//g;

	    printf ("$lisp;$lib;$job;mem;$tem\n");
	  }
	elsif (m/before it was aborted by a non-local transfer of control/)
	  {
	    printf ("$lisp;$lib;$job;err;1\n");
	  }
      }
  }

if (-f "$stem.ccl.log")
  {
    open (STDIN, "<", "$stem.ccl.log")
      || die ("$stem.ccl.log: $!\n");

    $lisp = "CCL";
    $lib = undef;
    $job = undef;

    while ($_ = <STDIN>)
      {
	s/\A\s+//;
	s/\s+\z//;

	if (m/\A([^;]+);(read|write);/i)
	  {
	    $lib = $1;
	    $job = $2;
	  }
	elsif (m/\((\d+\.\d+) seconds\) to run/)
	  {
	    printf ("$lisp;$lib;$job;run;$1\n");
	  }
	elsif (m/\((\d+\.\d+) seconds\) were spent in user mode/)
	  {
	    printf ("$lisp;$lib;$job;user;$1\n");
	  }
	elsif (m/\((\d+\.\d+) seconds\) were spent in system mode/)
	  {
	    printf ("$lisp;$lib;$job;sys;$1\n");
	  }
	elsif (m/((?:\d+,)*\d+) bytes of memory allocated/)
	  {
	    my $tem = $1;
	    $tem =~ s/,//g;

	    printf ("$lisp;$lib;$job;mem;$tem\n");
	  }
      }
  }

if (-f "$stem.clisp.log")
  {
    open (STDIN, "<", "$stem.clisp.log")
      || die ("$stem.clisp.log: $!\n");

    $lisp = "CLISP";
    $lib = undef;
    $job = undef;

    while ($_ = <STDIN>)
      {
	s/\A\s+//;
	s/\s+\z//;

	if (m/\A([^;]+);(read|write);/i)
	  {
	    $lib = $1;
	    $job = $2;
	  }
	elsif (m/Run time: (\d+\.\d+) sec/)
	  {
	    printf ("$lisp;$lib;$job;run;$1\n");
	  }
	elsif (m/Space: (\d+) Bytes/)
	  {
	    printf ("$lisp;$lib;$job;mem;$1\n");
	  }
      }
  }

## bench.pl ends here
