## GNUmakefile --- JSON test files

.PHONY: sync
sync:
	svn export --force https://github.com/boostorg/json/trunk/bench/data .
	chmod 644 *.json
	svn status

## GNUmakefile ends here
