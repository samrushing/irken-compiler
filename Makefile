# -*- Mode: Makefile; tab-width: 4 -*-

all: safe

# build from bootstrap.
bootstrap: vm
	python3 util/bootstrap.py

vm: vm/irkvm

vm/irkvm: vm/irkvm.c include/header1.c include/irken.h
	python3 util/build_vm.py

test:
	python3 util/run_tests.py

# remove nearly everything
clean:
	python3 util/clean.py

# leave self/compile[0-9]? binaries.
semi:
	python3 util/clean.py -s

tags:
	find ./self ./lib -name "*.scm" | etags -

# build self/compile with binary rotation.
safe:
	python3 util/safe.py

vmself:
	self/compile self/compile.scm -b

