# -*- Mode: Makefile; tab-width: 4 -*-

all: safe

# build from bootstrap.
bootstrap: vm
	python util/bootstrap.py

vm: vm/irkvm

vm/irkvm: vm/irkvm.c
	$(CC) -O3 -fomit-frame-pointer -I./include vm/irkvm.c -o vm/irkvm -l ffi

test:
	python util/run_tests.py

# remove nearly everything
clean:
	python util/clean.py

# leave self/compile[0-9]? binaries.
semi:
	python util/clean.py -s

dist:
	python util/dist.py

tags:
	find ./self ./lib -name "*.scm" | etags -

# build self/compile with binary rotation.
safe:
	python util/safe.py

vmself:
	self/compile self/compile.scm -b

