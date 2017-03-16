# -*- Mode: Makefile; tab-width: 4 -*-

bootstrap:
	python util/bootstrap.py

test:
	python util/run_tests.py

clean:
	python util/clean.py

semi:
	python util/clean.py -s

dist:
	python util/dist.py

tags:
	find ./self ./lib -name "*.scm" | etags -

safe:
	python util/safe.py
