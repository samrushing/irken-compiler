# -*- Mode: Makefile; tab-width: 4 -*-

bootstrap:
	python util/bootstrap.py

test:
	python util/run_tests.py

clean:
	python util/clean.py

dist:
	python util/dist.py
