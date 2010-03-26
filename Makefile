# -*- Mode: Makefile; tab-width: 4 -*-

clean:
	python util/clean.py

dist: clean
	python util/dist.py
