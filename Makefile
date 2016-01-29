default: all

all: elm.js

ELM_FILES = $(shell find . -path ./elm-stuff -prune -o -type f -name '*.elm')

SHELL := /bin/bash

elm.js: $(ELM_FILES)
	elm-make --warn --yes Main.elm --output elm.js

clean-deps:
	rm -rf elm-stuff
	rm -f elm-io.sh

elm-watch:
	find . -name *.elm -type f | grep -v elm-stuff | entr make

clean:
	rm -f *.js
	rm -rf elm-stuff/build-artifacts
	rm -f Test/*.js

deps:
	elm-package install --yes

repl:
	elm-repl
