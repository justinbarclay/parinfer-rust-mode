export EMACS ?= emacs
EMACSFLAGS = -L .
CASK = cask
VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PKG = parinfer-rust-mode

ELS_ALL = $(wildcard *.el)
ELS = $(filter-out $(PKG)-autoloads.el,$(ELS_ALL))
OBJECTS = $(ELS:.el=.elc)

.PHONY: elpa build version test lint clean elpaclean run-$(PKG)

all: build

-include .depend

elpa-$(EMACS):
	$(CASK) install
	$(CASK) update
	touch $@

elpa: elpa-$(EMACS)

build: version elpa
	$(CASK) build

version:
	$(EMACS) --version

test: version build
	$(CASK) exec ert-runner test/**.el --quiet

lint: version elpa
	$(CASK) exec $(EMACS) -Q --batch \
		--eval "(setq enable-local-variables :safe)" \
		-l elisp-lint.el -f elisp-lint-files-batch \
		--no-package-format \
                --no-fill-column \
		$(ELS)

test-all: lint test

clean:
	rm -f .depend $(OBJECTS) $(PKG)-autoloads.el

elpaclean: clean
	rm -f elpa*
	rm -rf .cask # Clean packages installed for development
