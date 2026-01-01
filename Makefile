EMACS ?= emacs
EASK ?= eask
# emacs version
EVERSION =
EMACSFLAGS = -Q -L .

EASK_DIR = ./.eask/$(shell $(EMACS) --batch --eval "(princ emacs-version)")
DEPS_FILE = .eask-deps

VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
PKG = parinfer-rust-mode
FILTER_FILES =  $(PKG)-autoloads.el test-helper.el run-tests.el generate-tests.el

ELS_ALL = $(wildcard *.el)
ELS = $(filter-out $(FILTER_FILES),$(ELS_ALL))
OBJECTS = $(ELS:.el=.elc)

OS = $(shell uname | tr '[:upper:]' '[:lower:]')


.PHONY: elpa build version test lint clean elpaclean run-$(PKG)

all: build

-include .depend

elpa-$(EMACS):
	$(EASK) install --all
	touch $@

elpa: elpa-$(EMACS)

build: version elpa
	$(EASK) compile

version:
	$(EMACS) --version

$(DEPS_FILE): Eask
	$(EASK) install-deps --dev
	@touch $@

install-dev: $(DEPS_FILE)

download:
ifeq (,$(wildcard $$(EASK_DIR)/parinfer-rust/parinfer-rust-$(OS).so))
	mkdir -p $(HOME)/.emacs.d/parinfer-rust
	curl -L "https://github.com/justinbarclay/parinfer-rust-emacs/releases/download/v0.4.7/parinfer-rust-$(OS).so" --create-dirs -o "$(EASK_DIR)/parinfer-rust/parinfer-rust-$(OS).so"
endif

test: clean elpa version download build install-dev
	$(EASK) test ert-runner test/**.el 2> /dev/null

lint: version elpa
	$(EASK) exec $(EMACS) -Q --batch \
		--eval "(setq enable-local-variables :safe)" \
		-l elisp-lint.el -f elisp-lint-files-batch \
		$(ELS)

test-all: lint test

clean:
	rm -f .depend $(OBJECTS) $(PKG)-autoloads.el

elpaclean: clean
	rm -f elpa*
	rm -rf .eask # Clean packages installed for development
