.PHONY : all test unit-test ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = 0.12.0

all: test tag

test:
	$(MAKE) unit-test
	$(MAKE) ecukes

unit-test:
	$(CASK) exec ert-runner

$(PKG_DIR):
	Cask
	$(CASK) install
	touch $@

ecukes:
	$(CASK) exec ecukes --reporter magnars --script $(FEATURES) --no-win

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags
