.PHONY : all test unit-test ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)

all: test

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
