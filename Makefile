.PHONY : all test unit-test ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
AUTOLOADS = google-translate-autoloads.el
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = 0.12.0

%.elc: %.el autoloads
	$(EMACS) -Q -batch -L . --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	-f package-initialize -f batch-byte-compile $<

all: cask $(ELCS) autoloads

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"phpactor\" default-directory))"

cask: .cask/installed

.cask/installed: Cask
	$(CASK) install
	@touch .cask/installed

test:
	$(MAKE) unit-test
	$(MAKE) ecukes

unit-test: .cask $(ELCS)
	$(CASK) exec ert-runner

$(PKG_DIR):
	touch $@

ecukes:
	$(CASK) exec ecukes --reporter magnars --script $(FEATURES) --no-win

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags

clean:
	rm -f $(ELC) $(AUTOLOADS)

.PHONY: all autoloads cask clean elc ecukes tag test unit-test version
