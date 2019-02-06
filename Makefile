.PHONY : all test unit-test ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = 0.11.17
TARGET_DIR = google-translate-$(VERSION)

all: test marmalade tag

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

marmalade: marmalade-tar marmalade-upload marmalade-rm

marmalade-tar:
	mkdir $(TARGET_DIR)
	cp google-translate-core-ui.el $(TARGET_DIR)
	cp google-translate-core.el $(TARGET_DIR)
	cp google-translate-default-ui.el $(TARGET_DIR)
	cp google-translate-query-auto-complete.el $(TARGET_DIR)
	cp google-translate-smooth-ui.el $(TARGET_DIR)
	cp google-translate.el $(TARGET_DIR)
	cp README.md $(TARGET_DIR)
	cp google-translate-pkg.el $(TARGET_DIR)
	tar -cf google-translate-$(VERSION).tar $(TARGET_DIR)

marmalade-upload:
	marmalade-upload -u atykhonov google-translate-$(VERSION).tar || true

marmalade-rm:
	rm google-translate-$(VERSION).tar
	rm -rf $(TARGET_DIR)

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags
