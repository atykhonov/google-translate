.PHONY : all test unit-test ecukes

EMACS ?= emacs
EASK ?= eask
FEATURES = $(wildcard features/*.feature)

all: test tag

ci: clean package install compile test

test:
	$(EASK) install-deps --dev
	$(MAKE) unit-test
	$(MAKE) ecukes

unit-test:
	$(EASK) ert ./test/*.el

ecukes:
	$(EASK) exec ecukes --reporter magnars --script $(FEATURES) --no-win

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint

clean:
	$(EASK) clean-all
