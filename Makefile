all:

include Makefunc.mk

TOP         := $(dir $(lastword $(MAKEFILE_LIST)))
EMACS_RAW   := $(filter-out emacs-undumped, $(shell compgen -c emacs- | xargs))
AVAILABLE   := $(strip $(sort $(EMACS_RAW)))
ALL_EMACS   := $(filter $(AVAILABLE),emacs-24.5 emacs-25.3 emacs-26.1)

EMACS       ?= emacs
BATCH       := $(EMACS) -Q --batch -L $(TOP)

SIMPLEHTTPD := simple-httpd.el
DEPEND      := $(SIMPLEHTTPD)

TESTFILE    := seml-mode-tests.el
ELS         := seml-mode.el

CORTELS     := $(TESTFILE) $(DEPEND) cort-test.el
CORT_ARGS   := -l $(TESTFILE) -f cort-run-tests

LOGFILE     := .make-check.log
MAKE-NPD    := $(MAKE) --no-print-directory

##################################################

all: git-hook $(ELS:.el=.elc)

git-hook:
	cp -a git-hooks/* .git/hooks/

include Makefile-check.mk

##############################
#  depend files

_GITHUB := https://raw.githubusercontent.com
$(SIMPLEHTTPD):
	curl -O $(_GITHUB)/skeeto/emacs-web-server/master/$@

##############################
#  test on all Emacs

allcheck: $(ALL_EMACS:%=.make-check-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-check-%: $(DEPEND)
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cp -f Makefile-check.mk .make-$*/Makefile
	$(MAKE) -C .make-$* clean
	$(call EXPORT,ELS CORT_ARGS DEPEND) \
	  EMACS=$* $(MAKE-NPD) -C .make-$* check 2>&1 | tee -a $(LOGFILE)
	rm -rf .make-$*

##############################
#  silent `allcheck' job

test: $(ALL_EMACS:%=.make-test-%)
	@echo ""
	@cat $(LOGFILE) | grep =====
	@rm $(LOGFILE)

.make-test-%: $(DEPEND)
	mkdir -p .make-$*
	cp -f $(ELS) $(CORTELS) .make-$*/
	cp -f Makefile-check.mk .make-$*/Makefile
	$(MAKE-NPD) -C .make-$* clean
	$(call EXPORT,ELS CORT_ARGS DEPEND) \
	  EMACS=$* $(MAKE-NPD) -C .make-$* check 2>&1 >> $(LOGFILE)
	rm -rf .make-$*
