# Makefile for cl-bytes Elisp package

# Author: Dima Akater

ifndef SITELISP
export SITELISP = /usr/share/emacs/site-lisp
else
export SITELISP
endif

ifndef SITEETC
export SITEETC = /usr/share/emacs/etc
else
export SITEETC
endif

FAKEMAKEDEPS = org
FAKEMAKEDEPS:=$(FAKEMAKEDEPS:%=-L $(SITELISP)/%)

TESTDEPS = ort org-src-elisp-extras

BUILDDEPS = org
# Actually, not all of test dependencies are needed at build time
# and so this better be improved.
ifeq (test,$(findstring test, ${USE}))
	BUILDDEPS+=${TESTDEPS}
endif

BUILDDEPS:=$(BUILDDEPS:%=-L $(SITELISP)/%)

DEBUG = -f toggle-debug-on-error

EMACS_INIT     := emacs -Q --batch $(DEBUG) -L fakemake --load fakemake/init.el

EMACS_FAKEMAKE := $(EMACS_INIT) $(FAKEMAKEDEPS) --load fakemake.el

EVAL           := $(EMACS_FAKEMAKE) $(BUILDDEPS) -L . --eval

.PHONY: default test all install clean

default:
	$(EVAL) "(fakemake 'default)"

test:
	$(EVAL) "(fakemake 'test)"

all:
	$(EVAL) "(fakemake 'all)"

elpa-release: clean
	$(EVAL) "(requiring (lisp-mnt package) (fakemake 'elpa-release))"
	git add -Af
	git stash save
	git checkout elpa-release
	git checkout stash './*' ':!./.gitignore' ':!./fakemake' ':!./Makefile'
	git add -A
	git commit -m "Automatic ELPA release by fakemake"
	git stash drop

install:
	$(EMACS_INIT) --load install.el

clean:
	$(EMACS_FAKEMAKE) --eval "(fakemake 'clean)"
