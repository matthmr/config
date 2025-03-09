emacs/basic.elc: emacs/basic.el
emacs/kbind.elc: emacs/kbind.el
emacs/mh-basic.elc: emacs/mh-basic.el
emacs/mh-kbind.elc: emacs/mh-kbind.el
emacs/mh-ediff-merge.elc: emacs/mh-ediff-merge.el
emacs/mh-tm.elc: emacs/mh-tm.el
emacs/mh-lisp.elc: emacs/mh-lisp.el
emacs/mh-ed.elc: emacs/mh-ed.el
emacs/mh-ep.elc: emacs/mh-ep.el
emacs/mh-over.elc: emacs/mh-over.el
emacs/mh-kver.elc: emacs/mh-kver.el

EMACS?=emacs

EMACS_COMP:=emacs/basic.elc emacs/kbind.elc emacs/mh-basic.elc\
            emacs/mh-kbind.elc emacs/mh-ediff-merge.elc emacs/mh-tm.elc \
            emacs/mh-lisp.elc emacs/mh-ed.elc emacs/mh-ep.elc \
            emacs/mh-over.elc emacs/mh-kver.elc

####

$(EMACS_COMP):
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_COMP)

emacs: $(EMACS_COMP)
default: emacs

.PHONY: emacs clean
