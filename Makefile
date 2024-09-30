emacs/basic.elc: emacs/basic.el
emacs/kbind.elc: emacs/kbind.el
emacs/macros.elc: emacs/macros.el
emacs/mh-basic.elc: emacs/mh-basic.el
emacs/mh-kbind.elc: emacs/mh-kbind.el
emacs/mh-viper.elc: emacs/mh-viper.el
emacs/mh-ediff-merge.elc: emacs/mh-ediff-merge.el
emacs/mh-tm.elc: emacs/mh-tm.el
emacs/mh-lisp.elc: emacs/mh-lisp.el
emacs/mh-ed.elc: emacs/mh-ed.el
emacs/mh-ep.elc: emacs/mh-ep.el

EMACS?=emacs

EMACS_COMP:=emacs/basic.elc emacs/kbind.elc emacs/macros.elc \
            emacs/mh-basic.elc emacs/mh-kbind.elc emacs/mh-viper.elc \
            emacs/mh-ediff-merge.elc emacs/mh-tm.elc emacs/mh-lisp.elc \
            emacs/mh-ed.elc emacs/mh-ep.elc

####

$(EMACS_COMP):
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_COMP)

emacs: $(EMACS_COMP)
default: emacs

.PHONY: emacs clean
