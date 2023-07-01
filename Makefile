EMACS?=emacs

EMACS_SOURCES_ELC=emacs-modes/mh-emacsos.elc emacs-modes/mh-basic.elc \
                  emacs-modes/mh-cxm.elc emacs-modes/mh-mpc.elc \
                  emacs-basic.elc emacs-keybindings.elc emacs-macros.elc

emacs-modes/mh-emacsos.elc: emacs-modes/mh-emacsos.el
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-basic.elc: emacs-modes/mh-basic.el
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-cxm.elc: emacs-modes/mh-cxm.el
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-mpc.elc: emacs-modes/mh-mpc.el
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

emacs-basic.elc: emacs-basic
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-keybindings.elc: emacs-keybindings
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-macros.elc: emacs-macros
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_SOURCES_ELC)

emacs: $(EMACS_SOURCES_ELC)

.PHONY: emacs clean
