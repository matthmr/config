EMACS?=emacs

EMACS_SOURCES_ELC=emacs-modes/mh-emacsos.elc emacs-modes/mh-basic.elc \
                  emacs-modes/mh-cxm.elc emacs-modes/mh-mpc.elc \
                  emacs-basic.elc emacs-keybindings.elc emacs-macros.elc

emacs-modes/mh-emacsos.elc: emacs-modes/mh-emacsos.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-basic.elc: emacs-modes/mh-basic.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-cxm.elc: emacs-modes/mh-cxm.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-modes/mh-mpc.elc: emacs-modes/mh-mpc.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

emacs-basic.elc: emacs-basic
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-keybindings.elc: emacs-keybindings
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-macros.elc: emacs-macros
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_SOURCES_ELC)

emacs: $(EMACS_SOURCES_ELC)

.PHONY: emacs clean
