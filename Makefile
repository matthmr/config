EMACS?=emacs

EMACS_SOURCES_ELC=emacs/mh-emacsos.elc emacs/mh-basic.elc \
                  emacs/mh-cxm.elc emacs/mh-mpc.elc \
                  emacs/basic.elc emacs/kbind.elc emacs/macros.elc

emacs/mh-emacsos.elc: emacs/mh-emacsos.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-basic.elc: emacs/mh-basic.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-cxm.elc: emacs/mh-cxm.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-mpc.elc: emacs/mh-mpc.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

emacs/basic.elc: emacs/basic.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/kbind.elc: emacs/kbind.ekl
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/macros.elc: emacs/macros.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_SOURCES_ELC)

emacs: $(EMACS_SOURCES_ELC)

.PHONY: emacs clean
