EMACS?=emacs

EMACS_SOURCES_ELC=emacs/basic.elc emacs/kbind.elc emacs/macros.elc \
                  emacs/mh-basic.elc emacs/mh-kbind.elc emacs/mh-viper.elc

emacs/basic.elc: emacs/basic.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/kbind.elc: emacs/kbind.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/macros.elc: emacs/macros.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-basic.elc: emacs/mh-basic.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-kbind.elc: emacs/mh-kbind.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs/mh-viper.elc: emacs/mh-viper.el
	@echo "[ .. ] Compiling $?"
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

clean:
	rm -rfv $(EMACS_SOURCES_ELC)

emacs: $(EMACS_SOURCES_ELC)

.PHONY: emacs clean
