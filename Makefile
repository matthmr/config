EMACS?=emacs

emacs-basic.elc: emacs-basic
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-keybindings.elc: emacs-keybindings
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"
emacs-macros.elc: emacs-macros
	$(EMACS) --batch --exec "(byte-compile-file \"$?\")"

emacs: emacs-basic.elc emacs-keybindings.elc emacs-macros.elc
