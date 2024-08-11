;; Local installation of tree-sitter

(add-to-list 'load-path @EMACS_TREESIT_CORE@)
(add-to-list 'load-path @EMACS_TREESIT_LISP@)
(add-to-list 'load-path @EMACS_TREESIT_LANGS@)

(setq-default tsc-dyn-get-from '(:compilation))

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)

(add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode)
(global-tree-sitter-mode t)
