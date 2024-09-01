(require 'indent-bars)

(defface mh/indent-bars-face '((t :foreground "color-19"))
  "Face for `indent-bars's bars")

;;;;

(indent-bars-set-face mh/indent-bars-face)

(add-hook 'tree-sitter-hl-mode-hook 'indent-bars-mode)
