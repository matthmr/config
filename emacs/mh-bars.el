(require 'indent-bars)

(defface mh/indent-bars-face '((t :foreground "color-19"))
  "Face for `indent-bars's bars")

;;;;

(indent-bars-set-face mh/indent-bars-face)

(defun mh/bars (load)
  (if load
    (progn
      (add-hook 'tree-sitter-hl-mode-hook 'indent-bars-mode)
      (indent-bars-mode 1))
    (progn
      (remove-hook 'tree-sitter-hl-mode-hook 'indent-bars-mode))
      (let ((mod (buffer-modified-p)))
        (indent-bars-mode -1)
        (unless mod
          (not-modified)))
      ))

(mh/provide 'bars #'mh/bars)
