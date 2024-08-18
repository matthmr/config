(require 'indent-bars)

(setq-default
  indent-bars-color '(highlight :face-bg "brightblack")
  indent-bars-pattern "."
  indent-bars-starting-column 0
  indent-bars-zigzag nil
  indent-bars-color-by-depth nil
  indent-bars-highlight-current-depth nil)

(add-hook (if (fboundp 'tree-sitter-hl-mode)
              'tree-sitter-hl-mode-hook
            'prog-mode-hook)
  'indent-bars-mode)
