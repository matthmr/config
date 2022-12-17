;; Preamble
(load "~/.emacs.d/emacs-basic")

; color theme

(setq max-mini-window-height 1)

; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; THEMES
(setq base16-theme-distinct-fringe-background nil)
(load-theme 'base16-gruvbox-dark-hard t)

;; INPUT
;(set-input-method 'programmer-dvorak)
