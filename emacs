;; Preamble
(load "/home/mh/.emacs.d/basic")

; color theme
(add-to-list 'custom-theme-load-path
             "/home/mh/.emacs.d/themes") ;; gruber-darker,vscode-dark-plus
(add-to-list 'custom-theme-load-path
             "/home/mh/.emacs.d/themes/base16-theme/build") ;; base16
; git-managed packages
(add-to-list 'load-path
             "/home/mh/Git/EMACS/markdown-mode") ;; markdown-mode
(add-to-list 'load-path
             "/home/mh/Git/EMACS/evil-mode") ;; evil-mode
(add-to-list 'load-path
             "/home/mh/.emacs.d/themes") ;; base16

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
