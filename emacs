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

;; Style
(set-display-table-slot standard-display-table 'truncation ?…)
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector "↷"))

;; INPUT
;(set-input-method 'programmer-dvorak)

;; Auto made
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(completion-styles '(basic partial-completion emacs22 substring))
 '(diff-refine nil)
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(search-default-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(viper-emacs-state-cursor-color "#ab4642")
 '(viper-ex-style-editing nil)
 '(viper-ex-style-motion nil)
 '(viper-insert-state-cursor-color "#ab4642")
 '(viper-shift-width 2)
 '(viper-toggle-key "")
 '(viper-want-ctl-h-help t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight semi-bold :height 169 :width normal))))
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(icomplete-first-match ((t (:foreground "#dc9656"))))
 '(trailing-whitespace ((t (:background "#fb4934" :foreground "#fabd2f"))))
 '(viper-minibuffer-emacs ((t nil)))
 '(viper-minibuffer-insert ((t nil)))
 '(viper-minibuffer-vi ((t nil)))
 '(viper-replace-overlay ((t nil))))
