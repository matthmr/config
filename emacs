;; Preamble
(load "/home/p/config/emacs-basic")

; color theme
(add-to-list 'custom-theme-load-path
             "/home/mh/.emacs.d/themes") ;; gruber-darker,vscode-dark-plus
(add-to-list 'custom-theme-load-path
             "/home/mh/.emacs.d/themes/base16-theme/build") ;; base16
; git-managed packages
(add-to-list 'load-path
             "/home/mh/Git/EMACS/PKG") ;; markdown-mode
(add-to-list 'load-path
             "/home/mh/.emacs.d/themes") ;; base16
(add-to-list 'load-path
             "/mnt/ssd/root/usr/share/emacs/site-lisp") ;; local

(require 'eglot)

(setq max-mini-window-height 1)
(setq eglot-stay-out-of '("flymake" "imenu"))

; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(add-hook 'emacs-startup-hook (lambda ()
                               (when (get-buffer "*scratch*")
                                (kill-buffer "*scratch*"))))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; THEMES
(load-theme 'base16-default-dark t)
;;; chosen in the past:
;; - default-dark

;; Style
(set-display-table-slot standard-display-table 'truncation ?…)
(set-display-table-slot standard-display-table 'wrap ?↩)
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector "↷"))

;; INPUT
;(set-input-method 'programmer-dvorak)

;; OwO
(require 'zen-mode)

;; Auto made
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "/tmp/emacs/" t)))
 '(auto-save-interval 500)
 '(auto-save-no-message t)
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "/mnt/hdd/backup/EMACS/")))
 '(base16-theme-256-color-source 'colors)
 '(base16-theme-distinct-fringe-background t)
 '(column-number-mode t)
 '(completion-styles '(basic partial-completion emacs22 substring))
 '(delete-auto-save-files nil)
 '(delete-old-versions t)
 '(diff-refine nil)
 '(eglot-autoshutdown t)
 '(eglot-events-buffer-size 100000)
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(eglot-menu-string "")
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(inhibit-startup-screen t)
 '(kept-new-versions 1)
 '(kept-old-versions 1)
 '(menu-bar-mode nil)
 '(mode-line-compact nil)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-misc-info mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-end-spaces))
 '(read-file-name-completion-ignore-case t)
 '(scroll-bar-mode nil)
 '(search-default-mode t)
 '(set-mark-command-repeat-pop t)
 '(size-indication-mode t)
 '(smerge-command-prefix "C-c m")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(viper-case-fold-search t)
 '(viper-emacs-state-cursor-color "#ab4642")
 '(viper-ex-style-editing nil)
 '(viper-ex-style-motion nil)
 '(viper-insert-state-cursor-color "#ab4642")
 '(viper-shift-width 2)
 '(viper-toggle-key "")
 '(viper-use-replace-region-delimiters nil)
 '(viper-want-ctl-h-help t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight medium :height 169 :width normal))))
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(eglot-mode-line ((t nil)))
 '(icomplete-first-match ((t (:foreground "#dc9656"))))
 '(trailing-whitespace ((t (:background "#fb4934" :foreground "#fabd2f"))))
 '(viper-minibuffer-emacs ((t nil)))
 '(viper-minibuffer-insert ((t nil)))
 '(viper-minibuffer-vi ((t nil)))
 '(viper-replace-overlay ((t nil))))
