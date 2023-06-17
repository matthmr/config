;;;; Preamble

(load "/home/p/config/emacs-basic")

;;; color themes
(add-to-list 'custom-theme-load-path
             "/home/mh/Emacs/themes")
(add-to-list 'custom-theme-load-path
             "/home/mh/Emacs/themes/base16-themes")

;;; load-paths
(add-to-list 'load-path
             "/home/mh/Emacs/lisp")

;;; load some programs
(require 'eglot)
(require 'zen-mode)
(require 'caps)
(require 'rainbow-delimiters)
(require 'edwina)

(load "/home/p/config/emacs-modes/mh-viper")
(load "/home/p/config/emacs-modes/mh-mpc")

;; (setq max-mini-window-height 1)
(setq eglot-stay-out-of '("flymake"))

;;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; TTY vs PTS

(let ((env/wm (getenv "WM")))
  (if (or (not env/wm) (string= env/wm "emacs-tty")
                       (string= env/wm "tty"))
      ;; display time, and use visual line
      (progn
        (display-time-mode t)
        (global-visual-line-mode t))
    ;; some character which TTYs can't display properly, xterm-mouse-mode, themes
    (progn
      (set-display-table-slot standard-display-table 'truncation ?…)
      (set-display-table-slot standard-display-table 'wrap ?↩)
      (set-display-table-slot standard-display-table 'selective-display
                              (string-to-vector "↷"))
      (load-theme 'base16-default-dark t)
      (xterm-mouse-mode t))))

;;; Auto made
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "/tmp/emacs/" t)))
 '(auto-save-interval 300)
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "/mnt/hdd/backup/EMACS/")))
 '(base16-theme-256-color-source 'colors)
 '(base16-theme-distinct-fringe-background nil)
 '(column-number-mode t)
 '(completion-styles '(basic partial-completion emacs22 substring))
 '(delete-auto-save-files nil)
 '(delete-old-versions t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop-lock")
 '(desktop-load-locked-desktop t)
 '(desktop-path '("~/Emacs/desktop"))
 '(diff-refine nil)
 '(display-line-numbers-widen t)
 '(display-time-default-load-average nil)
 '(display-time-format "%Y%m%d%H%M")
 '(eglot-autoshutdown t)
 '(eglot-events-buffer-size 100000)
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(eglot-menu-string "")
 '(eww-search-prefix "https://google.com/search?q=")
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-alternate-dictionary "/home/mh/Documents/Dict/english-words")
 '(kept-new-versions 1)
 '(kept-old-versions 1)
 '(menu-bar-mode nil)
 '(mode-line-compact nil)
 '(mode-line-format
   '("%e" " " mode-line-misc-info mode-line-mule-info mode-line-client mode-line-modified mode-line-remote " " mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes))
 '(org-agenda-files nil)
 '(outline-minor-mode-prefix "")
 '(package-selected-packages '(with-editor compat))
 '(read-file-name-completion-ignore-case t)
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(size-indication-mode t)
 '(smerge-command-prefix "m")
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(tsc-dyn-get-from nil)
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
 '(viper-want-ctl-h-help nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight medium :height 169 :width normal))))
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(eglot-mode-line ((t nil)))
 '(icomplete-first-match ((t (:foreground "#dc9656"))))
 '(log-view-message ((t (:extend t :background "grey85" :foreground "black"))))
 '(smerge-base ((t (:extend t :background "#ffffaa" :foreground "black"))))
 '(smerge-lower ((t (:extend t :background "#ddffdd" :foreground "black"))))
 '(smerge-markers ((t (:extend t :background "grey85" :foreground "black"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#aaffaa" :foreground "black"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#ffbbbb" :foreground "black"))))
 '(smerge-upper ((t (:extend t :background "#ffdddd" :foreground "black"))))
 '(trailing-whitespace ((t (:background "#fb4934" :foreground "#fabd2f"))))
 '(viper-minibuffer-emacs ((t nil)))
 '(viper-minibuffer-insert ((t nil)))
 '(viper-minibuffer-vi ((t nil)))
 '(viper-replace-overlay ((t nil))))
