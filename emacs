;;;; Dot Emacs

(load "/home/p/config/emacs-basic")

;;; Color Themes
(add-to-list 'custom-theme-load-path
             "/home/mh/Emacs/themes")
(add-to-list 'custom-theme-load-path
             "/home/mh/Emacs/themes/base16-themes")

;;; Load Paths

(add-to-list 'load-path "/home/mh/Emacs/lisp")
(add-to-list 'load-path "/home/mh/Git/EMACS/multiple-cursors.el")
(add-to-list 'load-path "/home/mh/Git/EMACS/company-mode")

;;; Load Some Programs
(require 'eglot)
(require 'zen-mode)
(require 'caps)
(require 'rainbow-delimiters)
(require 'edwina)
(require 'multiple-cursors)
(require 'company)
(require 'move-text)

(load "/home/p/config/emacs-modes/mh-emacsos")
(load "/home/p/config/emacs-modes/mh-basic")
(load "/home/p/config/emacs-modes/mh-mpc")
(load "/home/p/config/emacs-modes/mh-cxm")

;; (setq max-mini-window-height 1)
(setq eglot-stay-out-of '("flymake"))

;;; Multiple Cursors

(global-set-key (kbd "C-x C-M-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x C-M-e") 'mc/edit-lines)
(global-set-key (kbd "C-x C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x C-M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x M-n")   'mc/skip-to-next-like-this)
(global-set-key (kbd "C-x M-p")   'mc/skip-to-previous-like-this)

;;; Move Text

(global-set-key (kbd "M-P")       'move-text-up)
(global-set-key (kbd "M-N")       'move-text-down)

;;; Company

(global-set-key (kbd "M-/")       'company-complete)

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; Abbrev

(read-abbrev-file "/home/mh/Emacs/abbrevs")

;;; TTY vs PTS

(let ((env/wm (getenv "WM")))
  (if (or (not env/wm) (string= env/wm "emacs-tty")
                       (string= env/wm "tmux-tty")
                       (string= env/wm "tty"))
      ;; display time, and use visual line
      (setq-default truncate-lines nil)
    ;; some character which TTYs can't display properly, xterm-mouse-mode,
    ;; themes
    (progn
      (set-display-table-slot standard-display-table 'truncation ?…)
      (set-display-table-slot standard-display-table 'wrap ?↩)
      (set-display-table-slot standard-display-table 'selective-display
                              (string-to-vector "↷"))
      (setq-default truncate-lines t)
      (xterm-mouse-mode t)
      (global-hl-line-mode)
      (load-theme 'base16-default-dark t))))

;;; Customize
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
 '(company-backends
   '(company-bbdb company-semantic company-cmake company-dabbrev-code company-dabbrev company-capf company-clang company-files
                  (company-gtags company-etags company-keywords)
                  company-oddmuse))
 '(company-idle-delay nil)
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
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(ispell-alternate-dictionary "/home/mh/Documents/Dict/english-words")
 '(kept-new-versions 1)
 '(kept-old-versions 1)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(mode-line-compact nil)
 '(mode-line-format
   '("%e" " " mode-line-misc-info mode-line-mule-info mode-line-client mode-line-modified mode-line-remote " " mode-line-buffer-identification "   " mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes))
 '(org-agenda-files nil)
 '(outline-minor-mode-prefix "")
 '(package-selected-packages '(with-editor compat))
 '(read-file-name-completion-ignore-case t)
 '(save-abbrevs nil)
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
 '(version-control t))
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
 '(trailing-whitespace ((t (:background "#fb4934" :foreground "#fabd2f")))))
