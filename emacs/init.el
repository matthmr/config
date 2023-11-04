;;;; Dot Emacs

(load @EMACS_BASIC@)

;;; Color Themes

(add-to-list 'custom-theme-load-path @EMACS_THEMES@)
(add-to-list 'custom-theme-load-path @EMACS_BASE16@)

;;; Load Paths

(add-to-list 'load-path @EMACS_LISP@)
(add-to-list 'load-path @EMACS_MULTIPLE_CURSORS@)
(add-to-list 'load-path @EMACS_COMPANY_MODE@)
(add-to-list 'load-path @EMACS_DOOM_MODELINE@)
(add-to-list 'load-path @EMACS_COMPAT@)

;;; Load Some Programs

(require 'zen-mode)
(require 'caps)
(require 'rainbow-delimiters)
(require 'multiple-cursors)
(require 'company)
(require 'move-text)
(require 'doom-modeline)
(require 'god-mode)

;; Load built-ins
(require 'scroll-lock)
(require 'facemenu)

(load @EMACS_MH_EMACSOS@)
(load @EMACS_MH_USER@) ; confidential options for the user, not in this repository
(load @EMACS_MH_BASIC@)
(load @EMACS_MH_MPC@)

;; (setq max-mini-window-height 1)
(setq eglot-stay-out-of '("flymake"))

;;; Custom loaders

(defun mh/load-cxm () (interactive) (load @EMACS_CXM@))
(defun mh/load-viper () (interactive) (load @EMACS_VIPER@))

;;; Multiple Cursors

(global-set-key (kbd "C-x C-M-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x C-M-e") 'mc/edit-lines)
(global-set-key (kbd "C-x C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x C-M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x M-n")   'mc/skip-to-next-like-this)
(global-set-key (kbd "C-x M-p")   'mc/skip-to-previous-like-this)

;;; Doom-Modeline

(add-hook 'after-init-hook #'doom-modeline-mode)

;;; Move Text

(global-set-key (kbd "C-M-p") 'move-text-up)
(global-set-key (kbd "C-M-n") 'move-text-down)

;;; Company

(global-set-key (kbd "C-x C-_") 'company-complete)

;;; Scroll-lock

(define-key scroll-lock-mode-map "\C-n" 'scroll-lock-next-line)
(define-key scroll-lock-mode-map "\C-p" 'scroll-lock-previous-line)
(define-key scroll-lock-mode-map "\M-{" 'scroll-lock-backward-paragraph)
(define-key scroll-lock-mode-map "\M-}" 'scroll-lock-forward-paragraph)

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; TTY vs PTS

(defvar mh/display
  `((truncation . ?…)
    (wrap . ?↩)
    (selective-display . ,(string-to-vector "↷"))
    (vertical-border . ?│))
  "Display table")

(let ((env/wm (getenv "WM")))
  (if (or (not env/wm) (string= env/wm "emacs-tty")
                       (string= env/wm "tmux-tty")
                       (string= env/wm "tty"))
      ;; display time, and use visual line
      (progn
        (setq-default Man-switches "-Tascii") ;; cannot render UTF-8
        (setq-default truncate-lines nil))
    ;; some character which TTYs can't display properly, xterm-mouse-mode,
    ;; themes
    (progn
      (set-display-table-slot standard-display-table 'truncation ?…)
      (set-display-table-slot standard-display-table 'wrap ?↩)
      (set-display-table-slot standard-display-table 'selective-display
                                                       (string-to-vector "↷"))
      (set-display-table-slot standard-display-table 'vertical-border ?│)

      (setq-default truncate-lines t)
      (mouse-wheel-mode t)
      (xterm-mouse-mode t)
      (global-hl-line-mode) ; min
      (global-whitespace-mode) ; min
      (load-theme 'base16-classic-dark t) ; min
      )))

(defvar mh/initial-scratch-message "\
;;                           ___
;;                          (.. |
;;                          (<> |          ,= ,-_-. =.
;;                         / __  \\        ((_/)o o(\\_))
;;                        ( /  \\ /|        `-!(. .)`-!
;;                       _/\\ __)/_)            \\_/
;;                       \\/-____\\/
;;
;;                       __             _
;;                      /__ |\\ | | |   |_ ._ _   _.  _  _
;;                      \\_| | \\| |_|   |_ | | | (_| (_ _>

"
  "Initial scratch message")

;;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width-max nil)
 '(auto-save-file-name-transforms '((".*" @EMACS_TMP@ t)))
 '(auto-save-interval 300)
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . @EMACS_BACKUP@)))
 '(base16-theme-256-color-source 'colors)
 '(base16-theme-distinct-fringe-background nil)
 '(browse-url-browser-display "ungoogled-chromium")
 '(browse-url-text-browser "w3m")
 '(column-number-mode t)
 '(comment-column 0)
 '(company-backends
   '(company-semantic company-capf company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-dabbrev))
 '(company-idle-delay 0.5)
 '(completion-styles '(basic partial-completion emacs22 substring))
 '(delete-active-region nil)
 '(delete-auto-save-files nil)
 '(delete-old-versions t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop-lock")
 '(desktop-load-locked-desktop t)
 '(desktop-path '(@EMACS_DESKTOP@))
 '(diff-refine nil)
 '(display-line-numbers-widen t)
 '(display-time-default-load-average nil)
 '(display-time-format "%Y%m%d%H%M")
 '(doom-modeline-buffer-file-name-style 'truncate-with-project)
 '(doom-modeline-icon nil)
 '(doom-modeline-minor-modes t)
 '(doom-modeline-mode t)
 '(doom-modeline-workspace-name nil)
 '(ediff-combination-pattern
   '("<<<<<<< ours" A "||||||| parent" Ancestor ">>>>>>> theirs" B "======= end"))
 '(ediff-keep-variants nil)
 '(eglot-autoshutdown t)
 '(eglot-events-buffer-size 100000)
 '(eglot-highlight-symbol-face ((t (:inherit underline))))
 '(eglot-menu-string "")
 '(eldoc-echo-area-use-multiline-p 1)
 '(eshell-buffer-maximum-lines 8000)
 '(eshell-directory-name @EMACS_ESHELL@)
 '(eww-search-prefix "https://google.com/search?q=")
 '(file-name-shadow-tty-properties '(invisible t intangible t field shadow))
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(global-company-mode t)
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(gud-key-prefix [3 1])
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(initial-scratch-message mh/initial-scratch-message)
 '(isearch-lazy-count t)
 '(ispell-alternate-dictionary @EMACS_ENGLISH_DICT@)
 '(kept-new-versions 1)
 '(kept-old-versions 1)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(mode-line-compact nil)
 '(org-agenda-files nil)
 '(outline-minor-mode-prefix "\3\23")
 '(package-selected-packages '(dash with-editor))
 '(read-file-name-completion-ignore-case t)
 '(rmail-preserve-inbox t)
 '(save-abbrevs nil)
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(set-mark-command-repeat-pop t)
 '(shift-select-mode nil)
 '(size-indication-mode nil)
 '(smerge-command-prefix "\3m")
 '(standard-indent 2)
 '(todo-directory @EMACS_TODO_DIR@)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(tsc-dyn-get-from nil)
 '(use-short-answers t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "UKWN" :slant normal :weight normal :height 140 :width normal))))
 '(doom-modeline-buffer-file ((t (:inherit (doom-modeline mode-line-buffer-id bold)))))
 '(doom-modeline-buffer-major-mode ((t (:inherit (doom-modeline-emphasis bold) :background "#1a1a1a"))))
 '(doom-modeline-buffer-minor-mode ((t (:inherit (doom-modeline font-lock-doc-face) :background "#1a1a1a" :slant italic :weight normal))))
 '(doom-modeline-input-method ((t (:inherit doom-modeline-emphasis :background "#1a1a1a"))))
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
 '(trailing-whitespace ((t (:background "#ee0000"))))
 '(vertical-border ((t (:background "#202020" :foreground "#303030")))))
