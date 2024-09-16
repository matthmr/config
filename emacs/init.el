;;; Dot emacs

(load @EMACS_MH_LISP@)

;;; Load paths

(add-to-list 'load-path @EMACS_LISP@)
@EMACS_LOAD_EXTRAS@

;;; Load some programs (with no extra configs)

@EMACS_REQUIRE@

;;; Custom loaders

@EMACS_LOAD@

;;; Scroll-lock

(with-eval-after-load "scroll-lock"
  (define-key scroll-lock-mode-map "\C-n" 'scroll-lock-next-line)
  (define-key scroll-lock-mode-map "\C-p" 'scroll-lock-previous-line)
  (define-key scroll-lock-mode-map "\M-{" 'scroll-lock-backward-paragraph)
  (define-key scroll-lock-mode-map "\M-}" 'scroll-lock-forward-paragraph))

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; TTY vs PTS

(defvar mh/display-table
  `((truncation . ?…)
    (wrap . ?↲)
    (selective-display . ,(string-to-vector "↓"))
    (vertical-border . ?│))
   "Display table alist")

(let ((env/session (getenv "SESSION")))
  (if (or (not env/session)
          (string= env/session "tmux")
          (string= env/session "shell"))
      ;; display time, and use visual line
      (progn
        (setf (cdar mode-line-format)
              (cons '(:eval (mh/ed-string)) (cdar mode-line-format)))
        (setq-default Man-switches "-Tascii") ;; cannot render UTF-8
        (setq-default truncate-lines nil))
    ;; some character which TTYs can't display properly, xterm-mouse-mode,
    ;; themes
    (progn
      (setq-default truncate-lines t)

      (dolist (pair mh/display-table)
        (let ((attr (car pair))
              (val (cdr pair)))
          (set-display-table-slot standard-display-table attr val)))

      (mh/load "line")
      (global-whitespace-mode)

      ;; min
      ;; (global-hl-line-mode)
      ;; (mouse-wheel-mode t)
      ;; (xterm-mouse-mode t)

      (let ((base16-theme-256-color-source 'base16-shell))
        (load-theme 'base16-shell t))
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
 '(abbrev-file-name @EMACS_ABBREV@)
 '(auth-source-save-behavior nil)
 '(auto-save-file-name-transforms '((".*" @EMACS_TMP@ t)))
 '(auto-save-interval 300)
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . @EMACS_BACKUP@)))
 '(base16-theme-distinct-fringe-background nil)
 '(browse-url-browser-display nil)
 '(browse-url-browser-function
   (lambda
     (url &optional args)
     (async-shell-command
      (format "uhandle -d '%s'" url "*url*" "*url-error*"))))
 '(browse-url-text-browser "w3m")
 '(column-number-mode t)
 '(comment-column 0)
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
 '(display-time-format "%Y%m%d-%w %I%M%p")
 '(ediff-combination-pattern
   '("<<<<<<< ours" A "||||||| parent" Ancestor ">>>>>>> theirs" B "======= end"))
 '(ediff-keep-variants nil)
 '(eldoc-echo-area-use-multiline-p 1)
 '(epg-pinentry-mode 'loopback)
 '(eshell-buffer-maximum-lines 8000)
 '(eshell-directory-name @EMACS_ESHELL@)
 '(eww-search-prefix "https://google.com/search?q=")
 '(file-name-shadow-tty-properties '(invisible t intangible t field shadow))
 '(fill-column 80)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(goal-column nil)
 '(gud-key-prefix [3 1])
 '(icomplete-matches-format "[%s/%s] ")
 '(icomplete-mode t)
 '(icomplete-show-matches-on-no-input t)
 '(imenu-auto-rescan t)
 '(initial-scratch-message mh/initial-scratch-message)
 '(isearch-lazy-count t)
 '(ispell-alternate-dictionary @EMACS_ENGLISH_DICT@)
  ; min
 '(kept-new-versions 50)
  ; min
 '(kept-old-versions 50)
 '(menu-bar-mode nil)
 '(mh/eglot-markdown-doc nil)
 '(mh/tm-use-region-as-mark 'always)
 '(mode-line-compact nil)
 '(mpc-browser-tags '(Artist Album Title Filename))
 '(org-agenda-files nil)
 '(outline-minor-mode-prefix "\3\23")
 '(package-selected-packages '(dash with-editor))
 '(read-file-name-completion-ignore-case t)
 '(rmail-preserve-inbox t)
 '(save-abbrevs nil)
 '(scheme-program-name "guile")
 '(scroll-bar-mode nil)
 '(server-kill-new-buffers nil)
 '(set-mark-command-repeat-pop t)
 '(shift-select-mode nil)
 '(size-indication-mode nil)
 '(smerge-command-prefix "\3m")
 '(standard-indent 2)
 '(todo-directory @EMACS_TODO_DIR@)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(use-short-answers t)
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inverse-video t))))
 '(eglot-mode-line ((t nil)))
 '(fill-column-indicator ((t (:background "color-18" :foreground "color-19"))))
 '(icomplete-first-match ((t (:foreground "#dc9656"))))
 '(log-view-message ((t (:extend t :background "grey85" :foreground "black"))))
 '(smerge-base ((t (:extend t :background "#ffffaa" :foreground "black"))))
 '(smerge-lower ((t (:extend t :background "#ddffdd" :foreground "black"))))
 '(smerge-markers ((t (:extend t :background "grey85" :foreground "black"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "#aaffaa" :foreground "black"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "#ffbbbb" :foreground "black"))))
 '(smerge-upper ((t (:extend t :background "#ffdddd" :foreground "black"))))
 '(trailing-whitespace ((t (:background "color-124"))))
 '(vertical-border ((t (:background "color-18" :foreground "color-19")))))
