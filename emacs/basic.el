;;;; Emacs basic config

(load @EMACS_KEYBINDINGS@)
(load @EMACS_ABBREVS@) ; the abbrevs file is not in this repository

(setq-default
  create-lockfiles nil
  inhibit-startup-screen t
  scroll-error-top-bottom t
  scroll-preserve-screen-position t
  scroll-margin 2
  scroll-conservatively 100
  next-screen-context-lines 2
  column-number-indicator-zero-based nil
  split-height-threshold 30
  vc-handled-backends '(Git)
  temporary-file-directory @EMACS_TMP@
  tab-width 2
  window-divider-default-places 'right-only
  kill-ring-max 500
  message-log-max 7000
  select-enable-clipboard nil
  select-active-regions nil
  frame-title-format '("emacs@linux - %b")
  inhibit-x-resources t
  indent-tabs-mode nil
  sentence-end-double-space nil
  ; min
  local-enable-local-variables nil
  ; min
  enable-local-variables nil
  ; min
  enable-dir-local-variables nil
  ring-bell-function 'ignore
  echo-keystrokes 0.1
  vc-find-revision-no-save t
  auto-save-list-file-prefix nil
  completion-ignore-case t
  read-buffer-completion-ignore-case t
  goal-column nil
  gc-cons-threshold 600000 ; 600 M
  confirm-kill-emacs 'y-or-n-p
  backup-inhibited nil
  auto-save-default t
  hi-lock-face-defaults '("inverse-video")
  disabled-command-function nil
  comment-column 0
  completions-format 'one-column
  require-final-newline t
  kill-whole-line t
  enable-recursive-minibuffers t
  search-whitespace-regexp ".*?"
  show-trailing-whitespace t
  lazy-count-prefix-format "[%s/%s] "
  overlay-arrow-string ">")

(setq-local default-directory @EMACS_TMP@)

(fset 'yes-or-no-p 'y-or-n-p)

;;;; Major modes

;;; Gnus

(setq-default
  gnus-article-save-directory @EMACS_GNUS@
  gnus-startup-file @EMACS_NEWSRC@)

;;; C

(setq c-default-style "linux"
      c-basic-offset 2)

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "TAB") #'indent-for-tab-command)
  (define-key c-mode-map (kbd "C-M-h") #'backward-kill-sexp))

;;; Python

(setq python-basic-offset 2
      python-indent-offset 2)

;;; Shell

(setq sh-basic-offset 2)

;;; Hl-line

(defface mh/hl-line
  '((t :inherit nil :underline t))
  "Default face for highlighting the current line in Hl-Line mode.")

(setq hl-line-face 'mh/hl-line)

;;; Eshell

(setq eshell-banner-message
      "      ___
     (.. |
     (<> |
    / __  \\
   ( /  \\ /|
  _/\\ __)/_)
  \\/-____\\/\n\n")

(setq eshell-prompt-regexp "^\\[.+\\]> ")

(defun mh/eshell-block (str)
  (concat
   (propertize "[" 'face `(:foreground "red"))
   (propertize str 'face `(:weight bold))
   (propertize "]" 'face `(:foreground "red"))))

(setq eshell-prompt-function
  (lambda ()
    (format "%s %s\n%s%s "
            (concat (propertize "[" 'face `(:foreground "red"))
                    (propertize (user-login-name) 'face `(:weight bold))
                    (propertize "@" 'face `(:foreground "red"))
                    (propertize (system-name) 'face `(:weight bold))
                    (propertize "]" 'face `(:foreground "red")))
            (mh/eshell-block "eshell")
            (mh/eshell-block (eshell/pwd))
            (propertize ">" 'face `(:foreground "red")))))

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(with-eval-after-load "markdown-mode"
  (define-key markdown-mode-map "\M-TAB" 'completion-at-point)

  (set-face-attribute 'markdown-header-face-1 nil :inherit '(outline-1))
  (set-face-attribute 'markdown-header-face-2 nil :inherit '(outline-2))
  (set-face-attribute 'markdown-header-face-3 nil :inherit '(outline-3))
  (set-face-attribute 'markdown-header-face-4 nil :inherit '(outline-4))
  (set-face-attribute 'markdown-header-face-5 nil :inherit '(outline-5))
  (set-face-attribute 'markdown-header-face-6 nil :inherit '(outline-6))
  )

;;;; Minor modes

;;; Scroll-lock

(with-eval-after-load "scroll-lock"
  (define-key scroll-lock-mode-map "\C-n" 'scroll-lock-next-line)
  (define-key scroll-lock-mode-map "\C-p" 'scroll-lock-previous-line)
  (define-key scroll-lock-mode-map "\M-{" 'scroll-lock-backward-paragraph)
  (define-key scroll-lock-mode-map "\M-}" 'scroll-lock-forward-paragraph))

;;; Global minor modes

; min
(icomplete-mode 1)
(electric-pair-mode 1)
(global-display-fill-column-indicator-mode 1)
; min
(tab-bar-mode 1)
(auto-save-mode 1)

;;;; Hooks

;;; Text mode

(add-hook 'text-mode-hook 'auto-fill-mode)
; (add-hook 'html-mode-hook 'nxml-mode)

(add-hook 'mail-mode-hook
          (lambda ()
            (cd "/tmp/emacs")))

(add-hook 'outline-mode-hook 'auto-fill-mode)
(add-hook 'diff-mode-hook 'outline-minor-mode)

(add-hook 'latex-mode-hook
          (lambda ()
            (define-key latex-mode-map
                        (kbd "M-TAB") 'completion-at-point)))
(add-hook 'tex-mode-hook
          (lambda ()
            (define-key tex-mode-map
                        (kbd "M-TAB") 'completion-at-point)))

(add-hook 'diff-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (display-fill-column-indicator-mode 1)
            (auto-fill-mode 1)
            (setq-local show-trailing-whitespace t)))

(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (setq truncate-lines t)))

;;; Prog mode

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local
              tab-width 2
              indent-tabs-mode nil
              comment-column 0)
            (let ((capf (buffer-local-value
                          'completion-at-point-functions
                          (current-buffer))))
              (setq-local completion-at-point-functions
                (append
                  (delete t capf)
                  (list #'ispell-completion-at-point
                        #'comint-filename-completion
                        t))
                ))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local
              comment-start "//"
              comment-end ""
              page-delimiter "^/\\{4\\}")
            ; min
            (outline-minor-mode 1)
            (auto-fill-mode 1)
            (abbrev-mode -1)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local page-delimiter "^#\\{4\\}")
            (outline-minor-mode 1)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local page-delimiter "^#\\{4\\}")))

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local page-delimiter "^;\\{4\\}")
            (rainbow-delimiters-mode 1)))

(add-hook 'scheme-mode-hook
          (lambda ()
            (setq-local page-delimiter "^;\\{4\\}")
            (rainbow-delimiters-mode 1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local page-delimiter "^;\\{4\\}")
            (rainbow-delimiters-mode 1)))

(add-hook 'compilation-mode-hook #'mh/edit-buffer-n)
(add-hook 'compilation-minor-mode-hook #'mh/edit-buffer-n)
(add-hook 'gud-mode-hook #'mh/edit-buffer-n)

;;;; Misc

(add-hook 'vc-dir-mode-hook
  (lambda ()
    (define-key vc-dir-mode-map "!" 'vc-edit-next-command)))

(add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

(with-eval-after-load "compilation"
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;;;; Auto-mode

(add-to-list 'auto-mode-alist '("\\(neo\\)?mutt-.*" . message-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\|MERGE_MSG" . diff-mode))
(add-to-list 'auto-mode-alist '("TAG_EDITMSG" . text-mode))
