(require 'eglot)

;;;; Eglot

(defcustom mh/eglot-bc t
  "Flag for whether Eglot should use `breadcrumb'")

(defcustom mh/eglot-ep t
  "Flag for whether Eglot should use `mh/eldoc-popon'")

(defcustom mh/eglot-markdown-doc t
  "Flag for whether Eglot should tell the LSP to use markdown documentation")

(setq eglot-autoshutdown t
      eglot-events-buffer-size 100000
      eglot-highlight-symbol-face '((t (:inherit underline)))
      eglot-menu-string ""
      eglot-prefer-plaintext t
      eglot-stay-out-of '("flymake")
      eglot-ignored-server-capabilities '(:inlayHintProvider)
)

(when mh/eglot-markdown-doc
  (require 'markdown-mode)
  (setq eglot-prefer-plaintext nil))

(when mh/eglot-bc
  (mh/load "bc")
  (define-key eglot-mode-map "\C-x\C-\M-i" #'breadcrumb-jump))

(when mh/eglot-ep
  (mh/load "ep")

  (defun mh/eglot-ep (docs interactive)
    (when eglot--highlights
      (add-hook 'post-command-hook #'mh/ep-kill)
      (mh/ep-with-buffer (eldoc--format-doc-buffer docs))))

  (mh/ep-setup #'mh/eglot-ep))

(define-key eglot-mode-map "\C-c\C-M-ld" #'eglot-find-declaration)
(define-key eglot-mode-map "\C-c\C-M-li" #'eglot-find-implementation)
(define-key eglot-mode-map "\C-c\C-M-lt" #'eglot-find-typeDefinition)
(define-key eglot-mode-map "\C-c\C-M-lr" #'eglot-rename)
(define-key eglot-mode-map "\C-c\C-M-lv" #'eglot-inlay-hints-mode)
(define-key eglot-mode-map "\C-c\C-M-lk" #'eglot-shutdown)
(define-key eglot-mode-map "\C-c\C-M-l\M-k" #'eglot-shutdown-all)
(define-key eglot-mode-map "\C-c\C-M-ll" #'eglot-reconnect)
(define-key eglot-mode-map "\C-c\C-M-lm" #'flymake-mode)

;;;; Flymake

(define-key flymake-mode-map "\C-c\C-\M-n" 'flymake-goto-next-error)
(define-key flymake-mode-map "\C-c\C-\M-p" 'flymake-goto-prev-error)
(define-key flymake-mode-map "\C-c\C-\M-d" 'flymake-show-buffer-diagnostics)

;;;; Call

;; (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

(call-interactively #'eglot)
