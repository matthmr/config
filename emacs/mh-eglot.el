(require 'eglot)

;;;; Eglot

(defvar mh/eglot-bc t
  "Flag for whether Eglot should use `breadcrumb'")

(defvar mh/eglot-markdown-doc t
  "Flag for whether Eglot should tell the LSP to use markdown documentation")

(setq eglot-autoshutdown t
      eglot-events-buffer-size 100000
      eglot-highlight-symbol-face '((t (:inherit underline)))
      eglot-menu-string ""
      eglot-prefer-plaintext t)
      ;; eglot-stay-out-of '("flymake")
      ;; eglot-ignored-server-capabilities '(:inlayHintProvider)

(when mh/eglot-markdown-doc
  (require 'markdown-mode)
  (setq eglot-prefer-plaintext nil))

(when mh/eglot-bc
  (mh/load "bc")
  (define-key eglot-mode-map "\C-x\C-\M-i" #'breadcrumb-jump))

(define-key eglot-mode-map "\C-c\C-l\C-d" #'eglot-find-declaration)
(define-key eglot-mode-map "\C-c\C-l\C-i" #'eglot-find-implementation)
(define-key eglot-mode-map "\C-c\C-l\C-t" #'eglot-find-typeDefinition)
(define-key eglot-mode-map "\C-c\C-l\C-r" #'eglot-rename)
(define-key eglot-mode-map "\C-c\C-l\C-v" #'eglot-inlay-hints-mode)
(define-key eglot-mode-map "\C-c\C-l\C-k" #'eglot-shutdown)
(define-key eglot-mode-map "\C-c\C-l\C-\M-k" #'eglot-shutdown-all)
(define-key eglot-mode-map "\C-c\C-l\C-l" #'eglot-reconnect)

;;;; Flymake

(define-key flymake-mode-map "\C-c\C-\M-n" 'flymake-goto-next-error)
(define-key flymake-mode-map "\C-c\C-\M-p" 'flymake-goto-prev-error)
(define-key flymake-mode-map "\C-c\C-\M-l" 'flymake-show-buffer-diagnostics)

;;;; Call

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

(call-interactively #'eglot)
