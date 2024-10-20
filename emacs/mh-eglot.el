(require 'eglot)

;;;; Eglot

(defcustom mh/eglot-bc t
  "Flag for whether Eglot should use `breadcrumb'")

(defcustom mh/eglot-hi t
  "Flag for whether Eglot should use `eglot-hierarchy'")

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
      eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider
                                          :documentRangeFormattingProvider
                                          :documentFormattingProvider
                                          :codeActionProvider)
)

(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

(if mh/eglot-markdown-doc
  (progn
    (require 'markdown-mode)
    (setq eglot-prefer-plaintext nil))
  (progn
    (fmakunbound 'gfm-view-mode)))

(when mh/eglot-bc
  (mh/load "bc")
  (define-key eglot-mode-map (kbd "C-x C-M-i") #'breadcrumb-jump))

(when mh/eglot-hi
  (require 'eglot-hierarchy)
  (define-key eglot-mode-map (kbd "C-c l u") #'eglot-hierarchy-call-hierarchy)
  (define-key eglot-mode-map (kbd "C-c l M-u")
              #'eglot-hierarchy-type-hierarchy))

(when mh/eglot-ep
  (mh/load "ep")

  (defun mh/eglot-ep (docs interactive)
    (when eglot--highlights
      (add-hook 'post-command-hook #'mh/ep-kill)
      (mh/ep-with-buffer (eldoc--format-doc-buffer docs))))

  (mh/ep-setup #'mh/eglot-ep))

(define-key eglot-mode-map (kbd "C-c l .") #'eglot-find-declaration)
(define-key eglot-mode-map (kbd "C-c l i") #'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-c l t") #'eglot-find-typeDefinition)
(define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
(define-key eglot-mode-map (kbd "C-c l v") #'eglot-inlay-hints-mode)
(define-key eglot-mode-map (kbd "C-c l k") #'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c l M-k") #'eglot-shutdown-all)
(define-key eglot-mode-map (kbd "C-c l g") #'eglot-reconnect)
(define-key eglot-mode-map (kbd "C-c l f") #'flymake-mode)

;;;; Flymake

(define-key flymake-mode-map (kbd "C-c C-M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c C-M-p") 'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c C-M-d") 'flymake-show-buffer-diagnostics)

;;;; Call

(call-interactively #'eglot)
