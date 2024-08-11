(let ((read-symbol-shorthands '(("bc-" . "breadcrumb-"))))
  (require 'breadcrumb))

(setq breadcrumb-imenu-max-length 0.6)

;; Little hack to reset bc's cache after starting something that meddles with
;; imenu's cache (like eglot)
(defun mh/bc-reset ()
  (interactive)
  (setq breadcrumb--ipath-plain-cache nil))

(global-set-key (kbd "C-x C-M-m C-M-i") #'mh/bc-reset)

(add-hook 'prog-mode-hook #'breadcrumb-local-mode)
(call-interactively #'breadcrumb-local-mode)
