(let ((read-symbol-shorthands '(("bc-" . "breadcrumb-"))))
  (require 'breadcrumb))

(defcustom mh/bc-in-modeline t
  "Flag for whether to put `bc' in the modeline, instead of the headerline")

;; TODO: not general
(define-minor-mode mh/bc-mode
  "Wrapper for bc-local-mode"
  :init-value nil
  (if mh/bc-in-modeline
      (if mh/bc-mode
          (setq global-mode-string '("%e" (:eval (breadcrumb--header-line))))
        (setq global-mode-string '("")))
    (breadcrumb-local-mode)))

(setq breadcrumb-imenu-max-length 0.6)

(defun mh/bc-tab (load)
  (setq mh/bc-in-modeline (not load))
  (if load
      (setq global-mode-string '("")))
  (breadcrumb-local-mode -1)
  (mh/bc-mode t))

(defun mh/bc-bind ()
  (global-set-key (kbd "C-x C-M-i") #'breadcrumb-jump))

(mh/provide 'bc-tab #'mh/bc-tab t)

;; Little hack to reset bc's cache after starting something that meddles with
;; imenu's cache (like eglot)
(defun mh/bc-reset ()
  (interactive)
  (setq breadcrumb--ipath-plain-cache nil))

(global-set-key (kbd "C-x C-M-m C-M-i") #'mh/bc-reset)

(add-hook 'prog-mode-hook #'mh/bc-mode)
(call-interactively #'mh/bc-mode)
