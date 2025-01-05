(require 'corfu-terminal)

(define-key corfu-map (kbd "C-v") 'scroll-up-command)
(define-key corfu-map (kbd "M-v") 'scroll-down-command)
(define-key corfu-map (kbd "SPC") 'corfu-insert-separator)
(define-key corfu-map (kbd "[")   'corfu-quit)

(global-corfu-mode 1)
(corfu-terminal-mode 1)

(setq tab-always-indent 'complete)

(defun mh/corfu-show-eglot-doc ()
  (interactive)

  (let ((comp (nth corfu--index corfu--candidates)))
    (let* ((eglot-response (get-text-property 0 'eglot--lsp-item comp))
           (doc (cadr
                 (memq :value (cadr
                               (memq :documentation eglot-response))))))
      (message doc)
      )))

(define-key corfu-map (kbd "?") #'mh/corfu-show-eglot-doc)
