(require 'zen-mode)

(defun mh/toggle-zen ()
  (interactive)
  (zen-mode 'toggle))

(global-set-key (kbd "C-x C-M-m C-M-z") 'mh/toggle-zen)
