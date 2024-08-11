(add-to-list 'load-path @EMACS_MULTIPLE_CURSORS@)

(require 'multiple-cursors)

(global-set-key (kbd "C-x C-M-a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x C-M-e") 'mc/edit-lines)
(global-set-key (kbd "C-x C-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x C-M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x M-n")   'mc/skip-to-next-like-this)
(global-set-key (kbd "C-x M-p")   'mc/skip-to-previous-like-this)

(setq-default mc/always-run-for-all t)
