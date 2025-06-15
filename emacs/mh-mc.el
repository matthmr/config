(add-to-list 'load-path @EMACS_MULTIPLE_CURSORS@)

(require 'multiple-cursors)

(global-set-key (kbd "C-x m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x m m") 'mc/edit-lines)
(global-set-key (kbd "C-x m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x m M-n") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-x m M-p") 'mc/skip-to-previous-like-this)

(setq-default mc/always-run-for-all t)
