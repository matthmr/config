(require 'git-gutter)

(global-set-key (kbd "C-x v m")   #'git-gutter-mode)

(global-set-key (kbd "C-x v n")   #'git-gutter:next-hunk)
(global-set-key (kbd "C-x v p")   #'git-gutter:previous-hunk)
(global-set-key (kbd "C-x v +")   #'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v SPC") #'git-gutter:mark-hunk)
(global-set-key (kbd "C-x v U")   #'git-gutter:revert-hunk)
(global-set-key (kbd "C-x v a")   #'git-gutter:stage-hunk)

(defun mh/gg (load)
  (if load
    (progn
      (add-hook 'prog-mode-hook #'git-gutter-mode))
    (progn
      (remove-hook 'prog-mode-hook #'git-gutter-mode))))

; min
(mh/provide 'gg #'mh/gg t)
