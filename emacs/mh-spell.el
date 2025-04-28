(require 'ispell)

(global-set-key (kbd "C-c s s") 'ispell-word)
(global-set-key (kbd "C-c s TAB") 'ispell-complete-word)

(add-hook #'completion-at-point-functions #'ispell-completion-at-point t t)
