(require 'tetris)

(define-key tetris-mode-map "h" #'tetris-move-left)
(define-key tetris-mode-map "j" #'tetris-move-down)
(define-key tetris-mode-map "d" #'tetris-move-down)
(define-key tetris-mode-map "l" #'tetris-move-right)

(define-key tetris-mode-map "k" #'tetris-rotate-next)
(define-key tetris-mode-map "K" #'tetris-rotate-prev)

(define-key tetris-mode-map "s" (lambda () (interactive)
                                  (tetris-rotate-next)
                                  (tetris-rotate-next)))
