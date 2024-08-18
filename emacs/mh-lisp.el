;;; mh-lisp.el --- Generic LISP utils

;; Copyright (C) 2024 mH

;; Author: mH <github.com/matthmr>
;; Version: 1.0.0

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;;  Some LISP utilities

(defun mh/with-prefix (prefix function)
  (cond
   ((eq current-prefix-arg nil)
    (setq current-prefix-arg prefix))
   ((eq current-prefix-arg '-)
    (setq current-prefix-arg (* -1 prefix)))
   (t t))
  (call-interactively function))


;; TODO: we could use this instead of `keyboard-quit'
;; (defun mh/rep-clean-up ()
;;   (interactive)
;;   (message nil))

(defun mh/set-mode-map-rep (prefix-mode prefix-str msg reps unqs
                            &optional init dele)
  ;; just in case
  (global-unset-key (kbd prefix-str))

  (let ((map (intern (concat prefix-mode "-map")))
        (mode (intern (concat prefix-mode "-mode")))
        (rep-cmds (intern (concat prefix-mode "-rep-cmds")))
        (keep-pred (intern (concat prefix-mode "-keep-pred"))))

    ;; bind local symbols globally & define keymap
    (eval `(setq ,map (make-sparse-keymap) ,rep-cmds nil))

    ;; populate repeating predicates
    (eval
      `(dolist (el reps)
        (push (eval (cdr el)) ,rep-cmds)))

    ;; define 'keep' predicate function. non-nil means the map is kept
    (let ((dele (if dele (list dele) t)))
      (eval
       `(defun ,keep-pred ()
          (or (and (memq this-command ,rep-cmds))
              (progn ,dele nil)))
       ))

    ;; define the mode
    (let ((init (if init (list init) t)))
      (eval
       `(defun ,mode ()
         (interactive)
         ,init
         (set-transient-map ,map #',keep-pred nil ,msg))
       ))

    ;; bind to mode
    (eval
     `(global-set-key (kbd prefix-str) ',mode))

    ;; populate built-ins
    (let ((built-in '(("<escape>" . 'keyboard-quit)
                       ("[" . 'keyboard-quit)
                       ("?" .
                        (eval `(lambda () (interactive)
                                 (describe-keymap ',map))))
                       )))
      (eval
       `(dolist (el built-in)
          (let ((bind-key (car el))
                (bind-cmd (cdr el)))
            (define-key ,map (kbd bind-key) (eval bind-cmd)))
          )
       ))

    ;; populate keymap
    (eval
     `(dolist (el (append unqs reps))
       (let ((bind-key (car el))
             (bind-cmd (cdr el)))
         (define-key ,map (kbd bind-key) (eval bind-cmd))
         )))
  ))
