;;; mh-lisp.el --- Generic LISP utils

;; Copyright (C) 2024 mh

;; Author: mh <github.com/matthmr>
;; Version: 1.1.0

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

;;;; Loading

(defun mh/feat-state (feat)
  "Return the state symbol for `feat'"
  (intern (format "%s--state" feat)))

(defvar mh/feats nil
  "List of `mh' features")

(defun mh/load (&optional thing)
  "Loads `mh' file"
  (interactive)

  (if thing
    (setq thing (format @MH_LISP_FROM_LISP@ thing))
    (setq thing
      (read-file-name "Load: " @MH_LIST_FROM_INTERACTIVE@ nil nil nil
        (lambda (e) (string-match "mh.*\.el$" e)))
      ))

  (load thing))

(defun mh/toggle (&optional thing)
  "Toggles `mh' feature"
  (interactive)

  (unless thing
    (setq thing (completing-read "Toggle: " mh/feats)))

  (let ((feat (assoc (intern-soft thing) mh/feats)))
    (if feat
        (let* ((feat-fun (cdr feat))
               (feat-state (mh/feat-state feat-fun)))
          ;; my eyes!
          (funcall feat-fun
                   (eval `(setq ,feat-state (not ,feat-state)))))
      (message "mh/feature `%s' does not exist, or is not set" thing))
    ))

(defun mh/provide (mode feat &optional inert)
  "Provide `feat' as an `mh' feature for `mode'. `feat' should be a function
   that takes one argument that when set to `t' should run initialization code,
   and to `nil' unloading code"
  (push (cons mode feat) mh/feats)

  ;; create a variable with the same name as `mode' suffixed by `--state'
  (let ((feat-state (mh/feat-state feat))
        (active (not inert)))
    (eval `(setq ,feat-state ,active))
    (when active
      (funcall feat active))
    ))

;;;; Keymap

;; TODO: we could use this instead of `keyboard-quit'
;; (defun mh/rep-clean-up ()
;;   (interactive)
;;   (message nil))

(defun mh/set-mode-map-rep (prefix-mode prefix-str reps unqs
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
    (let ((init (if init (list init) t))
          (msg (format "%s>" prefix-mode)))
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

;;;; Cursor

(defun mh/cursor-box ()
  (interactive)
  (send-string-to-terminal "\e[1 q"))

(global-set-key (kbd "C-x C-M-m 1") #'mh/cursor-box)

(defun mh/cursor-bar ()
  (interactive)
  (send-string-to-terminal "\e[5 q"))

(global-set-key (kbd "C-x C-M-m 2") #'mh/cursor-bar)

(defun mh/cursor-under ()
  (interactive)
  (send-string-to-terminal "\e[3 q"))

(global-set-key (kbd "C-x C-M-m 3") #'mh/cursor-under)
