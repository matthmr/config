;;;; mh-ed.el --- Very small minor mode that adds some VI-like bindings

;; Copyright (C) 2023-2025 mh

;; Author: mh <github.com/matthmr>
;; Version: 2.2.4

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
;;  Very small minor mode that adds some VI-like bindings. They are not *actual*
;;  VI bindings, but something close. The main layout of these is that most of
;;  `self-insert-command' keys get some other function

(defun mh/ed-kill-region-then-exit ()
  "Call `kill-region' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'kill-region)
  (mh/ed-toggle))

(defun mh/ed-delete-region-then-exit ()
  "Call `delete-region' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'delete-region)
  (mh/ed-toggle))

(defun mh/ed-delete-char-then-exit ()
  "Call `delete-char' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'delete-char)
  (mh/ed-toggle))

(defun mh/ed-delete-up-to-char-then-exit ()
  "Kill `zap-up-to-char' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'zap-up-to-char)
  (mh/ed-toggle))

(defun mh/ed-overwrite-mode-then-exit ()
  "Kill `overwrite-mode' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'overwrite-mode)
  (mh/ed-toggle t))

(defun mh/ed-open-below-then-exit ()
  "Open a line above, then exit `ed-mode'"
  (interactive)
  (call-interactively #'move-end-of-line)
  (insert "\n")
  (mh/ed-toggle))

(defun mh/ed-open-above-then-exit ()
  "Open a line below, then exit `ed-mode'"
  (interactive)
  (call-interactively #'move-beginning-of-line)
  (call-interactively #'split-line)
  (mh/ed-toggle))

(defun mh/ed-replace-char ()
  "Replace character, and keep the mode"
  (interactive)
  (call-interactively #'delete-char)

  (mh/ed-toggle)

  ;; assert this is not
  (let ((key (read-key-sequence "r> ")))
    (when (eq (key-binding key) 'self-insert-command))
      (let ((char (string-to-char key)))
        (insert char))
    )

  (mh/ed-toggle))

(defcustom mh/ed-on nil
  "List of functions to call when `ed-mode' is turned on")

(defcustom mh/ed-off nil
  "List of functions call when `ed-mode' is turned off")

(defun mh/ed-cur (load)
  (if load
    (progn
      (push 'mh/cursor-box mh/ed-on)
      (push 'mh/cursor-bar mh/ed-off))
    (progn
      (pop mh/ed-on)
      (pop mh/ed-off)
      (mh/cursor-box))))

; min
(mh/provide 'ed-cur #'mh/ed-cur t)

;; Ignore any self-inserting keybindings that are not bound to cursor movements
(defvar mh/ed-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)

    ;; movement

    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "h" 'backward-char)
    (define-key map "l" 'forward-char)
    (define-key map "J" 'mh/next-line-keep)
    (define-key map "K" 'mh/previous-line-keep)
    (define-key map "H" 'mh/scroll-right)
    (define-key map "L" 'mh/scroll-left)
    (define-key map "0" 'move-beginning-of-line)
    (define-key map "." 'mh/scroll-up)
    (define-key map "," 'mh/scroll-down)
    (define-key map "{" 'backward-paragraph)
    (define-key map "}" 'forward-paragraph)
    (define-key map "g" 'goto-line)
    (define-key map "w" 'forward-word)
    (define-key map "b" 'backward-word)
    (define-key map "W" 'forward-sexp)
    (define-key map "B" 'backward-sexp)
    (define-key map "<" 'indent-rigidly-left-to-tab-stop)
    (define-key map ">" 'indent-rigidly-right-to-tab-stop)
    (define-key map "f" 'mh/to-char)
    (define-key map "t" 'mh/up-to-char)
    (define-key map "0" 'beginning-of-line)
    (define-key map "-" 'end-of-line)
    (define-key map "$" 'end-of-line)
    (define-key map ";" 'point-to-register)
    (define-key map "'" 'jump-to-register)
    (define-key map "m" 'recenter-top-bottom)
    (define-key map "M" 'move-to-window-line-top-bottom)
    (define-key map "#" 'comment-dwim)
    (define-key map "n" 'isearch-repeat-forward)
    (define-key map "N" 'isearch-repeat-backward)
    (define-key map "v" 'end-of-buffer)
    (define-key map "V" 'beginning-of-buffer)
    (define-key map "x" 'exchange-point-and-mark)
    (define-key map "|" 'back-to-indentation)
    (define-key map (kbd "RET") 'next-line)
    (define-key map (kbd "DEL") 'previous-line)

    ;; toggle/quit

    (define-key map "[" 'mh/keyboard-quit)
    (define-key map "]" 'mh/ed-toggle)
    (define-key map "s" 'mh/ed-toggle)

    ;; `*-then-exit' commands

    (define-key map "d" 'mh/ed-delete-char-then-exit)
    (define-key map "c" 'mh/ed-kill-region-then-exit)
    (define-key map "C" 'mh/ed-delete-region-then-exit)
    (define-key map "e" 'mh/ed-open-below-then-exit)
    (define-key map "E" 'mh/ed-open-above-then-exit)
    (define-key map "R" 'mh/ed-overwrite-mode-then-exit)
    (define-key map "F" 'mh/ed-delete-up-to-char-then-exit)

    ;; editing

    (define-key map "p" 'yank)
    (define-key map "P" 'yank-pop)
    (define-key map "o" 'kill-region)
    (define-key map "y" 'kill-ring-save)
    (define-key map "q" 'quoted-insert)
    (define-key map " " 'set-mark-command)
    (define-key map "/" 'isearch-forward-regexp)
    (define-key map "?" 'isearch-backward-regexp)
    (define-key map "u" 'undo)
    (define-key map "i" 'undo-only)
    (define-key map "U" 'undo-redo)
    (define-key map "\"" 'copy-to-register)
    (define-key map "r" 'mh/ed-replace-char)
    (define-key map "D" 'kill-whole-line)
    (define-key map "O" 'delete-region)
    ;; (define-key map "\"" 'insert-register)

    ;; misc

    (define-key map ":" 'execute-extended-command)
    (define-key map "a" 'universal-argument)
    (define-key map "`" 'negative-argument)
    (define-key map "z" 'repeat)
    (define-key map "\\" 'repeat-complex-command)

    ;; the only map that breaks the rules is the `describe-keymap' one
    (define-key map "\C-xm" (lambda () (interactive)
                              (describe-keymap 'mh/ed-map)))
    map))

(global-set-key (kbd "C-M-]") #'mh/ed-toggle)
(global-set-key (kbd "C-M-[") #'mh/ed-toggle)

(defun mh/ed-toggle (&optional skip-undo-overwrite)
  (interactive)
  (let ((funs (if mh/ed mh/ed-off mh/ed-on)))
    (dolist (fun funs)
      (funcall fun)))
  (when (and (not skip-undo-overwrite) overwrite-mode)
    (overwrite-mode -1))
  (mh/ed 'toggle))

(defun mh/ed-string ()
  "Displays the string state of `mh/ed'. `=' for ed on, `*' for ed off"
  (if mh/ed " = " " * "))

;;;; Minor Mode

(define-minor-mode mh/ed
  "This mode will add some VI-like bindings"
  :interactive nil
  :keymap mh/ed-map)
