;;;; mh-ed.el --- Very small minor mode that adds some VI-like bindings

;; Copyright (C) 2023-2024 mH

;; Author: mH <github.com/matthmr>
;; Version: 2.0.0

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

(defvar-local ed-current-mode 'emacs)

(defun mh/ed-kill-region-then-exit ()
  "Call `kill-region' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'kill-region)
  (ed-mode -1))

(defun mh/ed-delete-char-then-exit ()
  "Call `delete-char' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'delete-char)
  (ed-mode -1))

(defun mh/ed-delete-up-to-char-then-exit ()
  "Kill `zap-up-to-char' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'zap-up-to-char)
  (ed-mode -1))

(defun mh/ed-overwrite-mode-then-exit ()
  "Kill `overwrite-mode' interactively, then exit `ed-mode'"
  (interactive)
  (call-interactively #'overwrite-mode)
  (ed-mode -1))

(defun mh/ed-open-below-then-exit ()
  "Open a line above, then exit `ed-mode'"
  (interactive)
  (call-interactively #'move-end-of-line)
  (insert "\n")
  (ed-mode -1))

(defun mh/ed-open-above-then-exit ()
  "Open a line below, then exit `ed-mode'"
  (interactive)
  (call-interactively #'move-beginning-of-line)
  (call-interactively #'split-line)
  (ed-mode -1))

(defun mh/ed-replace-char ()
  "Replace character, and keep the mode"
  (interactive)
  (call-interactively #'delete-char)

  (ed-mode -1)

  ;; assert this is not
  (let ((key (read-key-sequence "r> ")))
    (when (eq (key-binding key) 'self-insert-command))
      (let ((char (string-to-char key)))
        (insert char))
    )

  (ed-mode 1))

;; Ignore any self-inserting keybindings that are not bound to cursor movements
(defvar ed-mode-map
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
    (define-key map "-" 'move-end-of-line)
    (define-key map "." 'mh/scroll-up)
    (define-key map "," 'mh/scroll-down)
    (define-key map "{" 'backward-paragraph)
    (define-key map "}" 'forward-paragraph)
    (define-key map "g" 'goto-line)
    (define-key map "w" 'forward-word)
    (define-key map "b" 'backward-word)
    (define-key map "W" 'forward-sexp)
    (define-key map "B" 'backward-sexp)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map "f"  'mh/to-char)
    (define-key map "0" 'beginning-of-line)
    (define-key map "-" 'end-of-line)
    (define-key map ";" 'point-to-register)
    (define-key map "'" 'jump-to-register)
    (define-key map "m" 'recenter-top-bottom)
    (define-key map "M" 'move-to-window-line-top-bottom)

    ;; toggle/quit

    (define-key map "[" 'keyboard-quit)
    (define-key map "]" 'mh/ed-toggle)
    (define-key map "s" 'mh/ed-toggle)

    ;; `*-then-exit' commands

    (define-key map "d" 'mh/ed-delete-char-then-exit)
    (define-key map "c" 'mh/ed-kill-region-then-exit)
    (define-key map "e" 'mh/ed-open-below-then-exit)
    (define-key map "E" 'mh/ed-open-above-then-exit)
    (define-key map "O" 'mh/ed-overwrite-mode-then-exit)
    (define-key map "F" 'mh/ed-delete-up-to-char-then-exit)

    ;; editing

    (define-key map "p" 'yank)
    (define-key map "P" 'yank-pop)
    (define-key map "o" 'kill-region)
    (define-key map "y" 'kill-ring-save)
    (define-key map "q" 'quoted-insert)
    (define-key map " " 'set-mark-command)
    (define-key map "/" 'isearch-forward)
    (define-key map "?" 'isearch-backward)
    (define-key map "u" 'undo)
    (define-key map "i" 'undo-only)
    (define-key map "U" 'undo-redo)
    (define-key map "\"" 'copy-to-register)
    (define-key map "r" 'mh/ed-replace-char)
    ;; (define-key map "\"" 'insert-register)

    ;; misc

    (define-key map "x" 'execute-extended-command)
    (define-key map "a" 'universal-argument)
    (define-key map "n" 'repeat)

    ;; the only map that breaks the rules is the `describe-keymap' one
    (define-key map "\C-xm" (lambda () (interactive)
                              (describe-keymap 'ed-mode-map)))
    map))

(global-set-key (kbd "M-]") #'mh/ed-toggle)
(global-set-key (kbd "C-M-[") #'mh/ed-toggle)

(defun mh/ed-toggle ()
  (interactive)
  (ed-mode 'toggle))

;;;; Minor Mode

(define-minor-mode ed-mode
  "This mode will add some VI-like bindings"
  :interactive nil
  (setq-local ed-current-mode (if ed-mode 'ed 'emacs)))
