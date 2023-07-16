;;;; mh-basic.el --- Basic layer on top of .emacs.d configuration

;; Copyright (C) 2023 mH

;; Author: mH <github.com/matthmr>
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
;;  Basic layer on top of .emacs.d configuration

;;;; Clipboard (aka `edit-copy')

(global-set-key (kbd "C-x c w")
                (lambda () (interactive)
                  (write-file "/tmp/clipboard")
                  (shell-command "/home/mh/Scripts/edit-copy.sh -i")))
(global-set-key (kbd "C-x c e")
                (lambda () (interactive)
                  (find-file "/tmp/clipboard")
                  (shell-command "/home/mh/Scripts/edit-copy.sh -d")
                  (revert-buffer-quick)))
(global-set-key (kbd "C-x c r")
                (lambda () (interactive)
                  (find-file "/tmp/clipboard")
                  (revert-buffer-quick)))
(global-set-key (kbd "C-x c y")
                (lambda () (interactive)
                  (shell-command "/home/mh/Scripts/edit-copy.sh -d")
                  (insert-file "/tmp/clipboard")))

;;;; VC

(global-set-key (kbd "C-x v C-i")
                (lambda () (interactive)
                  (setq vc-buffer-file-name (buffer-file-name))
                  (async-shell-command
                   (format "git add --patch %s"
                           (if vc-buffer-file-name
                               vc-buffer-file-name
                             ".")))
                  (switch-to-buffer-other-window "*Async Shell Command*")
                  (highlight-regexp "^+.*"  'diff-added)
                  (highlight-regexp "^-.*+" 'diff-removed)
                  (highlight-regexp "^@@.*" 'diff-hunk-header)))

(global-set-key (kbd "C-x v C-d")
                (lambda () (interactive)
                  (setq vc-buffer-file-name (buffer-file-name))
                  (shell-command
                   (format "git diff %s"
                           (if vc-buffer-file-name
                               vc-buffer-file-name
                             ".")))
                  (switch-to-buffer-other-window "*Shell Command Output*")
                  (read-only-mode)
                  (diff-mode)))

;;;; Toggle *

(defun mh/zen ()
  "Enables `zen-mode' with all its bells and whistles :)"
  (interactive)
  (setq-local show-trailing-whitespace
              (not show-trailing-whitespace))
  (display-fill-column-indicator-mode 'toggle)
  (display-line-numbers-mode 'toggle))

;;; Toggle Input Method
(global-set-key (kbd "C-x t i") 'toggle-input-method)

;;; Toggle Whitespace
(global-set-key (kbd "C-x t w")
                (lambda () (interactive)
                  (setq-local show-trailing-whitespace (not show-trailing-whitespace))))

;;; Toggle Truncation
(global-set-key (kbd "C-x t v")
                (lambda () (interactive)
                  (setq-local truncate-lines (not truncate-lines))))

;;; Toggle Line Numbers
(global-set-key (kbd "C-x t l")
                (lambda () (interactive)
                  (display-line-numbers-mode 'toggle)))

;;; Toggle Fill Column
(global-set-key (kbd "C-x t c")
                (lambda () (interactive)
                  (display-fill-column-indicator-mode 'toggle)))

;;; Toggle All
(global-set-key (kbd "C-x t s") 'mh/zen)
(global-set-key (kbd "C-x t z") (lambda () (interactive)
                                  (zen-mode 'toggle)
                                  (mh/zen)))

;;;; Scrolling

(defun mh/scroll-up (arg)
  "Pull up half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-up (/ (window-height) 2))
	(scroll-up arg))
    (error (beep 1)
	   (message "End of buffer")
	   (goto-char (point-max)))))


(defun mh/scroll-down (arg)
  "Pull down half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-down (/ (window-height) 2))
	(scroll-down arg))
    (error (beep 1)
	   (message "Beginning of buffer")
	   (goto-char (point-min)))))

(global-set-key (kbd "C-v")   'mh/scroll-up)
(global-set-key (kbd "M-v")   'mh/scroll-down)
(global-set-key (kbd "C-M-v") 'scroll-lock-mode)

;;;; Up-to-Char

(defun mh/up-to-char (arg char)
  "Points to char given by interactive `char'"
  (interactive "P\ncUp to char: ")
  (if (eq arg '-)
      (search-backward (char-to-string char) nil nil 1)
    (progn
      (search-forward (char-to-string char) nil nil 1)
      (backward-char))))

(defun mh/to-char (arg char)
  "Points over char given by interactive `char'"
  (interactive "P\ncTo char: ")
  (mh/up-to-char arg char)
  (if (eq arg '-)
      (backward-char)
    (forward-char)))

(global-set-key (kbd "M-z")   'mh/to-char)
(global-set-key (kbd "C-M-z") 'mh/up-to-char)
(global-set-key (kbd "M-Z")   'zap-up-to-char)

;;;; Formating

(defun mh/delete-space-after-point ()
  (interactive)
  (let ((point (point)))
    (delete-region
      point
      (progn
        (skip-chars-forward " \t")
	      (constrain-to-field nil point t)))))

(global-set-key (kbd "C-x C-M-\\") 'mh/delete-space-after-point)

;;;; Yanking

(defun mh/yank (times)
  (interactive "P")
  (if (numberp times)
      (progn
        (dotimes (i (abs times)) (yank)))
      (yank times)))

(global-set-key (kbd "C-y")   'mh/yank)
(global-set-key (kbd "C-M-y") 'yank)
