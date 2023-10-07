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

(defun mh/with-prefix (prefix function)
  (when (eq current-prefix-arg nil)
    (setq current-prefix-arg prefix))
  (call-interactively function))

;;;; Windowing

(global-set-key (kbd "C-x C-M-o") (lambda () (interactive)
                                    (mh/with-prefix -1 'other-window)))

(global-set-key (kbd "C-x ^")   (lambda () (interactive)
                                  (mh/with-prefix 5 'enlarge-window)))
(global-set-key (kbd "C-x {")   (lambda () (interactive)
                                  (mh/with-prefix 5 'shrink-window-horizontally)))
(global-set-key (kbd "C-x }")   (lambda () (interactive)
                                  (mh/with-prefix 5 'enlarge-window-horizontally)))

;;;; Clipboard (aka `edit-copy')

(global-set-key (kbd "C-c C-x C-w")
                (lambda () (interactive)
                  (write-file @EMACS_CLIPBOARD@)
                  (shell-command @EMACS_EDIT_COPY_I@)))
(global-set-key (kbd "C-c C-x C-y")
                (lambda () (interactive)
                  (shell-command @EMACS_EDIT_COPY_D@)
                  (insert-file @EMACS_CLIPBOARD@)))
(global-set-key (kbd "C-c C-x e")
                (lambda () (interactive)
                  (find-file @EMACS_CLIPBOARD@)
                  (shell-command @EMACS_EDIT_COPY_D@)
                  (revert-buffer-quick)))

(global-set-key (kbd "C-c C-t C-w")
                (lambda () (interactive)
                  (call-interactively 'kill-region)
                  (when-let ((kill-top (car kill-ring)))
                    (shell-command (format "tmux set-buffer \"$(echo \"%s\")\""
                                           (substring-no-properties kill-top))
                                   nil nil))))
(global-set-key (kbd "C-c C-t M-w")
                (lambda () (interactive)
                  (call-interactively 'kill-ring-save)
                  (when-let ((kill-top (car kill-ring)))
                    (shell-command (format "tmux set-buffer \"$(echo \"%s\")\""
                                           (substring-no-properties kill-top))
                                   nil nil))))

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

(defun mh/undo-edit ()
  "'Undoes' editing configurations. Useful hook for non-editing buffer"
  (interactive)

  (if show-trailing-whitespace
      (setq-local show-trailing-whitespace nil))
  (if truncate-lines
      (setq-local truncate-lines nil))

  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1))

(defun mh/reset-edit ()
    "'Resets' editing configurations. Useful hook for editing buffer"
  (interactive)

  (unless show-trailing-whitespace
    (setq-local show-trailing-whitespace t))
  (unless truncate-lines
      (setq-local truncate-lines t))

  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1))

;;; Toggle Input Method
(global-set-key (kbd "C-x C-M-m C-M-i") 'toggle-input-method)

;;; Toggle Whitespace
(global-set-key (kbd "C-x C-M-m C-M-w")
                (lambda () (interactive)
                  (setq-local show-trailing-whitespace
                              (not show-trailing-whitespace))))

;;; Toggle Truncation
(global-set-key (kbd "C-x C-M-m C-M-s")
                (lambda () (interactive)
                  (setq-local truncate-lines (not truncate-lines))))

;;; Toggle Line Numbers
(global-set-key (kbd "C-x C-M-m C-M-l") 'display-line-numbers-mode)

;;; Toggle Fill Column
(global-set-key (kbd "C-x C-M-m C-M-c") 'display-fill-column-indicator-mode)

;;; Toggle All
(global-set-key (kbd "C-x C-M-m C-M-e") 'mh/undo-edit)
(global-set-key (kbd "C-x C-M-m C-M-r") 'mh/reset-edit)
(global-set-key (kbd "C-x C-M-m C-M-z") (lambda () (interactive)
                                  (mh/undo-edit)
                                  (zen-mode 'toggle)))

(defun mh/toggle-viper ()
  (interactive)
  (when (boundp 'viper-mode)
    (if viper-mode
        (progn
          (viper-go-away))
      (progn
        (viper-mode)
        (delq 'viper-mode-string global-mode-string)))))

(defun mh/toggle-cxm ()
  (interactive)
  (when (boundp 'cxm-mode)
    (cxm-mode)))

(global-set-key (kbd "C-x C-M-m C-M-v") 'mh/toggle-viper)
(global-set-key (kbd "C-x C-M-m C-M-m") 'mh/toggle-cxm)

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

(global-set-key (kbd "C-x <")   (lambda () (interactive)
                                  (mh/with-prefix 15 'scroll-right)))
(global-set-key (kbd "C-x >")   (lambda () (interactive)
                                  (mh/with-prefix 15 'scroll-left)))

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
(global-set-key (kbd "M-Z")   'zap-up-to-char)

(global-set-key (kbd "C-M-z") 'repeat-complex-command)

;;;; Formating

(defun mh/delete-space-after-point ()
  (interactive)
  (let ((point (point)))
    (delete-region
      point
      (progn
        (skip-chars-forward " \t")
	      (constrain-to-field nil point t)))))

(defun mh/kill-line-before-point ()
  "Kills the line before the point"
  (interactive)
  (if (eq current-prefix-arg nil)
      (setq current-prefix-arg 0))
  (call-interactively 'kill-line))

(defun mh/backward-kill-line ()
  "Kills the line backward"
  (interactive)
  (set-mark (point))
  (beginning-of-line)
  (call-interactively 'kill-region))

(global-set-key (kbd "C-x C-M-\\") 'mh/delete-space-after-point)
(global-set-key (kbd "C-x C-M-h")  'mh/backward-kill-line)

;;;; Yanking

(defun mh/yank (times)
  (interactive "P")
  (if (numberp times)
      (progn
        (dotimes (i (abs times)) (yank)))
      (yank times)))

(global-set-key (kbd "C-y")   'mh/yank)
(global-set-key (kbd "C-M-y") 'yank)

;;;; Completion

(defun mh/minibuffer-completion (start end collection &optional predicate)
  (if (active-minibuffer-window)
      (completion--in-region start end collection predicate)
    (let* ((initial (buffer-substring-no-properties start end))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read
                            "Completion: " collection predicate t initial)))))
      (cond (completion (completion--replace start end completion) t)
            (t (message "No completion") nil)))))

(setq completion-in-region-function #'mh/minibuffer-completion)

;;;; Mark

(defun mh/pop-to-mark-forward ()
  "Go back to the last popped mark"
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))) )
