;;;; mh-ep.el --- very crude `popon' integration with `eldoc'

;; Copyright (C) 2024 mH

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
;; Just a very crude `popon' integration with `eldoc', mainly to be used with
;; `eglot' (see `eglot--highlights')

(require 'popon)
(require 'eldoc)

;;;; Internals

(defvar mh/ep-popon nil
  "The popon we must delete")

(defun mh/ep-format-buffer (ep-buffer w h)
  "Format the eldoc buffer's contents to fit within the bounds"
  (with-current-buffer ep-buffer
    (let ((lines (string-split (buffer-string)
                               "\n"))
          (output* (list)))

      (dolist (line lines)
        (if (> (length line) w)
          ;; lines too big: break them down
          (let ((nlines (string-split
                         (string-fill line width) "\n")))
            (dolist (nline nlines)
              (push (string-pad nline w ? ) output*)))

          ;; push line
          (push (string-pad line w ? ) output*))
        )

      (let ((output (string-join (reverse output*) "\n")))
        ;; set background to `brightblack'
        (add-face-text-property 0 (length output)
          '(:background "brightblack") t output)
        output)
      )))

(defun mh/ep-kill ()
  "Kill the current active popon"
  (popon-kill mh/ep-popon)
  (setq mh/ep-popon nil)
  ;; (message "killed by %s" this-command)
  (remove-hook 'post-command-hook #'mh/ep-kill)
  )

(defun mh/ep-with-buffer (ep-buffer)
  "Call popon given the buffer"
  (setq mh/ep-popon
    (let* ((pos (popon-x-y-at-pos (point)))
           (width (- (/ (window-width) 2) 3))
           (height (/ (window-height) 2))
           (text (mh/ep-format-buffer ep-buffer width height)))
      (popon-create text (progn (setcdr pos (1+ (cdr pos))) pos))
      ))
  nil)

;;;; Externals

;; From `emacs-lisp/eldoc.el' (`eldoc-display-in-buffer')
(defun mh/ep-display (docs _interactive)
  "Eldoc display-function to call `popon-create' with the results"
  (mh/ep-with-buffer (eldoc--format-doc-buffer docs))
  (add-hook 'post-command-hook #'mh/ep-kill))

;; From `emacs-lisp/eldoc.el' (`turn-on-eldoc-mode')
(defun mh/ep-turn-on-eldoc-mode ()
  "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail."
  (when (and (not (minibufferp)) (eldoc--supported-p))
    (eldoc-mode 1)))

(defun mh/ep-setup (display)
  "Setup a popon frontend to work directly with `eldoc'"
  (push display eldoc-display-functions)
  (defalias 'turn-on-eldoc-mode #'mh/turn-on-eldoc-mode))

;;;; Main

(defun mh/ep ()
  "Main popon function"
  (interactive)
  (mh/ep-with-buffer (eldoc-doc-buffer)))

(defun mh/ep-mode (load)
  (unless load
    ;; TODO: undo the `turn-on-eldoc-mode' aliasing
    (pop eldoc-display-functions)))

(mh/provide 'ep #'mh/ep-mode)
