;;;; mh-ep.el --- very crude `popon' integration with `eldoc'

;; Copyright (C) 2024-2025 mh

;; Author: mh <github.com/matthmr>
;; Version: 1.2.0

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

(defvar mh/ep-lines 0)

(defun mh/ep-format-buffer (ep-buffer w h)
  "Format the eldoc buffer's contents to fit within the bounds"
  (with-current-buffer ep-buffer
    (let ((lines (string-split (buffer-string)
                               "\n"))
          (output* (list)))

      (setq mh/ep-lines (length lines))
      (dolist (line lines)
        (if (> (length line) w)
          ;; lines too big: break them down
          (let ((nlines (string-split
                         (string-fill line w) "\n")))
            (setf mh/ep-lines (+ mh/ep-lines (- (length nlines) 1)))
            (dolist (nline nlines)
              (push (string-pad nline w ? ) output*)))

          ;; push line
          (push (string-pad line w ? ) output*))
        )

      (let ((output (string-join (reverse output*) "\n")))
        ;; set background to `brightblack'
        (add-face-text-property 0 (length output)
          '(:background "color-19") t output)
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
    (let* ((w-pos (popon-x-y-at-pos (point)))
           (w-width (window-body-width))
           (w-height (window-body-height))

           ;; at most 3/4 of the width, 1/2 of the height
           (t-width (round (* 0.66 w-width)))
           (t-height (/ w-height 2))
           (text (mh/ep-format-buffer ep-buffer t-width t-height))

           ;; expected border on each side of the popon
           (p-bord (/ (- w-width t-width) 2))
           (px-off (if (> (1+ (car w-pos)) p-bord)
                     (- (car w-pos) p-bord)
                     0))
           (py-off (if (> (+ (1+ (cdr w-pos)) mh/ep-lines) w-height)
                      mh/ep-lines
                      -1))

           (p-pos (progn (setcar w-pos (- (car w-pos) px-off))
                         (setcdr w-pos (- (cdr w-pos) py-off))
                          w-pos))
           )
      (popon-create text p-pos)
      ))
  nil)

;;;; Externals

;; From `emacs-lisp/eldoc.el' (`eldoc-display-in-buffer')
(defun mh/ep-display (docs _interactive)
  "Eldoc display-function to call `popon-create' with the results"
  (mh/ep-with-buffer (eldoc--format-doc-buffer docs))
  (add-hook 'post-command-hook #'mh/ep-kill))

;; TODO: not needed?
;; From `emacs-lisp/eldoc.el' (`turn-on-eldoc-mode')
(defun mh/ep-turn-on-eldoc-mode ()
  "Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail."
  (when (and (not (minibufferp)) (eldoc--supported-p))
    (eldoc-mode 1)))

(defun mh/ep-setup (display)
  "Setup a popon frontend to work directly with `eldoc'"
  (push display eldoc-display-functions)
  ;; (defalias 'turn-on-eldoc-mode #'mh/turn-on-eldoc-mode)
  )

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
