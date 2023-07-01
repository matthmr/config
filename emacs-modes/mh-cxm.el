;;;; mh-cmx.el --- Very small minor mode that makes `hjkl' work like VI keys

;; Copyright (C) 2023 mH

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
;;  Very small minor mode that makes `hjkl' work like VI keys

;; Ignore any self-inserting keybindings that are not bound to cursor movements
(defvar cxm-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    (define-key map "J" 'mh/scroll-up)
    (define-key map "K" 'mh/scroll-down)
    (define-key map "L" 'recenter-top-bottom)
    (define-key map "0" 'beginning-of-line)
    (define-key map "$" 'end-of-line)
    (define-key map "-" 'end-of-line)
    (define-key map "{" 'forward-paragraph)
    (define-key map "}" 'backward-paragraph)
    (define-key map " " 'set-mark-command)
    (define-key map "i" 'cxm-mode)
    (define-key map "m" 'cxm-mode)
    map))

(define-minor-mode cxm-mode
  "This mode will make `hjkl' act like VI keys. Useful for one-handed
scrolling"
  :lighter " cxm"
  :global t
  :keymap cxm-mode-map)
