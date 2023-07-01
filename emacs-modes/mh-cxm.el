;;;; mh-cmx.el --- Very small minor mode that makes `hjkl' work like VI keys
;;                 and nothing else

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
;;  Very small minor mode that makes `hjkl' work like VI keys and nothing else

(define-minor-mode cxm-mode
  "This mode will make `hjkl' act like VI keys. Useful for one-handed
scrolling"
  :lighter " cxm"
  :keymap '(("h" . backward-char)
            ("j" . next-line)
            ("k" . previous-line)
            ("l" . forward-char)
            ("0" . beginning-of-line)
            ("$" . end-of-line)
            ("{" . forward-paragraph)
            ("}" . backward-paragraph)
            ("-" . end-of-line)
            (" " . set-mark-command)
            ("m" . cxm-mode)))
