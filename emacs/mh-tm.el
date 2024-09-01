;;;; mh-tm.el --- Tree-sitter movement based on `elisp-tree-sitter'

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
;;  Small mode to move through syntactic nodes as if they were SEXPs

(require 'tree-sitter)

(defcustom mh/tm-use-region-as-mark nil
  "Descriminant for whether `tm' should interpret the region being active as
acting like a mark (t or nil), or whether calling the mode should start with the
mark ('always)")

(defcustom mh/tm-use-named? t
  "Flag for whether `tm' should call the `named' variant of tree-sitters
functions. This variant makes movement more akin to an AST, instead of a CST.
Run the `mh/tm-up-funcs' after changing this variable during run-time")

;;; internals

(defvar mh/tm-cache-node nil
  "Cache value for the current node")

(defvar mh/tm-mark? nil
  "Flag for whether we should be using a mark to create a region")

;;;;

(defvar mh/tm-sel-child #'tsc-get-nth-child)
(defvar mh/tm-sel-next-sib #'tsc-get-next-sibbling)
(defvar mh/tm-sel-prev-sib #'tsc-get-prev-sibbling)

(defun mh/tm-up-funcs ()
  (if mh/tm-use-named?
      (progn
        (setq mh/tm-sel-child #'tsc-get-nth-named-child
              mh/tm-sel-next-sib #'tsc-get-next-named-sibling
              mh/tm-sel-prev-sib #'tsc-get-prev-named-sibling))
      (progn
        (setq mh/tm-sel-child #'tsc-get-nth-child
              mh/tm-sel-next-sib #'tsc-get-next-sibling
              mh/tm-sel-prev-sib #'tsc-get-prev-sibling))))

(mh/tm-up-funcs)

;;;

(defun mh/tm-select-parent ()
  (when-let ((node (tsc-get-parent mh/tm-cache-node)))
    (setq mh/tm-cache-node node)))

(defun mh/tm-select-child ()
  (when-let ((node (apply mh/tm-sel-child (list mh/tm-cache-node 0))))
    (setq mh/tm-cache-node node)))

(defun mh/tm-select-next-sib ()
  (when-let ((node (apply mh/tm-sel-next-sib (list mh/tm-cache-node))))
    (setq mh/tm-cache-node node)))

(defun mh/tm-select-prev-sib ()
  (when-let ((node (apply mh/tm-sel-prev-sib (list mh/tm-cache-node))))
    (setq mh/tm-cache-node node)))

;;;

(defun mh/tm-goto ()
  (goto-char (tsc-node-start-byte mh/tm-cache-node)))

(defun mh/tm-mark ()
  (let ((rng (tsc-node-byte-range mh/tm-cache-node))
        (pt (point)))
    (goto-char (car rng))
    (set-mark (point))
    (goto-char (cdr rng))
    (exchange-point-and-mark)))

(defun mh/tm-after ()
  (if (or mh/tm-mark?
          (and (or (and (eq mh/tm-use-region-as-mark t) (region-active-p))
                   (eq mh/tm-use-region-as-mark 'always))
            (setq mh/tm-mark? t)))
      (mh/tm-mark)
      (mh/tm-goto)))

;;;

(defun mh/tm-select-this-cmd () (interactive)
  (mh/tm-after))

(defun mh/tm-select-parent-cmd () (interactive)
  (mh/tm-select-parent)
  (mh/tm-after))

(defun mh/tm-select-child-cmd () (interactive)
  (mh/tm-select-child)
  (mh/tm-after))

(defun mh/tm-select-next-sib-cmd () (interactive)
  (mh/tm-select-next-sib)
  (mh/tm-after))

(defun mh/tm-select-prev-sib-cmd () (interactive)
  (mh/tm-select-prev-sib)
  (mh/tm-after))

(defun mh/tm-set-mark-cmd () (interactive)
  (if (setq mh/tm-mark? (not mh/tm-mark?))
    (mh/tm-mark)
    (pop-mark)))

;;;

(defun mh/tm-clean-up (&optional map)
  (setq mh/tm-cache-node nil mh/tm-mark? nil))

(defun mh/tm-echo ()
  (tsc-node-to-sexp mh/tm-cache-node))

(defun mh/tm-set-node (&rest force)
  (if (or (not mh/tm-cache-node) (and (listp force) (car force)))
      (setq mh/tm-cache-node (tree-sitter-node-at-pos)))
      mh/tm-cache-node)

;;;

(mh/set-mode-map-rep "mh/tm" "C-x ,"
  '(("SPC" . #'mh/tm-set-mark-cmd)

    ("," . #'mh/tm-select-this-cmd)
    ("u" . #'mh/tm-select-parent-cmd)
    ("d" . #'mh/tm-select-child-cmd)
    ("f" . #'mh/tm-select-next-sib-cmd)
    ("n" . #'mh/tm-select-next-sib-cmd)
    ("b" . #'mh/tm-select-prev-sib-cmd)
    ("p" . #'mh/tm-select-prev-sib-cmd))

  nil #'mh/tm-set-node #'mh/tm-clean-up)
