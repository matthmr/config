;;;; mh-tm.el --- Tree-sitter movement based on `elisp-tree-sitter'

;; Copyright (C) 2024-2025 mh

;; Author: mh <github.com/matthmr>
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

(defcustom mh/tm-echo nil
  "Flag for whether `tm' should echo the currently selected node")

;;; internals

(defvar mh/tm-node nil
  "Cache value for the current node")

(defvar mh/tm-mark? nil
  "Flag for whether we should be using a mark to create a region")

(defvar mh/tm-kill-yourself nil
  "Should you kill yourself?")

;;;;

(defvar mh/tm-sel-child #'tsc-get-nth-child)
(defvar mh/tm-sel-next-sib #'tsc-get-next-sibbling)
(defvar mh/tm-sel-prev-sib #'tsc-get-prev-sibbling)
(defvar mh/tm-count #'tsc-count-children)

(defun mh/tm-up-funcs ()
  (if mh/tm-use-named?
      (progn
        (setq mh/tm-sel-child #'tsc-get-nth-named-child
              mh/tm-sel-next-sib #'tsc-get-next-named-sibling
              mh/tm-sel-prev-sib #'tsc-get-prev-named-sibling
              mh/tm-count #'tsc-count-named-children))
      (progn
        (setq mh/tm-sel-child #'tsc-get-nth-child
              mh/tm-sel-next-sib #'tsc-get-next-sibling
              mh/tm-sel-prev-sib #'tsc-get-prev-sibling
              mh/tm-count #'tsc-count-children))))

(mh/tm-up-funcs)

;;; these functions change the value of the node

(defun mh/tm-select-parent ()
  (when-let ((node (tsc-get-parent mh/tm-node)))
    (setq mh/tm-node node)))

(defun mh/tm-select-child ()
  (when-let ((node (apply mh/tm-sel-child (list mh/tm-node 0))))
    (setq mh/tm-node node)))

(defun mh/tm-select-next-sib ()
  (when-let ((node (apply mh/tm-sel-next-sib (list mh/tm-node))))
    (setq mh/tm-node node)))

(defun mh/tm-select-prev-sib ()
  (when-let ((node (apply mh/tm-sel-prev-sib (list mh/tm-node))))
    (setq mh/tm-node node)))

(defun mh/tm-select-beg ()
  (when-let ((node (mh/tm-select-parent)))
    (setq mh/tm-node
      (apply mh/tm-sel-child (list node 0))
      )))

(defun mh/tm-select-end ()
  (when-let* ((node (mh/tm-select-parent))
              (node-n (apply mh/tm-count (list node))))
    (if (not (zerop node-n))
      (setq mh/tm-node
        (setq m/node
          (apply mh/tm-sel-child (list node (- node-n 1))))
          ))))

;;;

(defun mh/tm-goto ()
  "Goto the start of the current tree-sitter node"
  (goto-char (tsc-node-start-byte mh/tm-node)))

(defun mh/tm-mark ()
  "Create a region around the whole current tree-sitter node"
  (let ((rng (tsc-node-byte-range mh/tm-node))
        (pt (point)))
    (goto-char (car rng))
    (set-mark (point))
    (goto-char (cdr rng))
    (exchange-point-and-mark)))

(defun mh/tm-after ()
  "Operations to do after the tree-sitter cursor has issue to move: mark, goto"
  (if (or mh/tm-mark?
          (and (or (and (eq mh/tm-use-region-as-mark t) (region-active-p))
                   (eq mh/tm-use-region-as-mark 'always))
            (setq mh/tm-mark? t)))
      (mh/tm-mark)
      (mh/tm-goto))
  (when mh/tm-echo
    (message (tsc-node-text mh/tm-node))))

;;;

(defun mh/tm-select-this-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-after))

(defun mh/tm-mark-this-cmd ()
  (interactive)
  (mh/tm-set-node)
  (let ((mh/tm-mark? t))
    (mh/tm-after)))

(defun mh/tm-select-parent-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-parent)
  (mh/tm-after))

(defun mh/tm-select-child-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-child)
  (mh/tm-after))

(defun mh/tm-select-next-sib-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-next-sib)
  (mh/tm-after))

(defun mh/tm-select-prev-sib-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-prev-sib)
  (mh/tm-after))

(defun mh/tm-set-mark-cmd ()
  (interactive)
  (mh/tm-set-node)
  (if (setq mh/tm-mark? (not mh/tm-mark?))
    (mh/tm-mark)
    (pop-mark)))

(defun mh/tm-select-beg-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-beg)
  (mh/tm-after))

(defun mh/tm-select-end-cmd ()
  (interactive)
  (mh/tm-set-node)
  (mh/tm-select-end)
  (mh/tm-after))

;;;

(defun mh/tm-raise-cmd ()
  (interactive)
  (mh/tm-set-node)

  (let* ((this-node mh/tm-node)
         (this-bounds (tsc-node-byte-range this-node)))
    (kill-ring-save (car this-bounds) (cdr this-bounds))

    (when (mh/tm-select-parent)
      (let ((next-bounds (tsc-node-byte-range mh/tm-node)))
        (kill-region (car next-bounds) (cdr next-bounds))
      )
      (yank 2))
    ))

(defun mh/tm-raise-and-kill-cmd ()
  (interactive)
  (mh/tm-set-node)

  (let ((this-text
          (buffer-substring-no-properties
            (region-beginning) (region-end))))
    (call-interactively #'mh/tm-select-parent-cmd)
    (insert this-text)
    (exchange-point-and-mark)
    (kill-region (region-beginning) (region-end))))

(defun mh/tm-swap-with-next-sib-cmd ()
  (interactive)
  (mh/tm-set-node)

  (let* ((this-node mh/tm-node)
         (this-bounds (tsc-node-byte-range this-node))
         (this-len (- (cdr this-bounds) (car this-bounds))))

    (when (mh/tm-select-next-sib)
      (let ((next-bounds (tsc-node-byte-range mh/tm-node)))
        (kill-ring-save (car this-bounds) (cdr this-bounds))

        (save-excursion
          (goto-char (car next-bounds))
          (kill-region (car next-bounds) (cdr next-bounds))
          (yank 2))

        ;; end up in ours
        (kill-region (car this-bounds) (cdr this-bounds))
        (yank 2)

        (goto-char (- (cdr next-bounds) this-len))
        )
      (mh/tm-set-node t))
    ))

(defun mh/tm-swap-with-prev-sib-cmd ()
  (interactive)
  (mh/tm-set-node t)

  (let* ((this-node mh/tm-node)
         (this-bounds (tsc-node-byte-range this-node)))

    (when (mh/tm-select-prev-sib)
      (let ((prev-bounds (tsc-node-byte-range mh/tm-node)))
        (kill-ring-save (car prev-bounds) (cdr prev-bounds))
        (kill-region (car this-bounds) (cdr this-bounds))
        (yank 2)

        (goto-char (car prev-bounds))
        (kill-region (car prev-bounds) (cdr prev-bounds))
        (yank 2)
        (goto-char (car prev-bounds)))
      (mh/tm-set-node t))
    ))

;;;

(defun mh/tm-clean-up ()
  (setq mh/tm-node nil mh/tm-mark? nil))

(defun mh/tm-echo ()
  (tsc-node-to-sexp mh/tm-node))

(defun mh/tm-move-clean-up ()
  (when (not (string-prefix-p "mh/tm-" (symbol-name last-command)))
    (mh/tm-clean-up)
    (setq mh/tm-kill-yourself nil)
    (pop post-command-hook)))

(defun mh/tm-set-node (&rest force)
  (when (and mh/tm-move-mode (not mh/tm-kill-yourself))
    (setq mh/tm-kill-yourself t)
    (push #'mh/tm-move-clean-up post-command-hook)
    )

  (when (or (not mh/tm-node) (and (listp force) (car force)))
    (setq mh/tm-node (tree-sitter-node-at-pos)))
    mh/tm-node)

;;;

(mh/set-mode-map-rep "mh/tm" "C-x ,"
  '(("SPC" . #'mh/tm-set-mark-cmd)

    ("," . #'mh/tm-select-this-cmd)
    ("u" . #'mh/tm-select-parent-cmd)
    ("d" . #'mh/tm-select-child-cmd)
    ("f" . #'mh/tm-select-next-sib-cmd)
    ("n" . #'mh/tm-select-next-sib-cmd)
    ("b" . #'mh/tm-select-prev-sib-cmd)
    ("p" . #'mh/tm-select-prev-sib-cmd)

    ("U" . #'mh/tm-raise-cmd)

    ("N" . #'mh/tm-swap-with-next-sib-cmd)
    ("F" . #'mh/tm-swap-with-next-sib-cmd)
    ("P" . #'mh/tm-swap-with-prev-sib-cmd)
    ("B" . #'mh/tm-swap-with-prev-sib-cmd))

  nil #'mh/tm-set-node #'mh/tm-clean-up)

(defvar mh/tm-move-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-M-@" #'mh/tm-mark-this-cmd)

    (keymap-set map "C-M-a" #'mh/tm-select-beg-cmd)
    (keymap-set map "C-M-e" #'mh/tm-select-end-cmd)

    (keymap-set map "C-M-n" #'mh/tm-select-next-sib-cmd)
    (keymap-set map "C-M-p" #'mh/tm-select-prev-sib-cmd)

    (keymap-set map "C-M-f" #'mh/tm-select-next-sib-cmd)
    (keymap-set map "C-M-b" #'mh/tm-select-prev-sib-cmd)

    (keymap-set map "C-M-u" #'mh/tm-select-parent-cmd)
    (keymap-set map "C-M-d" #'mh/tm-select-child-cmd)

    (keymap-set map "C-x C-M-u" #'mh/tm-raise-cmd)
    ;; (keymap-set map "C-x M-U" #'mh/tm-raise-and-kill-cmd)

    (keymap-set map "M-N" #'mh/tm-swap-with-next-sib-cmd)
    (keymap-set map "M-P" #'mh/tm-swap-with-prev-sib-cmd)

    (keymap-set map "M-F" #'mh/tm-swap-with-next-sib-cmd)
    (keymap-set map "M-B" #'mh/tm-swap-with-prev-sib-cmd)

    map)
  "mh/tm-move-mode SEXP movement-like bindings")

(define-minor-mode mh/tm-move-mode
  "SEXP movements bindings operate using tree-sitter"
  :lighter " ()")

(defvar mh/save mh/tm-use-region-as-mark)

(defun mh/tm-move (load)
  (if load
    (progn
      (setq mh/tm-use-region-as-mark nil)
      (add-hook 'prog-mode-hook #'mh/tm-move-mode))
    (progn
      (setq mh/tm-use-region-as-mark mh/save)
      (remove-hook 'prog-mode-hook #'mh/tm-move-mode))))

(defun mh/tm-echo (load)
  (setq mh/tm-echo load))

(global-set-key (kbd "C-x C-M-m C-M-a") #'mh/tm-move-mode)

(mh/provide 'tm-mov #'mh/tm-move t)
(mh/provide 'tm-echo #'mh/tm-echo t)
