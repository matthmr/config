;;;; mh-ediff-merge.el --- Custom layer atop ediff to merge with git-merge.sh

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
;;  Custom layer atop ediff to merge with git-merge.sh

(require 'ediff)
(require 'vc)

;; From `vc/vc-git.el' (vc-git-find-revision)
(defun mh/git-show-cmd (id buffer)
  "Calls Git to populate BUFFER with blob ID"
  (let* (process-file-side-effects
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary))
    (vc-git-command buffer 0 nil "cat-file" "blob" id)))

;; From `vc/vc.el' (vc-find-revision-no-save)
(defun mh/git-show (id buffer) ; file revision &optional backend buffer
  "Calls `mh/git-show-cmd', and returns the resultant buffer"
  (mh/git-show-cmd id buffer)

  (with-current-buffer buffer
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)

    ;; For non-interactive, skip any questions
    (let ((enable-local-variables :safe) ;; to find `mode:'
          (buffer-file-name nil))
      ;; Don't run hooks that might assume buffer-file-name
      ;; really associates buffer with a file (bug#39190).
      (ignore-errors (delay-mode-hooks (set-auto-mode)))))

  (setq-local vc-parent-buffer nil)
  buffer)

;; From `vc/vc.el' (vc-revision-other-window)
(defun mh/git-show-other-window (id file)
  "Calls `mh/git-show' in another window"
  (let ((buffer (get-buffer id)))
    ;; clone the buffer, instead of using the same
    (if buffer
      (progn
        (setq buffer (clone-indirect-buffer id nil))
        (with-current-buffer buffer (erase-buffer)))
      (setq buffer (create-file-buffer id)))

    (set-buffer buffer)
    (let ((buffer-file-name file))
      (vc-ensure-vc-buffer))
    (switch-to-buffer-other-window (mh/git-show id buffer))))

;; From `vc/ediff-vers.el' (ediff-vc-merge-internal)
(defun mh/ediff-merge-internal (ours-id theirs-id base-id file)
  "Internal merge function. Given their IDs and the resultant merge FILE"
  (let (buf1 buf2 ancestor-buf)
    (save-window-excursion
      ;; ours-id buffer
      (save-excursion
        (mh/git-show-other-window ours-id file)
        (setq buf1 (current-buffer)))

      ;; theirs-id buffer
      (save-excursion
        (mh/git-show-other-window theirs-id file)
        (setq buf2 (current-buffer)))

      ;; base-id buffer
      (when base-id
        (save-excursion
          (mh/git-show-other-window base-id file)
          (setq ancestor-buf (current-buffer)))
       ))

    ;; call to merge
    (if base-id
        (ediff-merge-buffers-with-ancestor
         buf1 buf2 ancestor-buf
         nil 'ediff-merge-revisions-with-ancestor file)
        (ediff-merge-buffers
         buf1 buf2 nil 'ediff-merge-revisions file))
    ))

;;;; Main

(defun mh/ediff-merge3 (ours-id theirs-id base-id file)
  "3-way merge of FILE given their IDs"
  (mh/ediff-merge-internal ours-id theirs-id base-id file))

(defun mh/ediff-merge2 (ours-id theirs-id base-id file)
  "2-way merge of FILE given their IDs. `base-id' is ignored, only kept for
   compatibility with `mh/ediff-merge3'"
  (mh/ediff-merge-internal ours-id theirs-id nil file))

(provide 'mh-ediff-merge)
