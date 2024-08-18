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

(mh/load "kbind")

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

;;;; Whitespace

(defun mh/whitespace-setup-display ()
  "Sets up the display table `table' with the value of `mh/display'"
  (when-let ((table (or whitespace-display-table buffer-display-table)))
    (dolist (e mh/display)
      (set-display-table-slot table (car e) (cdr e)))))

(with-eval-after-load "whitespace"
  (setq-default whitespace-space-regexp "\\( +$\\)")
  (setq-default whitespace-style
                '(face empty spaces tabs tab-mark))

  (defface mh/whitespace-tab
    '((t :foreground "brightblack"))
    "Face used to visualize TAB.")
  (defface mh/whitespace-space
    '((t :background "brightblack"))
    "Face used to visualize SPACE.")

  (setq-default whitespace-tab 'mh/whitespace-tab)
  (setq-default whitespace-space 'mh/whitespace-space)

  (setq-default whitespace-display-mappings
    '((tab-mark ?\x09 [?¦ ?	] [?> ?	])
      (space-mark ?\ [?·] [?.])
      (space-mark ?\xA0 [?¤] [?_])
      (newline-mark ?\n [?↵ ?\n] [?$ ?\n])
      ))

  ;; From `whitespace.el'
  (defun whitespace-turn-on ()
    "Turn on whitespace visualization."
    ;; prepare local hooks
    (add-hook 'write-file-functions 'whitespace-write-file-hook nil t)
    ;; create whitespace local buffer environment
    (setq-local whitespace-font-lock-keywords nil)
    (setq-local whitespace-display-table nil)
    (setq-local whitespace-display-table-was-local nil)
    (setq-local whitespace-active-style
                (if (listp whitespace-style)
                    whitespace-style
                    (list whitespace-style)))
    ;; turn on whitespace
    (when whitespace-active-style
      (whitespace-color-on)
      (whitespace-display-char-on))
    (mh/whitespace-setup-display)))

;;;; Function overrides

(with-eval-after-load "ediff"
  (add-hook 'ediff-mode-hook
    (lambda ()
      (setq ediff-highlighting-style 'face
            ediff-auto-refine 'on)))

  (add-hook 'ediff-quit-merge-hook
    ;; For git. The `glob' gets deleted as soon as Emacs launches Ediff. So we
    ;; save in whichever file exists still
    (lambda ()
      (setq ediff-merge-store-file
            (or ediff-merge-store-file
                (let ((file-A (buffer-file-name ediff-buffer-A))
                      (file-B (buffer-file-name ediff-buffer-B)))
                  (cond ((file-exists-p file-A) file-A)
                        ((file-exists-p file-B) file-B)
                        (t (read-file-name "Write file: ")))
                  )))
      ))

  ;;; NOTE: I don't know for what fucking reason this function doesn't have this
  ;;; defined when it loads, but it is what is
  ;; From `vc/ediff-init.el'
  (eval-when-compile
    (defmacro ediff-with-current-buffer (buffer &rest body)
      "Evaluate BODY in BUFFER."
      (declare (indent 1) (debug (form body)))
      `(if (ediff-buffer-live-p ,buffer)
           (save-current-buffer
       (set-buffer ,buffer)
       ,@body)
         (or (eq this-command 'ediff-quit)
       (error ediff-KILLED-VITAL-BUFFER))
         )))

  ;; From `vc/ediff-util.el'
  (defun mh/ediff-write-merge-buffer-and-maybe-kill
      (buf file &optional show-file save-and-continue)
    (ediff-with-current-buffer buf
      (if (or (not (file-exists-p file))
        (y-or-n-p (format "File %s exists, overwrite? " file)))
    (progn
      ;;(write-region nil nil file)
      (ediff-with-current-buffer buf
        (set-visited-file-name file)
        (save-buffer))
      (if show-file
          (progn
      (message "Merge buffer saved in: %s" file)
      (set-buffer-modified-p nil)
      (sit-for 3)))
      (if (and
           (not save-and-continue)
           (y-or-n-p "Merge buffer saved.  Now kill the buffer? "))
          (ediff-kill-buffer-carefully buf))))))

  ;; From `vc/ediff-util.el'
  (defun mh/ediff-swap-buffers ()
  "Rotate the display of buffers A, B, and C."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (and (window-live-p ediff-window-A) (window-live-p ediff-window-B))
      (let ((buf ediff-buffer-A)
	    (values ediff-buffer-values-orig-A)
	    (diff-vec ediff-difference-vector-A)
	    (hide-regexp ediff-regexp-hide-A)
	    (focus-regexp ediff-regexp-focus-A)
	    (wide-visibility-p (eq ediff-visible-bounds ediff-wide-bounds))
	    (overlay (if (ediff-has-face-support-p)
			 ediff-current-diff-overlay-A))
	    (face (if (ediff-has-face-support-p)
			 ediff-current-diff-face-A))
	    (fine (if (ediff-has-face-support-p)
			 ediff-fine-diff-face-A)))
	(if ediff-3way-comparison-job
	    (progn
	      (set-window-buffer ediff-window-A ediff-buffer-C)
	      (set-window-buffer ediff-window-B ediff-buffer-A)
	      (set-window-buffer ediff-window-C ediff-buffer-B)
	      )
	  (set-window-buffer ediff-window-A ediff-buffer-B)
	  (set-window-buffer ediff-window-B ediff-buffer-A))
	;; swap diff buffers
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-A ediff-buffer-C
		  ediff-buffer-C ediff-buffer-B
		  ediff-buffer-B buf)
	  (setq ediff-buffer-A ediff-buffer-B
		ediff-buffer-B buf))

	;; swap saved buffer characteristics
	(if ediff-3way-comparison-job
	    (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-C
		  ediff-buffer-values-orig-C ediff-buffer-values-orig-B
		  ediff-buffer-values-orig-B values)
	  (setq ediff-buffer-values-orig-A ediff-buffer-values-orig-B
		ediff-buffer-values-orig-B values))

	;; swap diff vectors
	(if ediff-3way-comparison-job
	    (setq ediff-difference-vector-A ediff-difference-vector-C
		  ediff-difference-vector-C ediff-difference-vector-B
		  ediff-difference-vector-B diff-vec)
	  (setq ediff-difference-vector-A ediff-difference-vector-B
		ediff-difference-vector-B diff-vec))

	;; swap hide/focus regexp
	(if ediff-3way-comparison-job
	    (setq ediff-regexp-hide-A ediff-regexp-hide-C
		  ediff-regexp-hide-C ediff-regexp-hide-B
		  ediff-regexp-hide-B hide-regexp
		  ediff-regexp-focus-A ediff-regexp-focus-C
		  ediff-regexp-focus-C ediff-regexp-focus-B
		  ediff-regexp-focus-B focus-regexp)
	  (setq ediff-regexp-hide-A ediff-regexp-hide-B
		ediff-regexp-hide-B hide-regexp
		ediff-regexp-focus-A ediff-regexp-focus-B
		ediff-regexp-focus-B focus-regexp))

	;; The following is needed for XEmacs, since there one can't move
	;; overlay to another buffer.  In Emacs, this swap is redundant.
	(when ediff-3way-comparison-job
	  (setq ediff-current-diff-overlay-A ediff-current-diff-overlay-B
		ediff-current-diff-overlay-B overlay)
	  (setq ediff-current-diff-face-A ediff-current-diff-face-B
		ediff-current-diff-face-B face)
	  (setq ediff-fine-diff-face-A ediff-fine-diff-face-B
		ediff-fine-diff-face-B fine)
	  )

	;; swap wide bounds
	(setq ediff-wide-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 1 ediff-wide-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)
			   (nth 2 ediff-wide-bounds)))
		    (t
		     (list (nth 1 ediff-wide-bounds)
			   (nth 0 ediff-wide-bounds)))))
	;; swap narrow bounds
	(setq ediff-narrow-bounds
	      (cond (ediff-3way-comparison-job
		     (list (nth 2 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 1 ediff-narrow-bounds)))
		    (ediff-3way-job
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)
			   (nth 2 ediff-narrow-bounds)))
		    (t
		     (list (nth 1 ediff-narrow-bounds)
			   (nth 0 ediff-narrow-bounds)))))
	(if wide-visibility-p
	    (setq ediff-visible-bounds ediff-wide-bounds)
	  (setq ediff-visible-bounds ediff-narrow-bounds))
	))
  (if ediff-3way-job
      (ediff-set-state-of-all-diffs-in-all-buffers ediff-control-buffer))
  (ediff-recenter)
  )

  ;; From `vc/ediff-util.el'
  (defun mh/ediff-verify-file-merge-buffer (file)
    (let ((buff (if (stringp file) (find-buffer-visiting file)))
          warn-message)
      (or (null buff)
          (progn
            (setq warn-message
                  (format "Buffer %s is visiting %s. Revert the buffer? "
                          (buffer-name buff) file))
            (with-output-to-temp-buffer ediff-msg-buffer
              (princ "\n\n")
              (princ warn-message)
              (princ "\n\n"))
            (if (y-or-n-p
                 (message "%s" warn-message))
                (with-current-buffer buff
                  (revert-buffer))
              (warn "Changes will be overwritten"))))
      ))

  (defun mh/ediff-copy-ancestor-to-C (arg)
    "Choose ancestor buffer"
    (interactive "P")
    (ediff-copy-diff ediff-current-difference nil 'C nil
      (ediff-get-region-contents ediff-current-difference
        'Ancestor ediff-control-buffer)))

  (add-hook 'ediff-keymap-setup-hook
    (lambda ()
      (define-key ediff-mode-map "c" #'mh/ediff-copy-ancestor-to-C)))

  (fset #'ediff-swap-buffers
        #'mh/ediff-swap-buffers)
  (fset #'ediff-verify-file-merge-buffer
        #'mh/ediff-verify-file-merge-buffer)
  (fset #'ediff-write-merge-buffer-and-maybe-kill
        #'mh/ediff-write-merge-buffer-and-maybe-kill))

(with-eval-after-load "dabbrev"
  ;; From `dabbrev.el'
  (defun dabbrev-completion (&optional arg)
    "Completion on current word.
  Like \\[dabbrev-expand] but finds all expansions in the current buffer
  and presents suggestions for completion.

  With a prefix argument ARG, it searches all buffers accepted by the
  function pointed out by `dabbrev-friend-buffer-function' to find the
  completions.

  If the prefix argument is 16 (which comes from \\[universal-argument] \\[universal-argument]),
  then it searches *all* buffers."
    (interactive "*P")
    (dabbrev--reset-global-variables)
    ;; (setq dabbrev--check-other-buffers (and arg t))
    (setq dabbrev--check-all-buffers t)
          ;; (and arg (= (prefix-numeric-value arg) 16)))
    (let ((completion-at-point-functions '(dabbrev-capf)))
      (completion-at-point))))

(with-eval-after-load "eldoc"
  ;; From `emacs-lisp/eldoc.el'
  (defun eldoc--format-doc-buffer (docs)
    "Ensure DOCS are displayed in an *eldoc* buffer."
    (with-current-buffer (if (buffer-live-p eldoc--doc-buffer)
                             eldoc--doc-buffer
                           (setq eldoc--doc-buffer
                                 (get-buffer-create " *eldoc*")))
      (unless (eq docs eldoc--doc-buffer-docs)
        (setq-local eldoc--doc-buffer-docs docs)
        (let ((inhibit-read-only t)
              (things-reported-on))
          (special-mode)
          (mh/edit-buffer-n)
          (erase-buffer)
          (setq-local nobreak-char-display nil)
          (cl-loop for (docs . rest) on docs
                   for (this-doc . plist) = docs
                   for thing = (plist-get plist :thing)
                   when thing do
                   (cl-pushnew thing things-reported-on)
                   (setq this-doc
                         (concat
                          (propertize (format "%s" thing)
                                      'face (plist-get plist :face))
                          ": "
                          this-doc))
                   do (insert this-doc)
                   when rest do (insert "\n")
                   finally (goto-char (point-min)))
          ;; Rename the buffer, taking into account whether it was
          ;; hidden or not
          (rename-buffer (format "%s*eldoc%s*"
                                 (if (string-match "^ " (buffer-name)) " " "")
                                 (if things-reported-on
                                     (format " for %s"
                                             (mapconcat
                                              (lambda (s) (format "%s" s))
                                              things-reported-on
                                              ", "))
                                   ""))))))
    eldoc--doc-buffer))

;; From `minibuffer.el'
(defun completion-pcm--string->pattern (string &optional point)
  "Split STRING into a pattern.
A pattern is a list where each element is either a string
or a symbol, see `completion-pcm--merge-completions'."
  (if (and point (< point (length string)))
      (let ((prefix (substring string 0 point))
            (suffix (substring string point)))
        (append (completion-pcm--string->pattern prefix)
                '(point)
                (completion-pcm--string->pattern suffix)))
    (let* ((pattern nil)
           (p 0)
           (p0 p)
           (pending nil))

      (while (and (setq p (string-match completion-pcm--delim-wild-regex
                                        string p))
                  (or completion-pcm-complete-word-inserts-delimiters
                      ;; If the char was added by minibuffer-complete-word,
                      ;; then don't treat it as a delimiter, otherwise
                      ;; "M-x SPC" ends up inserting a "-" rather than listing
                      ;; all completions.
                      (not (get-text-property p 'completion-try-word string))))
        ;; Usually, completion-pcm--delim-wild-regex matches a delimiter,
        ;; meaning that something can be added *before* it, but it can also
        ;; match a prefix and postfix, in which case something can be added
        ;; in-between (e.g. match [[:lower:]][[:upper:]]).
        ;; This is determined by the presence of a submatch-1 which delimits
        ;; the prefix.
        (if (match-end 1) (setq p (match-end 1)))
        (unless (= p0 p)
          (if pending (push pending pattern))
          (push (substring string p0 p) pattern))
        (setq pending nil)
        (if (or (eq (aref string p) ?*)
                (eq (aref string p) ? ))
            (progn
              (push 'star pattern)
              (setq p0 (1+ p)))
          (push 'any pattern)
          (if (match-end 1)
              (setq p0 p)
            (push (substring string p (match-end 0)) pattern)
            ;; `any-delim' is used so that "a-b" also finds "array->beginning".
            (setq pending 'any-delim)
            (setq p0 (match-end 0))))
        (setq p p0))

      (when (> (length string) p0)
        (if pending (push pending pattern))
        (push (substring string p0) pattern))
      (nreverse pattern))))

;; From `window.el'
(defun mh/split-window (&optional _)
  (let ((window (selected-window)))
    (with-selected-window window
      (if (> (window-total-width window) (* 2.7 (window-total-height window)))
          (split-window-right)
        (split-window-below)))))

(defun mh/new-window ()
  "Creates a new window given the split"
  (interactive)
  (select-window (mh/split-window)))

(setq-default split-window-preferred-function 'mh/split-window)

(defun mh/erase-buffer ()
  "Erase buffer regardless"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;; Theme

(defvar base16-mh-colors)
(deftheme base16-mh)
