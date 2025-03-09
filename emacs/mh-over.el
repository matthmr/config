;;;; Native Emacs functions overrides

;;;; Whitespace

(with-eval-after-load "whitespace"
  (defun mh/whitespace-setup-display ()
    "Sets up the display table `table' with the value of `mh/display-table' from
     `init'"
    (when-let ((table (or whitespace-display-table buffer-display-table)))
      (dolist (e mh/display-table)
        (set-display-table-slot table (car e) (cdr e)))))

;; From `whitespace.el'
;; Simply add `mh/whitespace-setup-display' at the end
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
  (mh/whitespace-setup-display))
)

;;;; Ediff

(with-eval-after-load "ediff"
  (eval-when-compile
;; From `vc/ediff-init.el'
;; NONE
(defmacro ediff-with-current-buffer (buffer &rest body)
  "Evaluate BODY in BUFFER."
  (declare (indent 1) (debug (form body)))
  `(if (ediff-buffer-live-p ,buffer)
       (save-current-buffer
	 (set-buffer ,buffer)
	 ,@body)
     (or (eq this-command 'ediff-quit)
	 (error ediff-KILLED-VITAL-BUFFER))
     ))
    )

;; From `vc/ediff-util.el'
;; Cut off the beginning of this function to suppress a warn message
(defun mh/ediff-write-merge-buffer-and-maybe-kill (buf file
					       &optional
					       show-file save-and-continue)
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
;; Sets up a correct swap of colors for `ediff's buffers
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
	(when (and (ediff-has-face-support-p) ediff-3way-comparison-job)
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
;; Revert the buffer, instead of saving and killing, and warn instead of erroing
(defun mh/ediff-verify-file-merge-buffer (file)
  (let ((buff (if (stringp file) (find-buffer-visiting file)))
	warn-message)
    (or (null buff)
	(progn
	  (setq warn-message
		(format "Buffer %s is visiting %s. Save and kill the buffer? "
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

  (fset #'ediff-swap-buffers
        #'mh/ediff-swap-buffers)
  (fset #'ediff-verify-file-merge-buffer
        #'mh/ediff-verify-file-merge-buffer)
  (fset #'ediff-write-merge-buffer-and-maybe-kill
        #'mh/ediff-write-merge-buffer-and-maybe-kill)
  )

;;;; Dabbrev

(with-eval-after-load "dabbrev"

;; From `dabbrev.el'
;; Search all buffer when running this function
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
  (setq dabbrev--check-all-buffers t)
  (let ((completion-at-point-functions '(dabbrev-capf)))
    (completion-at-point)))
)

;;;; Eldoc

(with-eval-after-load "eldoc"
;; From `emacs-lisp/eldoc.el'
;; Add `mh/edit-buffer-n' before `erase-buffer'
(defun eldoc--format-doc-buffer (docs)
  "Ensure DOCS are displayed in an *eldoc* buffer."
  (with-current-buffer (if (buffer-live-p eldoc--doc-buffer)
                           eldoc--doc-buffer
                         (setq eldoc--doc-buffer
                               (get-buffer-create " *eldoc*")))
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
               when rest do
               (insert eldoc-doc-buffer-separator)
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
                               "")))))
  eldoc--doc-buffer)
)

;;;; Minibuffer

;; From `minibuffer.el'
;; Add 'space' (`? ') as an 'any' pattern (look for (push 'star pattern))
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

