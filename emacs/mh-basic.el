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
    (lambda () (interactive)
      (setq ediff-highlighting-style 'face
            ediff-auto-refine 'on)))

  (add-hook 'ediff-quit-merge-hook
    ;; For git. The `glob' gets deleted as soon as Emacs launches Ediff. So we
    ;; save in whichever file exists still
    (lambda () (interactive)
      (setq ediff-merge-store-file
            (or ediff-merge-store-file
                (let ((file-A (buffer-file-name ediff-buffer-A))
                      (file-B (buffer-file-name ediff-buffer-B)))
                  (cond ((file-exists-p file-A) file-A)
                        ((file-exists-p file-B) file-B)
                        (t (let ((file
                                  (replace-regexp-in-string
                                   "/tmp/git-blob-.\\{6\\}/" ""
                                   (or file-A file-B)))
                                 (dir (read-file-name "Git root: ")))
                             (concat dir file))))
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

  ;; why the fuck do I have to do this?
  (fset #'ediff-write-merge-buffer-and-maybe-kill
        #'mh/ediff-write-merge-buffer-and-maybe-kill))

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
