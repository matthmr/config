;;;; Emacs-specific functions

(mh/load "kbind")
(mh/load "over")
(mh/load "kver")

;;;; Completion

;; From some stackoverflow I forgot to copy the link of
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

;;;; Whitespace

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
      )))

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

  (defun mh/ediff-copy-ancestor-to-C (arg)
    "Choose ancestor buffer"
    (interactive "P")
    (ediff-copy-diff ediff-current-difference nil 'C nil
      (ediff-get-region-contents ediff-current-difference
        'Ancestor ediff-control-buffer)))

  (add-hook 'ediff-keymap-setup-hook
    (lambda ()
      (define-key ediff-mode-map "c" #'mh/ediff-copy-ancestor-to-C)))
  )

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

;;;; Hooks

(defun mh/desktop-save (desktop-dirname)
  (shell-command (format @EMACS_DESKTOP_SAVE_FMT@
                   desktop-dirname desktop-dirname)))

(defun mh/desktop-read (desktop-dirname)
  (shell-command (format "ls -l %s" desktop-dirname) "*desktop*")
  (switch-to-buffer "*desktop*")
  (setq desktop-base-file-name (read-from-minibuffer "Desktop filename: "))

  (desktop-read desktop-dirname)
  ; next saves are done in `desktop'
  (setq desktop-base-file-name "desktop"))

;; (add-hook 'kill-emacs-hook
;;   (lambda () (interactive) (mh/desktop-save @EMACS_DESKTOP@)))
;; (add-hook 'desktop-no-desktop-file-hook
;;   (lambda () (interactive) (mh/desktop-read @EMACS_DESKTOP@)))

(defun mh/hi-keywords ()
  "Highlight comment keywords"
  (let ((comment-type
         '(("\\<\\(TODO\\|FIXME\\|COMBAK\\|NOTE\\|HACK\\|BUG\\)\\>"
            1 'font-lock-warning-face prepend)))
        (comment-obj
         '(("`\\(.*?\\)'"
            1 'font-lock-constant-face prepend))))
    (font-lock-add-keywords nil comment-type)
    (font-lock-add-keywords nil comment-obj)
    ))

(add-hook 'prog-mode-hook #'mh/hi-keywords)
