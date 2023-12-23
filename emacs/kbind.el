;;;; Emacs Keybindings

(global-unset-key (kbd "C-x m"))

(global-set-key (kbd "M-]")     'exit-recursive-edit)
(global-set-key (kbd "C-M-]")   'abort-recursive-edit)

(global-set-key (kbd "M-'")     'expand-abbrev)
(global-set-key (kbd "M-o")     'overwrite-mode)
(global-set-key (kbd "M-#")     'query-replace)
(global-set-key (kbd "M-*")     'query-replace-regexp)
(global-set-key (kbd "C-x ;")   'comment-line)
(global-set-key (kbd "M-_")     'first-error)
(global-set-key (kbd "M-+")     'next-error-select-buffer)
(global-set-key (kbd "C-^")     'delete-indentation)
(global-set-key (kbd "C-M-_")   'undo-redo)

;(global-unset-key (kbd "C-x C-t"))
(global-set-key (kbd "C-x C-t")    'transpose-regions)

(global-set-key (kbd "C-x M-;")   'comment-set-column)
(global-set-key (kbd "C-x x C-n") 'clone-indirect-buffer)

(global-set-key (kbd "C-M-c")     'caps-mode)

(global-set-key (kbd "C-h")       'delete-backward-char)
(global-set-key (kbd "M-h")       'backward-kill-word)
(global-set-key (kbd "C-M-h")     'backward-kill-sexp)
(global-set-key (kbd "C-x C-h")   'help-command)
(global-set-key (kbd "M-L")       'mark-defun)
(global-set-key (kbd "C-M-l")     'mark-paragraph)
(global-set-key (kbd "C-M-m")     'default-indent-new-line)
(global-set-key (kbd "C-x M-.")   'xref-find-references)
(global-set-key (kbd "C-x r C-l") 'list-registers)
(global-set-key (kbd "C-x a C-w") 'write-abbrev-file)
(global-set-key (kbd "C-M-j")     'duplicate-dwim)

;;; Overrides

(global-set-key (kbd "M-c")     'capitalize-dwim)
(global-set-key (kbd "M-u")     'upcase-dwim)
(global-set-key (kbd "M-l")     'downcase-dwim)
(global-set-key (kbd "M-?")     'dabbrev-completion)
(global-set-key (kbd "C-x o")   'delete-blank-lines)
(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "M-p")     'previous-error)
(global-set-key (kbd "M-n")     'next-error)
(global-set-key (kbd "M-(")     'kmacro-start-macro)
(global-set-key (kbd "M-)")     'kmacro-end-macro)
(global-set-key (kbd "M-@")     'kmacro-end-and-call-macro)

;;; Emacs 29

(global-set-key (kbd "M-N") 'minibuffer-next-completion)
(global-set-key (kbd "M-P") 'minibuffer-previous-completion)

;;; With `kmacro'

(global-set-key (kbd "C-x C-k C-w") 'insert-kbd-macro)

;;; With `buffer'

(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "C-x C-M-b")   'bs-show)
(global-set-key (kbd "C-x C-b C-b") 'ibuffer)
(global-set-key (kbd "C-x C-b C-n") 'switch-to-next-buffer)
(global-set-key (kbd "C-x C-b C-p") 'switch-to-prev-buffer)
(global-set-key (kbd "C-x C-b C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b C-w") 'kill-buffer-and-window)

;;; With `window' & `windmove'

(global-set-key (kbd "C-x 2")
                (lambda () (interactive)
                  (select-window (split-window-below))))
(global-set-key (kbd "C-x 3")
                (lambda () (interactive)
                  (select-window (split-window-right))))
(global-set-key (kbd "C-x w 2")
                (lambda () (interactive)
                  (select-window (split-root-window-below))))
(global-set-key (kbd "C-x w 3")
                (lambda () (interactive)
                  (select-window (split-root-window-right))))

(global-unset-key (kbd "C-x C-w"))

(global-set-key (kbd "C-x C-w C-w")   'mh/new-window)
(global-set-key (kbd "C-x C-w C-l")   'windmove-right)
(global-set-key (kbd "C-x C-w C-h")   'windmove-left)
(global-set-key (kbd "C-x C-w C-k")   'windmove-up)
(global-set-key (kbd "C-x C-w C-j")   'windmove-down)
(global-set-key (kbd "C-x C-w l")     'windmove-display-right)
(global-set-key (kbd "C-x C-w h")     'windmove-display-left)
(global-set-key (kbd "C-x C-w k")     'windmove-display-up)
(global-set-key (kbd "C-x C-w j")     'windmove-display-down)
(global-set-key (kbd "C-x C-w s")     'windmove-display-same-window)
(global-set-key (kbd "C-x C-w t")     'windmove-display-new-tab)
(global-set-key (kbd "C-x C-w f")     'windmove-display-new-frame)
(global-set-key (kbd "C-x C-w C-s")   'window-swap-states)
(global-set-key (kbd "C-x C-w C-M-l") 'windmove-swap-states-right)
(global-set-key (kbd "C-x C-w C-M-h") 'windmove-swap-states-left)
(global-set-key (kbd "C-x C-w C-M-k") 'windmove-swap-states-up)
(global-set-key (kbd "C-x C-w C-M-j") 'windmove-swap-states-down)
(global-set-key (kbd "C-x C-w M-l")   'windmove-delete-right)
(global-set-key (kbd "C-x C-w M-h")   'windmove-delete-left)
(global-set-key (kbd "C-x C-w M-k")   'windmove-delete-up)
(global-set-key (kbd "C-x C-w M-j")   'windmove-delete-down)
(global-set-key (kbd "C-x C-w -")     'minimize-window)
(global-set-key (kbd "C-x C-w +")     'maximize-window)
(global-set-key (kbd "C-x C-w C-e")   (lambda () (interactive)
                                        (mh/with-prefix 5 'enlarge-window-horizontally)))
(global-set-key (kbd "C-x C-w C-M-e") (lambda () (interactive)
                                        (mh/with-prefix 5 'shrink-window-horizontally)))
(global-set-key (kbd "C-x C-w C-v")   (lambda () (interactive)
                                        (mh/with-prefix 3 'enlarge-window)))
(global-set-key (kbd "C-x C-w C-M-v") (lambda () (interactive)
                                        (mh/with-prefix 3 'shrink-window)))
(global-set-key (kbd "C-x C-w M-J")   'split-root-window-below)
(global-set-key (kbd "C-x C-w M-L")   'split-root-window-right)
(global-set-key (kbd "C-x C-w C-b")   'fit-window-to-buffer)
(global-set-key (kbd "C-x C-w C-d")   'delete-window)
(global-set-key (kbd "C-x C-w C-M-d") 'delete-other-windows)

;;; With `tab-bar'

(global-set-key (kbd "C-x C-a C-n")   'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-x C-a C-p")   'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-x C-a C-i")   'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-x C-a C-t")   'tab-bar-switch-to-tab)
(global-set-key (kbd "C-x C-a i")     'tab-bar-switch-to-last-tab)
(global-set-key (kbd "C-x C-a C-k")   'tab-bar-close-tab)
(global-set-key (kbd "C-x C-a C-M-k") 'tab-bar-close-other-tabs)
(global-set-key (kbd "C-x C-a C-f")   'tab-bar-move-tab)
(global-set-key (kbd "C-x C-a C-b")   'tab-bar-move-tab-backward)
(global-set-key (kbd "C-x C-a C-a")   'tab-bar-new-tab)
(global-set-key (kbd "C-x C-a C-M-a") 'tab-bar-new-tab-to)
(global-set-key (kbd "C-x C-a C-w")   'tab-bar-move-window-to-tab)
(global-set-key (kbd "C-x C-a C-j")   'tab-bar-move-tab-to-frame)
(global-set-key (kbd "C-x C-a C-d")   'tab-bar-duplicate-tab)
(global-set-key (kbd "C-x C-a C-e")   'tab-bar-rename-tab)

;;; With `frame'

(global-unset-key (kbd "C-x C-j"))
(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-x C-j C-k")   'delete-frame)
(global-set-key (kbd "C-x C-j C-M-k") 'delete-other-frames)
(global-set-key (kbd "C-x C-j C-j")   'make-frame-command)
(global-set-key (kbd "C-x C-j j")     'other-frame-prefix)
(global-set-key (kbd "C-x C-j C-n")   'other-frame)
(global-set-key (kbd "C-x C-j C-p")   (lambda () (interactive)
                                        (mh/with-prefix -1 'other-frame)))
(global-set-key (kbd "C-x C-j C-d")   'clone-frame)
(global-set-key (kbd "C-x C-j C-b")   'display-buffer-other-frame)

;;; `M-S' commands

(global-set-key (kbd "M-A")  'align-regexp)
(global-set-key (kbd "M-W")  'delete-trailing-whitespace)
(global-set-key (kbd "M-C")  'compile)
(global-set-key (kbd "M-\"") 'center-region)
(global-set-key (kbd "M-E")  'replace-string)
(global-set-key (kbd "M-I")  'replace-regexp)

;;; `C-x ESC' Commands

(defun mh/imenu-at-point ()
  "Uses `imenu' to find symbol at point"
  (interactive)
  (let ((symbol (symbol-at-point)))
    (when symbol
      (imenu (symbol-name symbol)))))

(global-set-key (kbd "C-x C-M-d")   'delete-region)
(global-set-key (kbd "C-x C-M-k")   'kill-whole-line)
(global-set-key (kbd "C-x C-M-i")   'imenu)
(global-set-key (kbd "C-x C-M-s")   'ispell-complete-word)
(global-set-key (kbd "C-x C-M-u")   'raise-sexp)
(global-set-key (kbd "C-x C-M-l")   'desktop-read)
(global-set-key (kbd "C-x C-M-r")   'desktop-save-in-desktop-dir)
(global-set-key (kbd "C-x C-M-f")   'find-file-at-point)
(global-set-key (kbd "C-x C-M-v")   'view-file)
(global-set-key (kbd "C-x C-M-c")   'server-edit)
(global-set-key (kbd "C-x C-M-j")   'mh/imenu-at-point)
;(global-set-key (kbd "C-x C-M-w")   'mh/copy-thing-at-point) ; C-x C-M-SPC M-w
(global-set-key (kbd "C-x C-M-w")   'write-file)
(global-set-key (kbd "C-x C-M-q")   'mh/isearch-region)
(global-set-key (kbd "C-x C-M-SPC") 'mh/mark-thing-at-point)
(global-set-key (kbd "C-x C-M-t")   'mh/commit)

;;; Daemon

(defun mh/confirm-suspend ()
  "Prompt for confirming suspension"
  (interactive)
  (if (y-or-n-p (format "Really suspend frame? "))
    (suspend-frame)
    (message "Canceled frame suspension")))

(global-set-key (kbd "C-z")       'repeat)
(global-set-key (kbd "C-x C-z")   'mh/confirm-suspend)

(when (daemonp)
  (global-set-key (kbd "C-x C-c")
                  (lambda () (interactive)
                    (if (y-or-n-p (format "Kill Emacs frame? "))
                        (save-buffers-kill-terminal)
                      (message "Canceled frame kill"))))
  (global-set-key (kbd "C-x x C-c")
                  (lambda () (interactive)
                    (if (y-or-n-p (format "Kill Emacs daemon? "))
                        (kill-emacs)
                      (message "Canceled daemon kill")))))

;;;; Minor Modes Remaps

(define-key minibuffer-local-map "\C-h" 'delete-backward-char)
(define-key minibuffer-local-completion-map " " 'self-insert-command)
