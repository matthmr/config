;;;; Keybinds for local functions

;;;; Minor Modes Remaps

;;; Diff

(with-eval-after-load "diff"
  (define-key diff-mode-map (kbd "C-c TAB") 'diff-split-hunk))

;;; Mpc

(with-eval-after-load "mpc"
  (define-key mpc-songs-mode-map "\C-\M-m"  #'mpc-select-toggle)
  (define-key mpc-songs-mode-map "\M- "     #'mpc-select-extend)

  (define-key mpc-tagbrowser-mode-map "\C-\M-m" #'mpc-select-toggle)
  (define-key mpc-tagbrowser-mode-map "\M- "    #'mpc-select-extend)

  (define-key mpc-status-mode-map " "  #'mpc-toggle-play)

  (define-key mpc-mode-map "a" #'mpc-playlist-add)
  (define-key mpc-mode-map "d" #'mpc-playlist-delete)
  (define-key mpc-mode-map "D" #'mpc-playlist-destroy)
  (define-key mpc-mode-map "c" #'mpc-playlist-create)
  (define-key mpc-mode-map "r" #'mpc-playlist-rename)
  (define-key mpc-mode-map " " #'mpc-toggle-play)
  (define-key mpc-mode-map "s" #'mpc-stop)
  (define-key mpc-mode-map "P" #'mpc-playlist)
  (define-key mpc-mode-map "v" #'mh/mpc-vol)

  (define-key mpc-mode-map "-" (lambda () (interactive) (mh/mpc-vol "-2")))
  (define-key mpc-mode-map "=" (lambda () (interactive) (mh/mpc-vol "+2")))
  (define-key mpc-mode-map "\C-\M-@" #'mpc-play-at-point))

;;;; Daemon

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

;;;; Toggle *

;;; Toggle Input Method
;; (global-set-key (kbd "C-x C-M-m C-M-i") 'toggle-input-method)

;;; Toggle Whitespace
(defun mh/toggle-ws ()
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace)))

(global-set-key (kbd "C-x C-M-m C-M-w") 'mh/toggle-ws)

;;; Toggle 'local-variable'
(defun mh/toggle-locvar ()
  (interactive)
  (if enable-local-variables
    (setq enable-local-variables nil
        enable-dir-local-variables nil
        local-enable-local-variables nil)
    (setq enable-local-variables :all
          enable-dir-local-variables t
          local-enable-local-variables t))
  )

(global-set-key (kbd "C-x C-M-m C-M-v") 'mh/toggle-locvar)

;;; Toggle Truncation
(defun mh/toggle-trunc ()
  (interactive)
  (setq-local truncate-lines (not truncate-lines)))

(global-set-key (kbd "C-x C-M-m C-M-s") 'mh/toggle-trunc)

;;; Toggle `recentf'
(global-set-key (kbd "C-x C-M-m C-M-r") #'recentf-mode)

;;; Toggle Xterm*
(global-set-key (kbd "C-x C-M-m C-M-p") #'xterm-mouse-mode)
(global-set-key (kbd "C-x C-M-m C-M-o") #'mouse-wheel-mode)

;;; Toggle Line Numbers
(global-set-key (kbd "C-x C-M-m C-M-l") 'display-line-numbers-mode)

;;; Toggle Fill Column
(global-set-key (kbd "C-x C-M-m C-M-c") 'display-fill-column-indicator-mode)

;;; Toggle All
(defun mh/edit-buffer-y ()
  (interactive)
  (setq-local show-trailing-whitespace t)
  (setq-local truncate-lines t)
  (display-fill-column-indicator-mode 1)
  (display-line-numbers-mode 1))

(defun mh/edit-buffer-n ()
  (interactive)
  (setq-local show-trailing-whitespace nil)
  (setq-local truncate-lines nil)
  (display-fill-column-indicator-mode -1)
  (display-line-numbers-mode -1))

(global-set-key (kbd "C-x C-M-m C-M-m") 'mh/edit-buffer-y)
(global-set-key (kbd "C-x C-M-m C-M-n") 'mh/edit-buffer-n)

;;;; Formating

(defun mh/delete-space-after-point ()
  (interactive)
  (let ((point (point)))
    (delete-region
      point
      (progn
        (skip-chars-forward " \t")
        (constrain-to-field nil point t)))))

(defun mh/kill-line-before-point ()
  "Kills the line before the point"
  (interactive)
  (if (eq current-prefix-arg nil)
      (setq current-prefix-arg 0))
  (call-interactively 'kill-line))

(defun mh/backward-kill-line ()
  "Kills the line backward"
  (interactive)
  (set-mark (point))
  (beginning-of-line)
  (call-interactively 'kill-region))

(global-set-key (kbd "C-x C-M-\\") 'mh/delete-space-after-point)
(global-set-key (kbd "C-x C-M-h")  'mh/backward-kill-line)

;;;; External prog kill-ring

(defun mh/xclip-copy ()
  "Copy to XCLIP"
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end)
   "xc" nil nil))

(global-set-key (kbd "C-c SPC x M-w") 'mh/xclip-copy)

(defun mh/xclip-read ()
  "Read to clipboard"
  (interactive)
  (let ((buffer-file-name "/tmp/emacs/xclip"))
    (save-buffer))
  (shell-command "xc < /tmp/emacs/xclip") nil nil)

(global-set-key (kbd "C-c SPC x C-s") 'mh/xclip-read)

(defun mh/xclip-paste ()
  "Paste XCLIP's clipboard"
  (interactive)
  (shell-command "xp")
  (insert-buffer "*Shell Command Output*"))

(global-set-key (kbd "C-c SPC x C-y") 'mh/xclip-paste)

(defun mh/xclip-edit ()
  "Edit XCLIP's clipboard"
  (interactive)
  (shell-command
   "xp > /tmp/emacs/xclip" nil nil)
  (find-file "/tmp/emacs/xclip")
  (revert-buffer-quick))

(global-set-key (kbd "C-c SPC x C-e") 'mh/xclip-edit)

;;;

(defun mh/tmux-edit ()
  "Edit TMUX buffer"
  (interactive)
  (let ((buf (get-buffer "*tmux*")))
    (if buf
        ;; clear it
     (with-current-buffer buf (erase-buffer))
     (setq buf (create-file-buffer "*tmux*")))
    (shell-command "tmux save-buffer -" buf nil)
    (switch-to-buffer buf)))

(global-set-key (kbd "C-c SPC t C-e") 'mh/tmux-edit)

(defun mh/tmux-read ()
  "Read into TMUX buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max)
    "tmux load-buffer -"))

(global-set-key (kbd "C-c SPC t C-s") 'mh/tmux-read)

(defun mh/tmux-paste ()
  "Paste from TMUX buffer"
  (interactive)
  (shell-command "tmux save-buffer -")
  (insert-buffer "*Shell Command Output*"))

(global-set-key (kbd "C-c SPC t C-y") 'mh/tmux-paste)

(defun mh/tmux-copy ()
  "Copy region to TMUX"
  (interactive)
  (call-interactively 'kill-ring-save)
  (shell-command-on-region
   (region-beginning) (region-end)
   "tmux load-buffer -" nil nil))

(global-set-key (kbd "C-c SPC t M-w") 'mh/tmux-copy)

;;;; VC

(defun mh/vc-register-int ()
  (interactive)
  (let ((bufname (buffer-file-name)))
    (async-shell-command
     (format "git add --patch %s"
             (if bufname bufname "."))))
  (switch-to-buffer-other-window "*Async Shell Command*")
  (mh/edit-buffer-n)
  (highlight-regexp "^+.*"  'diff-added)
  (highlight-regexp "^-.*+" 'diff-removed)
  (highlight-regexp "^@@.*" 'diff-hunk-header))

(defun mh/vc-revert-int ()
  (interactive)
  (let ((bufname (buffer-file-name)))
    (async-shell-command
     (format "git restore --patch %s"
             (if bufname bufname "."))))
  (switch-to-buffer-other-window "*Async Shell Command*")
  (mh/edit-buffer-n)
  (highlight-regexp "^+.*"  'diff-added)
  (highlight-regexp "^-.*+" 'diff-removed)
  (highlight-regexp "^@@.*" 'diff-hunk-header))

;; (defun mh/vc-diff-work-against-staging ()
;;   (interactive)
;;   (let ((bufname (buffer-file-name)))
;;     (shell-command
;;      (format "git diff %s"
;;              (if vc-buffer-file-name
;;                  vc-buffer-file-name
;;                "."))))
;;   (switch-to-buffer-other-window "*Shell Command Output*")
;;   (read-only-mode)
;;   (diff-mode))
;; (global-set-key (kbd "C-x v C-d") 'mh/vc-diff-work-against-staging)

(global-set-key (kbd "C-x v C-i") 'mh/vc-register-int)
(global-set-key (kbd "C-x v C-u") 'mh/vc-revert-int)

;;;; Editing

(defun mh/yank (times)
  (interactive "P")
  (if (numberp times)
      (progn
        (dotimes (i (abs times)) (yank)))
      (yank times)))

(global-set-key (kbd "C-y")   'mh/yank)
(global-set-key (kbd "C-M-y") 'yank)

(defun mh/kill-to-register ()
  (interactive)
  (let ((bnd (car (region-bounds))))
    (call-interactively #'copy-to-register)
    (kill-region (car bnd) (cdr bnd))
  ))

(global-set-key (kbd "C-x r C-k") #'mh/kill-to-register)

;;;; Movement

(defun mh/pop-to-mark-forward ()
  "Go back to the last popped mark"
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(global-set-key (kbd "C-c C-SPC") #'mh/pop-to-mark-forward)

(defun mh/scroll-up (arg)
  "Pull up half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
          (scroll-up (/ (window-height) 2))
    (scroll-up arg))
    (error (beep 1)
           (message "End of buffer")
           (goto-char (point-max)))))

(defun mh/scroll-down (arg)
  "Pull down half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
          (scroll-down (/ (window-height) 2))
        (scroll-down arg))
    (error (beep 1)
           (message "Beginning of buffer")
           (goto-char (point-min)))))

(defun mh/next-line-keep ()
  "Go to the next line, and keep the cursor position"
  (interactive)
  (forward-line 1)
  (scroll-up 1))

(defun mh/previous-line-keep ()
  "Go to the previous line, and keep the cursor position"
  (interactive)
  (forward-line -1)
  (scroll-down 1))

(defun mh/scroll-right ()
  (interactive)
  (mh/with-prefix 15 'scroll-right))

(defun mh/scroll-left ()
  (interactive)
  (mh/with-prefix 15 'scroll-left))

(global-set-key (kbd "C-x <") #'mh/scroll-right)
(global-set-key (kbd "C-x >") #'mh/scroll-left)

(global-set-key (kbd "M-K") 'mh/previous-line-keep)
(global-set-key (kbd "M-J") 'mh/next-line-keep)

(global-set-key (kbd "C-v")   'mh/scroll-up)
(global-set-key (kbd "M-v")   'mh/scroll-down)
(global-set-key (kbd "C-M-v") 'scroll-lock-mode)

(defun mh/up-to-char (arg char)
  "Points to char given by interactive `char'"
  (interactive "P\ncUp to char: ")
  (if (eq arg '-)
      (search-backward (char-to-string char) nil nil 1)
    (progn
      (search-forward (char-to-string char) nil nil 1)
      (backward-char))))

(defun mh/to-char (arg char)
  "Points over char given by interactive `char'"
  (interactive "P\ncTo char: ")
  (mh/up-to-char arg char)
  (if (eq arg '-)
      (backward-char)
    (forward-char)))

(global-set-key (kbd "M-z") 'mh/to-char)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(global-set-key (kbd "C-x C-M-x") 'repeat-complex-command)

(defun mh/imenu-at-point ()
  "Uses `imenu' to find symbol at point"
  (interactive)
  (let ((symbol (symbol-at-point)))
    (when symbol
      (imenu (symbol-name symbol)))))

(global-set-key (kbd "C-x C-M-j") 'mh/imenu-at-point)

(defun mh/mark-sexp-at-point ()
  "Marks the SEXP at point"
  (interactive)
  (let ((beg (save-excursion (beginning-of-sexp) (point)))
        (end (save-excursion (end-of-sexp) (point))))
    (goto-char end)
    (set-mark (point))
    (goto-char beg)
  ))

(global-set-key (kbd "C-x C-M-SPC") 'mh/mark-sexp-at-point)

(defun mh/isearch-region ()
  "Use `isearch' with the region as the search string"
  (interactive)
  (when (region-active-p)
    (push
      (let ((beg (caar (region-bounds)))
            (end (cdar (region-bounds))))
        (buffer-substring beg end)
      )
      search-ring)
    (deactivate-mark)
    (isearch-backward)
  ))

(global-set-key (kbd "C-x C-M-s") 'mh/isearch-region)

(require 'ffap)

(defun mh/goto-file-at-point ()
  "Goto file at point"
  (interactive)
  (when-let ((file (ffap-file-at-point)))
    (find-file file)))

(global-set-key (kbd "C-x C-M-r") 'mh/goto-file-at-point)

;;;; Emacs Overrides

(defun mh/keyboard-quit ()
  "From `https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration':
   make `keyboard-quit' (`C-g') dwim-like"
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(global-set-key (kbd "C-g") #'mh/keyboard-quit)
