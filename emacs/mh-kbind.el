;;;; Minor Modes Remaps

;;; Diff

(with-eval-after-load "diff"
  (define-key diff-mode-map (kbd "C-c TAB") 'diff-split-hunk))

;;; Icomplete

(setq icomplete-minibuffer-map
  (let ((map (make-sparse-keymap)))
    ;(define-key map [remap minibuffer-complete-and-exit] 'icomplete-ret)
    (define-key map (kbd "TAB")   'icomplete-force-complete)
    (define-key map (kbd "C-M-j") 'exit-minibuffer)
    (define-key map (kbd "C-j")   'exit-minibuffer)
    ;; (define-key map (kbd "M-RET") 'exit-minibuffer) ;; select default
    (define-key map (kbd "RET")   'icomplete-force-complete-and-exit)
    (define-key map (kbd "C-M-n") 'icomplete-forward-completions)
    (define-key map (kbd "C-M-p") 'icomplete-backward-completions)
    map))

(setq icomplete-scroll t)

;;; Hideshow

(setq hs-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s C-d") 'hs-hide-block)
    (define-key map (kbd "C-c C-s C-s") 'hs-show-block)
    (define-key map (kbd "C-c C-s M-d") 'hs-hide-all)
    (define-key map (kbd "C-c C-s M-s") 'hs-show-all)
    (define-key map (kbd "C-c C-s C-l") 'hs-hide-level)
    (define-key map (kbd "C-c C-s C-c") 'hs-toggle-hiding)
    map))

;;; Indent

(setq indent-rigidly-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-b") 'indent-rigidly-left)
    (define-key map (kbd "C-f") 'indent-rigidly-right)
    (define-key map (kbd "C-M-b") 'indent-rigidly-left-to-tab-stop)
    (define-key map (kbd "C-M-f") 'indent-rigidly-right-to-tab-stop)
    map))

;;; Isearch

(setq isearch-mode-map
  (let ((i 0)
	(map (make-keymap)))
    (or (char-table-p (nth 1 map))
	(error "The initialization of isearch-mode-map must be updated"))
    ;; Make all multibyte characters search for themselves.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
			  'isearch-printing-char)

    ;; Single-byte printing chars extend the search string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'isearch-printing-char)
      (setq i (1+ i)))

    ;; To handle local bindings with meta char prefix keys, define
    ;; another full keymap.  This must be done for any other prefix
    ;; keys as well, one full keymap per char of the prefix key.  It
    ;; would be simpler to disable the global keymap, and/or have a
    ;; default local key binding for any key not otherwise bound.
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map))

    ;; Several non-printing chars change the searching behavior.
    (define-key map "\C-s" 'isearch-repeat-forward)
    (define-key map "\C-r" 'isearch-repeat-backward)
    ;; Define M-C-s and M-C-r like C-s and C-r so that the same key
    ;; combinations can be used to repeat regexp isearches that can
    ;; be used to start these searches.
    (define-key map "\M-\C-s"   'isearch-repeat-forward)
    (define-key map "\M-\C-r"   'isearch-repeat-backward)
    (define-key map "\177"      'isearch-delete-char)
    (define-key map "\C-h"      'isearch-delete-char)
    (define-key map [backspace] 'undefined) ;bug#20466.
    (define-key map "\C-g"      'isearch-abort)

    ;; This assumes \e is the meta-prefix-char.
    (or (= ?\e meta-prefix-char)
	(error "Inconsistency in isearch.el"))
    (define-key map (kbd "<escape>") 'isearch-cancel)

    (define-key map "\C-q" 'isearch-quote-char)

    (define-key map "\r"     'isearch-exit)
    (define-key map [return] 'isearch-exit)
    (define-key map "\C-j"   'isearch-printing-char)
    (define-key map "\t"     'isearch-printing-char)
    (define-key map [?\S-\ ] 'isearch-printing-char)

    (define-key map "\M-f"    'isearch-yank-word)
    (define-key map "\C-w"    'isearch-yank-word-or-char)
    (define-key map "\C-\M-f" 'isearch-yank-symbol-or-char)
    (define-key map "\C-b"    'isearch-del-char)
    (define-key map "\C-f"    'isearch-yank-char)
    (define-key map "\C-y"    'isearch-yank-kill)
    (define-key map "\M-z"    'isearch-yank-until-char)
    (define-key map "\C-\M-e" 'isearch-yank-line)

    (define-key map "\M-<" 'isearch-beginning-of-buffer)
    (define-key map "\M->" 'isearch-end-of-buffer)

    (define-key map (char-to-string help-char) isearch-help-map)
    (define-key map [help] isearch-help-map)
    (define-key map [f1] isearch-help-map)

    (define-key map "\M-n"    'isearch-ring-advance)
    (define-key map "\M-p"    'isearch-ring-retreat)
    (define-key map "\M-y"    'isearch-yank-pop-only)
    (define-key map "\C-\M-y" 'isearch-yank-pop)

    ;; Pass frame events transparently so they won't exit the search.
    ;; In particular, if we have more than one display open, then a
    ;; switch-frame might be generated by someone typing at another keyboard.
    (define-key map [switch-frame] nil)
    (define-key map [delete-frame] nil)
    (define-key map [iconify-frame] nil)
    (define-key map [make-frame-visible] nil)
    (define-key map [mouse-movement] nil)
    (define-key map [language-change] nil)

    ;; For searching multilingual text.
    (define-key map "\C-\\" 'isearch-toggle-input-method)
    (define-key map "\C-^" 'isearch-toggle-specified-input-method)
    (define-key map "\C-x\\" 'isearch-transient-input-method)

    ;; People expect to be able to paste with the mouse.
    (define-key map [mouse-2] #'isearch-mouse-2)
    (define-key map [down-mouse-2] nil)
    (define-key map [xterm-paste] #'isearch-xterm-paste)

    ;; Some bindings you may want to put in your isearch-mode-hook.
    ;; Suggest some alternates...
    (define-key map "\C-c" 'isearch-toggle-case-fold)
    (define-key map "\C-l" 'isearch-toggle-regexp)
    (define-key map "\C-v" 'isearch-toggle-invisible)
    (define-key map "\C-e" 'isearch-edit-string)
    (define-key map "\C-k" 'isearch-toggle-symbol)
    (define-key map "\C-\ "'isearch-toggle-lax-whitespace)
    (define-key map (kbd "C-M-TAB") 'isearch-complete)
    (define-key map "\C-p" 'isearch-toggle-word)

    (put 'isearch-toggle-case-fold :advertised-binding "\M-sc")
    (put 'isearch-toggle-regexp    :advertised-binding "\M-sr")
    (put 'isearch-edit-string      :advertised-binding "\M-se")

    (define-key map "\M-se" 'isearch-edit-string)
    ;; More toggles defined by `isearch-define-mode-toggle'.

    (define-key map "\M-#"  'isearch-query-replace)
    (define-key map "\M-*"  'isearch-query-replace-regexp)
    (define-key map "\M-o"  'isearch-occur)
    (define-key map "\C-o"  'isearch-occur)
    (define-key map "\M-hr" 'isearch-highlight-regexp)
    (define-key map "\M-hl" 'isearch-highlight-lines-matching-regexp)

    ;; The key translations defined in the C-x 8 prefix should add
    ;; characters to the search string.  See iso-transl.el.
    (define-key map "\C-x8\r" 'isearch-char-by-name)
    map))

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
