;;;; Emacs Basic Config

(load @EMACS_MACROS@)
(load @EMACS_KEYBINDINGS@)
(load @EMACS_ABBREVS@) ; the abbrevs file is not in this repository

(setq-default
  create-lockfiles nil ; these are *trully* useless
  inhibit-startup-screen t
  scroll-error-top-bottom t
  scroll-preserve-screen-position t
  scroll-margin 2
  scroll-conservatively 100
  next-screen-context-lines 2
  column-number-indicator-zero-based nil
  split-height-threshold 30 ; prefer spliting horizontally ; 90
  vc-handled-backends '(Git)
  temporary-file-directory @EMACS_TMP@
  tab-width 2
  window-divider-default-places 'right-only
  kill-ring-max 100
  message-log-max 100 ; as per `https://www.emacswiki.org/emacs/MessagesBuffers'
  select-enable-clipboard nil ; as per `edoc://Clipboard.html'
  select-active-regions nil ; as per `edoc://Primary-Selection.html'
  frame-title-format '("emacs@linux - %b") ; as per `https://emacs.stackexchange.com/questions/16834/how-to-change-the-frame-title-from-emacshost-to-visited-file-name'
  inhibit-x-resources t ; as per [C-h v inhibit-x-resources]
  indent-tabs-mode nil
  sentence-end-double-space nil ; Why the fuck would I do that?
  ; local-enable-local-variables nil ; Why the fuck would I do that?
  ring-bell-function 'ignore ; SHUT THE FUCK UP
  tab-bar-show 1
  echo-keystrokes 0.1
  vc-find-revision-no-save t
  auto-save-list-file-prefix nil ; STOP
  completion-ignore-case t ; case-insensitive
  read-buffer-completion-ignore-case t ; case-insensitive
  goal-column nil
  gc-cons-threshold 600000 ; 600 M
  confirm-kill-emacs 'y-or-n-p
  backup-inhibited nil
  auto-save-default t
  hi-lock-face-defaults '("underline")
  disabled-command-function nil
  comment-column 0
  completions-format 'one-column
  enable-recursive-minibuffers t)

(setq search-whitespace-regexp ".*?"
      lazy-count-prefix-format "[%s/%s] ")

(setq-local default-directory @EMACS_TMP@)

;; Gnus

(setq-default gnus-article-save-directory @EMACS_GNUS@
              gnus-startup-file @EMACS_NEWSRC@)

;;; Autosave

(auto-save-mode)
(fset 'yes-or-no-p 'y-or-n-p)

;;;; Major Modes

;;; C

(setq c-default-style "linux")
(setq c-basic-offset 2)

;;; Python

(setq python-basic-offset 2)
(setq python-indent-offset 2)

;;; Shell

(setq sh-basic-offset 2)

;;; WS

(setq-default whitespace-space-regexp "\\( +$\\)")
(setq-default whitespace-style
              '(face empty trailing spaces tab-mark))
(setq-default whitespace-display-mappings '(
              (tab-mark ?\x09 [?¦ ?\ ])))
(setq-default show-trailing-whitespace t)

;;;; Minor Modes Remaps

;;; Icomplete

(defvar icomplete-minibuffer-map
  (let ((map (make-sparse-keymap)))
    ;(define-key map [remap minibuffer-complete-and-exit] 'icomplete-ret)
    (define-key map (kbd "TAB")   'icomplete-force-complete)
    (define-key map (kbd "C-M-j") 'exit-minibuffer)
    (define-key map (kbd "C-j")   'exit-minibuffer)
    ;; (define-key map (kbd "M-RET") 'exit-minibuffer) ;; select default
    (define-key map (kbd "RET")   'icomplete-force-complete-and-exit)
    (define-key map (kbd "C-M-n") 'icomplete-forward-completions)
    (define-key map (kbd "C-M-p") 'icomplete-backward-completions)
    (define-key map (kbd "M-.")   'icomplete-forward-completions)
    (define-key map (kbd "M-,")   'icomplete-backward-completions)
    map)
  "Keymap used by `icomplete-mode' in the minibuffer'.")

;;; Hideshow

(defvar hs-minor-mode-map
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
    (define-key map "\e\e\e" 'isearch-cancel)

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

    (define-key map "\M-\t" 'isearch-complete)
    (define-key map "\M-_"  'isearch-toggle-symbol)
    (define-key map "\M-w"  'isearch-toggle-word)
    (define-key map "\M-i"  'isearch-toggle-invisible)
    (define-key map "\M-\ " 'isearch-toggle-lax-whitespace)

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
    (define-key map "\C-e" 'isearch-edit-string)
    (define-key map "\C-_" 'isearch-toggle-symbol)

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

(icomplete-mode)
(electric-pair-mode)
(global-display-fill-column-indicator-mode)

;;;; Hooks

(defun mh/desktop-save (desktop-dirname)
  (shell-command (format @EMACS_DESKTOP_SAVE_FMT@
                   desktop-dirname desktop-dirname)))

(defun mh/desktop-read (desktop-dirname)
  (shell-command (format "ls -l %s" desktop-dirname))
  (setq desktop-base-file-name (read-from-minibuffer "Desktop filename: "))

  (desktop-read desktop-dirname)
  ; next saves are done in `desktop'
  (setq desktop-base-file-name "desktop"))

(add-hook 'text-mode-hook   'auto-fill-mode)
(add-hook 'c-mode-hook      (lambda () (interactive)
                              (setq comment-start "//")
                              (setq comment-end "")
                              (setq-local page-delimiter "^/\\{4\\}")
                              (setq-local indent-tabs-mode nil)
                              (outline-minor-mode t)
                              (abbrev-mode -1)))
(add-hook 'python-mode-hook  (lambda () (interactive)
                               (setq-local page-delimiter "^#\\{4\\}")
                               (outline-minor-mode t)))
(add-hook 'sh-mode-hook      (lambda () (interactive)
                               (setq-local page-delimiter "^#\\{4\\}")))

(add-hook 'lisp-mode-hook    (lambda () (interactive)
                               (setq-local page-delimiter "^;\\{4\\}")
                               (outline-minor-mode t)
                               (rainbow-delimiters-mode t)))
(add-hook 'scheme-mode-hook  (lambda () (interactive)
                               (setq-local page-delimiter "^;\\{4\\}")
                               (outline-minor-mode t)
                               (rainbow-delimiters-mode t)))
(add-hook 'emacs-lisp-mode-hook  (lambda () (interactive)
                                   (setq-local page-delimiter "^;\\{4\\}")
                                   (outline-minor-mode t)
                                   (rainbow-delimiters-mode t)))

(add-hook 'html-mode-hook 'nxml-mode)   ; Emacs' default HTML is meh, nXML is
                                        ; better

(add-hook 'mail-mode-hook (lambda () (interactive)
                            (cd @EMACS_TMP@)))

(add-hook 'outline-mode-hook  'auto-fill-mode)
(add-hook 'diff-mode-hook     'outline-minor-mode)
(add-hook 'latex-mode-hook    #'(lambda () (interactive)
                                 (define-key latex-mode-map (kbd "M-TAB") 'completion-at-point)))
(add-hook 'tex-mode-hook      #'(lambda () (interactive)
                                 (define-key tex-mode-map (kbd "M-TAB") 'completion-at-point)))
(add-hook 'diff-mode-hook     #'(lambda ()
                                  (define-key diff-mode-map (kbd "C-c TAB") 'diff-split-hunk)))

(add-hook 'kill-emacs-hook
  (lambda () (interactive) (mh/desktop-save @EMACS_DESKTOP@)))
(add-hook 'desktop-no-desktop-file-hook
  (lambda () (interactive) (mh/desktop-read @EMACS_DESKTOP@)))

(add-hook 'vc-dir-mode-hook
  (lambda () (interactive)
    (define-key vc-dir-mode-map "!" 'vc-edit-next-command)))

;;;; Function overrides

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

;;;; Auto-mode

(add-to-list 'auto-mode-alist '("\\(neo\\)?mutt-.*" . message-mode))