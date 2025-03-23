;;;; Native Emacs function keymap overrides

;;;; Icomplete

(with-eval-after-load "icomplete"
;; From `icomplete.el'
  (setq icomplete-minibuffer-map (make-sparse-keymap))

  (keymap-set icomplete-minibuffer-map "TAB" #'icomplete-force-complete)
  (keymap-set icomplete-minibuffer-map "RET" #'icomplete-force-complete-and-exit)
  (keymap-set icomplete-minibuffer-map "C-M-n" #'icomplete-forward-completions)
  (keymap-set icomplete-minibuffer-map "C-M-p" #'icomplete-backward-completions)
  (keymap-set icomplete-minibuffer-map "C-M-j" #'exit-minibuffer)
  (keymap-set icomplete-minibuffer-map "C-j" #'exit-minibuffer)
)

;;;; Hideshow

(with-eval-after-load "hideshow"
;; From `progmodes/hideshow.el'
  (setq hs-minor-mode-map (make-sparse-keymap))

  ;; These bindings roughly imitate those used by Outline mode.
  (keymap-set hs-minor-mode-map "M-D d" #'hs-hide-block)
  (keymap-set hs-minor-mode-map "M-D s" #'hs-show-block)
  (keymap-set hs-minor-mode-map "M-D D" #'hs-hide-all)
  (keymap-set hs-minor-mode-map "M-D S" #'hs-show-all)
  (keymap-set hs-minor-mode-map "M-D l" #'hs-hide-level)
  (keymap-set hs-minor-mode-map "M-D M-D" #'hs-toggle-hiding)
)

;;;; Indent

;; From `indent.el'
(progn
  (setq indent-rigidly-map (make-sparse-keymap))

  (keymap-set indent-rigidly-map "TAB" #'indent-rigidly-right)
  (keymap-set indent-rigidly-map "M-TAB" #'indent-rigidly-left)
  (keymap-set indent-rigidly-map "C-f" #'indent-rigidly-right)
  (keymap-set indent-rigidly-map "C-b" #'indent-rigidly-left)
  (keymap-set indent-rigidly-map "C-M-f" #'indent-rigidly-right)
  (keymap-set indent-rigidly-map "C-M-b" #'indent-rigidly-left-to-tab-stop))

;;;; Isearch

;; From `isearch.el'
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
    (define-key map "\C-g" 'isearch-abort)

    ;; This assumes \e is the meta-prefix-char.
    (or (= ?\e meta-prefix-char)
	(error "Inconsistency in isearch.el"))
    (define-key map (kbd "<escape>") 'isearch-cancel)

    (define-key map "\C-q" 'isearch-quote-char)

    (define-key map "\r" 'isearch-exit)
    (define-key map [return] 'isearch-exit)
    (define-key map "\C-j" 'isearch-printing-char)
    (define-key map "\t" 'isearch-printing-char)
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

    (define-key map (kbd "C-M-TAB") 'isearch-complete)

    ;; Frame events should exit the search, because such frame events
    ;; as `switch-frame’ and `delete-frame’ change the current buffer.
    ;; See Bug#41338 for details.
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
