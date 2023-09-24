;;; mh-viper.el --- Custom layer for viper-mode.

;; Copyright (C) 2023 mH

;; Author: mH <github.com/matthmr>
;; Version: 2.0.0

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
;;  Custom layer on top of viper mode

(setq-default
  viper-ex-style-editing nil
  viper-ex-style-motion nil
  viper-vi-style-in-minibuffer nil
  viper-inhibit-startup-message 't
  viper-expert-level '5)

(setq viper-mode t)
(require 'viper)

;; let `doom-modeline' handle the modeline
(delq 'viper-mode-string global-mode-string)

;;;; Options

(setq-default
  viper-use-replace-region-delimiters t
  viper-case-fold-search t
  viper-shift-width 2
  viper-toggle-key ""
  viper-emacs-state-id "E "
  viper-replace-state-id "R "
  viper-insert-state-id "I "
  viper-vi-state-id "V "
  viper-want-ctl-h-help nil
  viper-vi-state-mode-list '()
  viper-insert-state-mode-list '())

;;;; Themes

(custom-set-faces
  '(viper-minibuffer-emacs ((t nil)))
  '(viper-minibuffer-insert ((t nil)))
  '(viper-minibuffer-vi ((t nil)))
  '(viper-replace-overlay ((t nil))))

;;;; Keybindings

;;; Normal

(define-key viper-vi-basic-map "\C-o"   'pop-to-mark-command)
(define-key viper-vi-basic-map "\C-i"   'mh/pop-to-mark-forward)
(define-key viper-vi-basic-map "\C-b"   'backward-char)
(define-key viper-vi-basic-map "\C-f"   'forward-char)
(define-key viper-vi-basic-map "\C-\\"  'toggle-input-method)
(define-key viper-vi-basic-map "-"      'viper-goto-eol)
(define-key viper-vi-basic-map "u"      'undo)
(define-key viper-vi-basic-map "U"      'undo-redo)
(define-key viper-vi-basic-map "\x20"   'set-mark-command)
(define-key viper-vi-basic-map "g"      'beginning-of-buffer)
(define-key viper-vi-basic-map "\C-v"   'mh/scroll-up)
(define-key viper-vi-basic-map "\M-v"   'mh/scroll-down)
(define-key viper-vi-basic-map "="      'universal-argument)
(define-key viper-vi-basic-map "\C-]"   'viper-change-state-to-emacs)

(setq viper-vi-intercept-map '(keymap (escape . keyboard-quit) (26 . repeat)))

;;; Insert (unset as of 20230924)

(define-key viper-insert-basic-map "\C-d"  'delete-char)
(define-key viper-insert-basic-map "\C-t"  'transpose-chars)
(define-key viper-insert-basic-map "\C-u"  'mh/kill-line-before-point)
(define-key viper-insert-basic-map "\C-\\" 'toggle-input-method)
(define-key viper-insert-basic-map "\C-v"  'mh/scroll-up)
(define-key viper-insert-basic-map "\M-v"  'mh/scroll-down)

;;; Emacs

(keymap-unset viper-emacs-intercept-map "C-z")

(define-key viper-emacs-kbd-map "/"    'self-insert-command)
(define-key viper-emacs-kbd-map "\C-z" 'repeat)
(define-key viper-emacs-kbd-map "\C-]" 'viper-change-state-to-vi)

(setq viper-emacs-intercept-map '(keymap (escape . viper-change-state-to-vi)))

;;;; Symbol override

(fset 'viper-change-state-to-insert 'viper-change-state-to-emacs)

;;;; Function overrides

;; allow SPC to operate on region
;; from: /usr/share/emacs/(emacs-version)/lisp/emulation/viper-cmd.el.gz
(defun viper-prefix-arg-com (char value com)
  (let ((cont t)
	cmd-info
	cmd-to-exec-at-end)
    (while (and cont
                (memq char
                      (list ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ?\ 
                            viper-buffer-search-char)))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
            (if (memq char '(?# ?\")) (user-error viper-ViperBell))
	    (setq com (cons char com))
	    (setq cont nil))
	;; If com is nil we set com as char, and read more.  Again, if char is
	;; ", we read the name of register and store it in viper-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit the while
	;; loop.
        (cond ((memq char '(?! ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((viper= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char)))
	      ((viper= char ?\")
	       (let ((reg (read-char)))
		 (if (viper-valid-register reg)
		     (setq viper-use-register reg)
		   (user-error viper-ViperBell))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (read-char))))))

    (if (atom com)
	;; `com' is a single char, so we construct the command argument
	;; and if `char' is `?', we describe the arg; otherwise
	;; we prepare the command that will be executed at the end.
	(progn
	  (setq cmd-info (cons value com))
	  (while (viper= char ?U)
	    (viper-describe-arg cmd-info)
	    (setq char (read-char)))
	  ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
	  ;; we execute it at the very end
	  (or (viper-movement-command-p char)
	      (viper-digit-command-p char)
	      (viper-regsuffix-command-p char)
	      (viper= char ?!) ; bang command
	      (viper= char ?g) ; the gg command (like G0)
	      (user-error viper-ViperBell))
	  (setq cmd-to-exec-at-end
		(viper-exec-form-in-vi
		 `(key-binding (char-to-string ,char)))))

      ;; as com is non-nil, this means that we have a command to execute
      (if (eq (car com) ?\ )
	  ;; execute appropriate region command.
	  (let ((char (car com)) (com (cdr com)))
	    (setq prefix-arg (cons value com))
	    (if (viper= char ?\ )
		(viper-region prefix-arg)
	      (viper-Region prefix-arg))
	    ;; reset prefix-arg
	    (setq prefix-arg nil))
	;; otherwise, reset prefix arg and call appropriate command
	(setq value (if (null value) 1 value))
	(setq prefix-arg nil)
	(cond
	 ;; If we change ?C to ?c here, then cc will enter replacement mode
	 ;; rather than deleting lines.  However, it will affect 1 less line
	 ;; than normal.  We decided to not use replacement mode here and
	 ;; follow Vi, since replacement mode on n full lines can be achieved
	 ;; with nC.
	 ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	 ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	 ((equal com '(?d . ?y)) (viper-yank-defun))
	 ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	 ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	 ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	 ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	 ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	 ;; gg  acts as G0
	 ((equal (car com) ?g)   (viper-goto-line 0))
	 (t (user-error viper-ViperBell)))))

    (if cmd-to-exec-at-end
	(progn
	  (setq last-command-event char)
	  (condition-case err
	      (funcall cmd-to-exec-at-end cmd-info)
	    (error
	     (error "%s" (error-message-string err))))))
    ))

;; allow C-h to be DEL, unless on normal mode
;; from: /usr/share/emacs/(emacs-version)/lisp/emulation/viper-cmd.el.gz
(defun viper-adjust-keys-for (state)
  "Make necessary adjustments to keymaps before entering STATE."
  (cond ((memq state '(insert-state replace-state))
	 (if viper-auto-indent
	     (progn
	       (define-key viper-insert-basic-map "\C-m" #'viper-autoindent)
	       (if viper-want-emacs-keys-in-insert
		   ;; expert
		   (define-key viper-insert-basic-map "\C-j" nil)
		 ;; novice
		 (define-key viper-insert-basic-map "\C-j" #'viper-autoindent)))
	   (define-key viper-insert-basic-map "\C-m" nil)
	   (define-key viper-insert-basic-map "\C-j" nil))

	 (setq viper-insert-diehard-minor-mode
	       (not viper-want-emacs-keys-in-insert))

	 (define-key viper-insert-basic-map
	   "\C-h" #'viper-del-backward-char-in-insert)
	 (define-key viper-replace-map
	   "\C-h" #'viper-del-backward-char-in-replace)
	 (define-key viper-minibuffer-map
	   "\C-h" #'viper-del-backward-char-in-insert)
	 ;; In XEmacs, C-h overrides backspace, so we make sure it doesn't.
	 (define-key viper-insert-basic-map
	   [backspace] #'viper-del-backward-char-in-insert)
	 (define-key viper-replace-map
	   [backspace] #'viper-del-backward-char-in-replace)
	 ) ; end insert/replace case
	(t ; Vi state
	 (setq viper-vi-diehard-minor-mode (not viper-want-emacs-keys-in-vi))
	 (define-key viper-vi-basic-map "\C-h" #'help-command)
	 ;; In XEmacs, C-h overrides backspace, so we make sure it doesn't.
	 (define-key viper-vi-basic-map [backspace] #'viper-backward-char))
	))

;; stupid fucking thing
;; from: /usr/share/emacs/(emacs-version)/lisp/emulation/viper.el.gz
(defun viper-set-replace-overlay-glyphs (_ after-glyph)
  (or (overlayp viper-replace-overlay)
      (viper-set-replace-overlay (point-min) (point-min)))
  (overlay-put viper-replace-overlay 'after-string after-glyph))

;; replace-mode has emacs-mode bindings
;; from: /usr/share/emacs/(emacs-version)/lisp/emulation/viper-cmd.el
(defun viper-change-state-to-replace (&optional non-R-cmd)
  (viper-change-state 'emacs-state)
  ;; Run insert-state-hook
  (condition-case conds
      (run-hooks 'viper-insert-state-hook 'viper-replace-state-hook)
    (error
     (viper-message-conditions conds)))

  (if non-R-cmd
      (viper-start-replace)
    ;; 'R' is implemented using Emacs's overwrite-mode
    (viper-start-R-mode))
  )
