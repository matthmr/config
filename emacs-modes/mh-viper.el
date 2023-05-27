;;; mh-viper.el --- Custom layer for viper-mode.

;; Copyright (C) 2023 mH

;; Author: mH <github.com/matthmr>
;; Version: 1.2.0

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

(setq viper-mode t)
(require 'viper)

(setq viper-emacs-state-id "<E> ")
(setq viper-replace-state-id "<R> ")
(setq viper-insert-state-id "<I> ")
(setq viper-vi-state-id "<N> ")

(setq viper-vi-style-in-minibuffer nil)

;; DEFINE-KEY: BEGIN

(define-key viper-insert-basic-map "\C-d"  'delete-char)
;(define-key viper-insert-basic-map "\C-w" 'viper-delete-backward-word)
(define-key viper-insert-basic-map  "\C-t" 'transpose-chars)
(define-key viper-insert-basic-map  "\C-u"
  (lambda () (interactive)
    (if (eq current-prefix-arg nil)
        (setq current-prefix-arg 0))
    (call-interactively 'kill-line)))
;;(define-key viper-insert-basic-map "\C-\\"  'toggle-input-method)

(define-key viper-vi-basic-map  "\C-o"
  (lambda () (interactive)
    (if (eq current-prefix-arg nil)
        (setq current-prefix-arg t))
    (call-interactively 'set-mark-command)))
(define-key viper-vi-basic-map  "\C-i"
  (lambda () (interactive)
    (when mark-ring
      (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
      (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
      (when (null (mark t)) (ding))
      (setq mark-ring (nbutlast mark-ring))
      (goto-char (marker-position (car (last mark-ring)))))))
(define-key viper-vi-basic-map "\C-b"   'backward-char)
(define-key viper-vi-basic-map "\C-f"   'forward-char)
;;(define-key viper-vi-basic-map "\C-\\"  'toggle-input-method)
(define-key viper-vi-basic-map "-"      'viper-goto-eol)
(define-key viper-vi-basic-map "u"      'undo)
(define-key viper-vi-basic-map "U"      'undo-redo)
(define-key viper-vi-basic-map "\x20"   'set-mark-command)
(define-key viper-vi-basic-map "v"      'set-mark-command)
(define-key viper-vi-basic-map "V"      (lambda () (interactive)
                                          (end-of-line)
                                          (set-mark-command nil)
                                          (move-beginning-of-line nil)))
(define-key viper-vi-basic-map "g"      'beginning-of-buffer)
;; (define-key viper-vi-basic-map "\C-v"    'rectangle-mark-mode)

(define-key viper-vi-basic-map "\M-]"   'universal-argument)

(define-key viper-minibuffer-map "\C-j" 'icomplete-fido-exit)
(define-key viper-emacs-kbd-map "/"     'self-insert-command)

;; DEFINE-KEY: END

;; allow SPC to operate on region
;; from: /usr/share/emacs/(emacs-version)/lisp/emulation/viper-cmd.el.gz
(defun viper-prefix-arg-com (char value com)
  (let ((cont t)
	cmd-info
	cmd-to-exec-at-end)
    (while (and cont
		(viper-memq-char char
				 (list ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ?\ 
				       viper-buffer-search-char)))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (viper-memq-char char '(?# ?\")) (user-error viper-ViperBell))
	    (setq com (cons char com))
	    (setq cont nil))
	;; If com is nil we set com as char, and read more.  Again, if char is
	;; ", we read the name of register and store it in viper-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit the while
	;; loop.
	(cond ((viper-memq-char char '(?! ?=))
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
      (if (viper-memq-char (car com) '(?\  ?\ ))
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
