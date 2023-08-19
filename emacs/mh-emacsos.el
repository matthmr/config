;;;; mh-emacsos.el --- EmacsOS layer for tmux-tty and emacs-tty

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
;;  EmacsOS layer for tmux-tty and emacs-tty

;;;; EXWM/XSESSION (also emulates xsession's)

;; Functor returning closures for shutdown/reboot
(defmacro acpi-for (acpi)
  `(lambda ()
    (interactive)
    (setenv "XINITSLEEP" (format @EMACS_ACPI_FMT@ ,acpi))
    (ansi-term "zsh" (format "%s-shell" ,acpi))
    (setenv "XINITSLEEP" nil)))

;; Functor returning closures for shutdown/reboot for inside tmux
(defmacro acpi-for-tmux (acpi x?)
  `(lambda ()
    (interactive)
    (shell-command (format ,(if x?
                                @EMACS_ACPI_FMT_X@
                             @EMACS_ACPI_FMT@) ,acpi))))

(defun emacsos/init (env/xsession)
  (unless (string= env/wm "tty")
    (find-file (format @EMACS_JOURNAL_FMT@ @EMACS_JOURNAL_PATH@
                       (format-time-string "%Y")
                       (format-time-string "%m"))))

  ;; suspend (suspend memory)
  (global-set-key (kbd "C-x C-M-x C-M-m") (acpi-for "suspend"))

  ;; hibernate (suspend disk)
  (global-set-key (kbd "C-x C-M-x C-M-d") (acpi-for "hibernate"))

  ;; halt (shutdown0)
  (global-set-key (kbd "C-x C-M-x C-M-s") (acpi-for "shutdown"))

  ;; reboot
  (global-set-key (kbd "C-x C-M-x C-M-r") (acpi-for "reboot"))

  ;; start xsession of XESSION is set to 1
  (when (and (string= env/xsession "1")
             (not (file-exists-p "/tmp/xsession.lock")))
    (ansi-term "zsh" "xsession")
    (setenv "XINITSLEEP" nil)))

(defun emacsos/init-tmux (x?)
  ;; suspend (suspend memory)
  (global-set-key (kbd "C-x C-M-x C-M-m") (acpi-for-tmux "suspend" x?))

  ;; hibernate (suspend disk)
  (global-set-key (kbd "C-x C-M-x C-M-d") (acpi-for-tmux "hibernate" x?))

  ;; halt (shutdown)
  (global-set-key (kbd "C-x C-M-x C-M-s") (acpi-for-tmux "shutdown" x?))

  ;; reboot
  (global-set-key (kbd "C-x C-M-x C-M-r") (acpi-for-tmux "reboot" x?)))


(let ((env/wm (getenv "WM"))
      (env/xsession (getenv "XSESSION")))

  (cond
   ;; emacs-xwm: exwm
   ((string= env/wm "emacs-xwm")
    ;; set and configure `exwm'
    (add-to-list 'load-path @EMACS_EXWM_DEP1@)
    (add-to-list 'load-path @EMACS_EXWM_DEP2@)
    (require 'exwm)
    (require 'exwm-config)
    (exwm-config-default)

    (emacsos/init env/xsession))

   ;; emacs-xsession: standard Xemacs
   ((or (string= env/wm "emacs-xsession")
        (string= env/wm "emacs-tty")
        (string= env/wm "tty"))
    (emacsos/init env/xsession))

   ;; tmux-tty: should spawn a new tmux session instead
   ((string= env/wm "tmux-tty")
    (emacsos/init-tmux nil))

   ;; in a stardard Xsession, start `init-tmux' passing the `-x' or `-xt' flag
   (t (emacsos/init-tmux t))))
