;; Preamble
; color theme
(add-to-list 'custom-theme-load-path
						 "/home/mh/.emacs.d/themes") ;; gruber-darker,vscode-dark-plus
(add-to-list 'custom-theme-load-path
						 "/home/mh/.emacs.d/themes/base16-theme/build") ;; base16
; git-managed packages
(add-to-list 'load-path
						 "/home/mh/Git/EMACS/markdown-mode") ;; markdown-mode
(add-to-list 'load-path
						 "/home/mh/Git/EMACS/evil-mode") ;; evil-mode
(add-to-list 'load-path
						 "/home/mh/.emacs.d/themes") ;; base16

;; Global Configuration
; set by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(fill-column 70)
 '(fringe-mode 0 nil (fringe))
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Term" :foundry "UKWN" :slant normal :weight semi-bold :height 169 :width normal)))))

; (in)sane defaults
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)
(setq next-screen-context-lines 20)
(setq column-number-indicator-zero-based nil)
(setq-default truncate-lines 0)
(setq-default tab-width 2)
(setq-default window-divider-default-places 'right-only)
(setq-default kill-ring-max 30)
(setq-default message-log-max 100) ; as per `https://www.emacswiki.org/emacs/MessagesBuffers'
(setq-default select-enable-clipboard nil) ; as per `edoc://Clipboard.html'
(setq-default select-active-regions nil) ; as per `edoc://Primary-Selection.html'
(setq-default frame-title-format '("emacs@linux - %b")) ; as per `https://emacs.stackexchange.com/questions/16834/how-to-change-the-frame-title-from-emacshost-to-visited-file-name'
(setq-default inhibit-x-resources t) ; as per [C-h v inhibit-x-resources]
(setq-default indent-tabs-mode nil)
(setq-default split-width-threshold 1)

; even more (in)sane defaults
(fset 'yes-or-no-p 'y-or-n-p) ;; make every "yes or no" question a "y or n" question

; remap
(defun call-prefix (prefix function)
  "Call `function' with `prefix'"
  (if (eq current-prefix-arg nil)
      (setq current-prefix-arg prefix))
  (call-interactively function))
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (call-prefix -1 'other-window)))
(global-set-key (kbd "C-x <") (lambda () (interactive) (call-prefix 10 'scroll-right)))
(global-set-key (kbd "C-x >") (lambda () (interactive) (call-prefix 10 'scroll-left)))
(global-set-key (kbd "M-o") 'overwrite-mode)

;; MODES
; Xterm
(setq xterm-mouse-mode t)

; C
(setq c-default-style "linux")
(setq c-basic-offset 2)

; Python
(setq python-basic-offset 2)

; Shell
(setq sh-basic-offset 2)

; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; HOOKS
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode))

;; THEMES
(load-theme 'base16-grayscale-dark t)

;; INPUT
(set-input-method 'programmer-dvorak)

;; EMACS DISABLED MODES (why?)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
