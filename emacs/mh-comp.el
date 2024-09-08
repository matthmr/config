(add-to-list 'load-path @EMACS_COMPANY_MODE@)

(require 'company)

;; swap them
(global-set-key (kbd "C-x C-_") 'completion-at-point)
(global-set-key (kbd "C-M-i")   'company-complete)

(setq-default
  company-backends
    '(company-capf company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
  company-idle-delay 0.5)

(global-company-mode t)
