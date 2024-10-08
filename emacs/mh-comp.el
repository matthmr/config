(add-to-list 'load-path @EMACS_COMPANY_MODE@)

(require 'company)

(global-set-key (kbd "C-M-q") 'completion-at-point)
(global-set-key (kbd "C-M-i") 'company-complete)

(setq-default
  company-backends
    '(company-capf company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
  company-idle-delay 0.5)

(global-company-mode t)
