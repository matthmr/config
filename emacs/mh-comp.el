(add-to-list 'load-path @EMACS_COMPANY_MODE@)

(require 'company)

(global-set-key (kbd "C-x C-_") 'company-complete)

(setq-default company-backends
              '(company-semantic
                company-capf company-files
                (company-dabbrev-code company-gtags company-etags
                                      company-keywords)
                company-dabbrev)
              company-idle-delay 0.5)

(global-company-mode t)
