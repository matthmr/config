(add-to-list 'load-path @EMACS_DOOM_MODELINE@)

(require 'doom-modeline)

(setq doom-modeline-buffer-file-name-style 'truncate-except-project
      doom-modeline-icon nil
      doom-modeline-minor-modes t
      doom-modeline-workspace-name nil)

(custom-set-faces
 '(doom-modeline-buffer-file ((t (:inherit (doom-modeline mode-line-buffer-id bold)))))
 '(doom-modeline-buffer-major-mode ((t (:inherit (doom-modeline-emphasis bold) :background "black"))))
 '(doom-modeline-buffer-minor-mode ((t (:inherit (doom-modeline font-lock-doc-face) :background "black" :slant italic :weight normal))))
 '(doom-modeline-input-method ((t (:inherit doom-modeline-emphasis :background "black")))))

(doom-modeline-mode t)
