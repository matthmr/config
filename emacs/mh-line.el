(add-to-list 'load-path @EMACS_DOOM_MODELINE@)

(require 'doom-modeline)

(setq doom-modeline-buffer-file-name-style 'truncate-except-project
      doom-modeline-icon nil
      doom-modeline-minor-modes t
      doom-modeline-workspace-name nil)

(set-face-attribute 'doom-modeline-buffer-file nil
  :inherit '(doom-modeline mode-line-buffer-id bold))

(set-face-attribute 'doom-modeline-buffer-major-mode nil
  :inherit '(doom-modeline-emphasis bold) :background "black")

(set-face-attribute 'doom-modeline-buffer-minor-mode nil
  :inherit '(doom-modeline font-lock-doc-face) :background "black"
  :slant 'italic :weight 'normal)

(set-face-attribute 'doom-modeline-input-method nil
  :inherit 'doom-modeline-emphasis :background "black")

(doom-modeline-mode t)
