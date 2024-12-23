(require 'pulsar)

(defvar pulsar-pulse-functions
  '(backward-page
    bookmark-jump
    delete-other-windows
    delete-window
    ;; evil-goto-first-line
    ;; evil-goto-line
    ;; evil-scroll-down
    ;; evil-scroll-line-to-bottom
    ;; evil-scroll-line-to-center
    ;; evil-scroll-line-to-top
    ;; evil-scroll-page-down
    ;; evil-scroll-page-up
    ;; evil-scroll-up
    forward-page
    goto-line
    handle-switch-frame
    ;; logos-backward-page-dwim
    ;; logos-forward-page-dwim
    handle-select-window
    move-to-window-line-top-bottom
    narrow-to-defun
    narrow-to-page
    narrow-to-region
    next-buffer
    next-multiframe-window
    org-backward-heading-same-level
    org-forward-heading-same-level
    org-next-visible-heading
    org-previous-visible-heading
    other-window
    outline-backward-same-level
    outline-forward-same-level
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-up-heading
    previous-buffer
    recenter-top-bottom
    reposition-window
    scroll-down-command
    scroll-up-command
    tab-close
    tab-new
    tab-next
    tab-previous
    widen
    windmove-down
    windmove-left
    windmove-right
    windmove-swap-states-down
    windmove-swap-states-left
    windmove-swap-states-right
    windmove-swap-states-up
    windmove-up))

(defface mh/pulsar-face '((t :inverse-video t))
  "Pulsar face")

(defvar pulsar-face 'mh/pulsar-face)

(pulsar-global-mode 1)
