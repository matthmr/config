;;; mh-mpc.el --- Dead simple shell-command-based mpd client using mpc.

;; Copyright (C) 2023 mH

;; Author: mH <github.com/matthmr>
;; Version: 1.0.0

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

(defcustom
  mh-mpc--cmd-prefix
  "C-c m"
  "Command prefix for mh-mpc")

(defun mh-mpc--shell-command (cmd)
  (shell-command (concat "mpc " cmd) "*Mpc*" "*Mpc-Error*"))

(defun mh-mpc-toggle ()
  "Toggles the play/pause state of MPD"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "toggle")))

(defun mh-mpc-play (&optional arg)
  "Starts playing. If paused, resumed from when it was paused"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "play")))

(defun mh-mpc-pause ()
  "Pauses the song, so that it can be resumed later"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "pause")))

(defun mh-mpc-stop ()
  "Stops the song. It cannot be resumed later in the same spot it was stopped"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "stop")))

(defun mh-mpc-playlist ()
  "Display the current playlist"
  (interactive)
  (mh-mpc--shell-command "playlist"))

(defun mh-mpc-next ()
  "Plays the next song in the playlist"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "next")))

(defun mh-mpc-prev ()
  "Plays the previous song in the playlist"
  (interactive)
  (save-window-excursion
    (mh-mpc--shell-command "prev")))

(defun mh-mpc-queued ()
  "Displays the current queued song"
  (interactive)
  (mh-mpc--shell-command "queued"))

(defun mh-mpc-seek (am)
  "Seeks `am' into song"
  (interactive "sMPC seek (like this): mpc seek ")
  (when (not (string= am ""))
    (save-window-excursion
      (mh-mpc--shell-command (concat "seek " am)))))

(defun mh-mpc-vol (vol)
  "Sets the current volume"
  (interactive "sMPC volume (like this): mpc volume ")
  (if (string= vol "")
      (mh-mpc--shell-command "volume")
    (save-window-excursion
      (mh-mpc--shell-command (concat "volume " vol)))))

(defun mh-mpc-cmd (cmd)
  "Runs an arbitrary command with MPC"
  (interactive "sMPC command (like this): mpc ")
  (shell-command (concat "mpc " cmd) "*Mpc*" "*Mpc-Error*"))

(global-set-key (kbd (concat mh-mpc--cmd-prefix " SPC")) 'mh-mpc-toggle)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " p"))   'mh-mpc-play)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " RET")) 'mh-mpc-pause)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " s"))   'mh-mpc-stop)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " C-n")) 'mh-mpc-next)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " C-p")) 'mh-mpc-prev)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " l"))   'mh-mpc-playlist)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " v"))   'mh-mpc-vol)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " q"))   'mh-mpc-queued)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " M-x")) 'mh-mpc-cmd)
(global-set-key (kbd (concat mh-mpc--cmd-prefix " g"))   'mh-mpc-seek)

(provide 'mh-mpc)
