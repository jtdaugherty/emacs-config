;;; mserv.el --- Mserv client for GNU Emacs.

;;; More information about Mserv: http://www.mserv.org

;; Author: Lars Bjønnes <lars.bjonnes@fredrikstad.online.no>
;; (With thanks to Christian Nybø for helping me improve my (inferior) Lisp skills)
;; Keywords: mserv, music, mp3

;; Copyright (C) 2002 Lars Bjønnes

;; This file is NOT part of GNU Emacs.

;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation.

;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.

;;   You should have received a copy of the GNU General Public License
;;   along with this program;  if not, write to the Free Software Foundation,
;;   Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.


;;;  Installation:
;;;   Make sure mserv.el is in your load path, and put (require 'mserv) 
;;;   in your .emacs

;;;   Example:
;;;    If mserv.el is in ~/my/elisp/files, add the following to .emacs:
;;;    (setq load-path `("~/my/elisp/files" ,@load-path))
;;;    (require 'mserv)

;;; Some variables: 
;;;   mserv-username  - the username for connecting to mserv
;;;   mserv-password  - the password for connecting to mserv
;;;   mserv-hostname  - the mserv host (default is localhost)
;;;   mserv-port      - the mserv port (default is 4444)

;;; Some commands:
;;;   mserv-play      - start playing
;;;   mserv-next      - next track
;;;   mserv-queue     - queue album/track
;;;   mserv-rate      - rate album/track 
;;;                     This function uses completing-read, so 
;;;                     just hit TAB to get a view of the available
;;;                     albums and their tracks.
;;;                     To rate the current track, just hit enter 
;;;                     when asked for album and track
;;;

;;; Some sample keybindings:
;;;   (define-key global-map [f7] 'mserv-play)
;;;   (define-key global-map [f8] 'mserv-next)
;;;   (define-key global-map [f9] 'mserv-decrease-volume)
;;;   (define-key global-map [f10] 'mserv-increase-volume)
;;;   (define-key global-map [f11] 'mserv-rate)
;;;   (define-key global-map [f12] 'mserv-status)

;;; Code:

;;; TODO: * Auto-generate simple functions
;;;       * Clean up code, add customization
;;;       * Add 'append current track info to file' function
;;;         (ie. "I want to play 'Johnny Cash: The Man Comes Around - Personal Jesus' 
;;;          next time I'm DJ"  function :-)
;;;       * Add current track info to the modeline

(require 'cl)

(defvar mserv-username nil 
  "*Mserv username")

(defvar mserv-password nil
  "*Mserv password")

(defvar mserv-hostname "localhost" 
  "*Hostname or IP address of the Mserv server. Defaults to localhost")

(defvar mserv-port "4444" 
  "*MServ port number. 4444 is default.")

(defvar mserv-volume-step "10" 
  "*Default step when increasing or decreasing volume.")

(defvar mserv-rating-values '(("Awful") ("Bad") ("Neutral") ("Good") ("Superb")) 
  "Available ratings.")

(defvar mserv-process-name "mserv" 
  "Mserv process name")

(defvar mserv-process-buffer "*mserv*" 
  "Mserv process buffer")



;; Global variables for mserv-mode
(defvar mserv-version "0.3.6" 
  "Mserv emacs mode version number")

(defvar mserv-last-output nil 
  "Output from Mserv")

(defvar mserv-process nil
  "Mserv process")

(defun mserv-version ()
  "Return Mserv emacs mode version."
  (interactive)
  (if (interactive-p)
      (message (mserv-version))
      (concat "Mserv emacs client v" mserv-version)))

(defun mserv-process-filter (proc string) 
  "Process filter for Mserv."
  (setq mserv-last-output (concat mserv-last-output string)))

(defun mserv-last-output () 
  "Return last output from Mserv"
  mserv-last-output)

(defun mserv-connected-p () 
  "Return t if connected to Mserv."
  (processp mserv-process))

(defun mserv-connect () 
  "Connect to Mserv. Returns the Mserv process"
  (interactive)
  (unless mserv-username
    (setq mserv-username (read-from-minibuffer "Enter Mserv username: ")))
  (unless mserv-password
    (setq mserv-password (read-from-minibuffer "Enter Mserv password: ")))
  (when (mserv-connected-p)
    (mserv-disconnect))
  (setq mserv-process (open-network-stream mserv-process-name mserv-process-buffer mserv-hostname mserv-port))
  (set-process-filter mserv-process 'mserv-process-filter)
  (mserv-send (concat "USER " mserv-username) t)
  (mserv-send (concat "PASS " mserv-password " RTCOMPUTER") t)
  mserv-process)

(defun mserv-reconnect()
  (interactive)
  (mserv-disconnect)
  (mserv-connect))

(defun mserv-disconnect() 
  (interactive)
  (mserv-send "QUIT" t)
  (delete-process mserv-process)
  (setq mserv-process nil))

(defun mserv-send (command wait)
  (unless (mserv-connected-p)
    (mserv-connect))
  (setq mserv-last-output nil)
  (process-send-string mserv-process (concat command "\n"))
  (when wait 
    (progn	
      (accept-process-output mserv-process)
      mserv-last-output)))

(defun mserv-do (&optional command silent)
  "Send COMMAND to mserv. 
If SILENT is non-NIL, output is sent to MESSAGE. If called interactively, the function prompts for COMMAND."
  (interactive "sEnter mserv command: ")
  (let ((output (mserv-send command t)))
    (unless silent
      (message output))
    output))
  
(defun mserv-increase-volume (&optional increment)
  "Increase volume with INCREMENT. 
If INCREMENT isn't supplied, increase with MSERV-VOLUME-STEP"
  (interactive)
  (mserv-set-volume 
   (concat 
    "+"
    (or increment mserv-volume-step))))

(defun mserv-decrease-volume (&optional decrement)
  "Decrease volume with DECREMENT. 
If DECREMENT isn't supplied, decrease with MSERV-VOLUME-STEP"
  (interactive)
  (mserv-set-volume 
   (concat 
    "-"
    (or decrement mserv-volume-step))))

(defun mserv-set-volume (volume)
  "Set the volume"
  (interactive "sEnter volume: ")
  (mserv-do (concat "VOLUME " volume) t))

(defun mserv-search (string) 
  "Search for albums and tracks."
  (interactive "sEnter search string: ")
  (mserv-do (concat "SEARCH " string)))

(defun mserv-queue (&optional album track)
  "Add album or tracks to the queue."
  (interactive)
  (unless album
      (setq album (mserv-prompt-for-album)))
  (unless track
      (setq track (mserv-prompt-for-track album)))
  (mserv-do (concat "QUEUE " album " " track)))

(defun mserv-rate (&optional album track rating)
  "Rate ALBUM and optionally TRACK with RATING. 
If called interactively, prompt for RATING, ALBUM and TRACK."
  (interactive)
  (mserv-do (concat 
	     "RATE " 
	     (or album (setq album (mserv-prompt-for-album)))
	     " "
	     (or track (mserv-prompt-for-track album))
	     " " 
	     (or rating (let ((completion-ignore-case t))
			  (upcase (completing-read "Enter rating: "  mserv-rating-values nil t)))))))

(defun mserv-prompt-for-album () 
  "Prompt for album."
  (interactive)
  (let ((albums (mserv-album-list)))
    (cdr (assoc (completing-read "Select album: " albums) albums))))

(defun mserv-prompt-for-track (album) 
  "Prompt for track number"
  (interactive)
  (let ((tracks (mserv-track-list album)))
    (cdr (assoc (completing-read "Select track: " tracks) tracks))))

(defun mserv-album-list () 
  "Return list of available albums."
  (loop for album in (cdr (split-string (mserv-do "ALBUMS" t) "\n"))
	for attributes = (split-string album)
	collect (cons (third attributes) (first attributes))))

(defun mserv-track-list (album) 
  "Return list of tracks belonging to ALBUM."
  (loop for track in (cddr (split-string (mserv-do (concat "TRACKS " album) t) "\n"))
	for attributes = (split-string track "\t")
	collect (cons (fourth attributes) (second attributes))))


;; Methods below should be generated
(defun mserv-info ()
  "Display availbale info for the current track."
  (interactive)
  (mserv-do "INFO"))

(defun mserv-clear-queue () 
  "Clear the queue."
  (interactive)
  (mserv-do "CLEAR "))

(defun mserv-play ()
  "Start playing, either from the queue or a random track."
  (interactive)
  (setq mserv-modeline (mserv-do "PLAY")))

(defun mserv-stop ()
  "Stop playing"
  (interactive)
  (mserv-do "STOP"))

(defun mserv-pause ()
  "Stop playing"
  (interactive)
  (mserv-do "PAUSE"))

(defun mserv-next ()
  "Play the next in queue, or if the queue is empty, play a random track"
  (interactive)
  (mserv-do "NEXT"))

(defun mserv-status ()
  "Display status. (Current track etc.)"
  (interactive)
  (mserv-do "STATUS" ))

(defun mserv-history ()
  "Display track history."
  (interactive)
  (mserv-do "HISTORY" ))

(defun mserv-help ()
  "Get help for a Mserv command."
  (interactive)
  (mserv-do (concat "HELP " (read-from-minibuffer "Enter command: " ))))

(defun mserv-factor ()
  "Change the Mser random factor."
  (interactive)
  (mserv-do (concat "FACTOR " (read-from-minibuffer "Enter random factor: " ))))

(provide 'mserv)