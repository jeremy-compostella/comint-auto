;;; comint-auto. --- comint automation module

;; Copyright (C) 2022 Jérémy Compostella

;; Author: Jérémy Compostella <jeremy.compostella@gmail.com>
;; Version: 0.1
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'comint)

(defvar comint-auto-latest-user-and-host nil
  "This variable holds the latest prompted username and hostname
 with a timestamp. If a password is requested within one second,
 this username and hostname are used.")

(defcustom comint-auto-accept-ssh-fingerprints t
  "If set, SSH fingerprints are automatically accepted.")

(defconst comint-auto-user-and-host-regexp "[a-zA-Z0-9_\.-]+"
  "Username and hostname regular expression.")

(defun comint-auto-username-filter (string)
  "Filter function to automatically respond to username prompts.

When `comint-auto-mode' is non-nil, this function attempts
to automatically supply a username in response to a username
prompt.

It searches for a prompt matching a regular expression
constructed from `comint-auto-user-and-host-regexp'. If a match
is found, it attempts to retrieve a username from
`auth-source'. If no username is found in `auth-source', it
prompts the user for input.

STRING is the output string from the inferior process."
  (let ((regexp (format "[uU]ser[a-z]* for '?\\(\\w*://\\)*\\(?1:%s+\\)'?:"
			comint-auto-user-and-host-regexp)))
    (when (and comint-auto-mode
	       (string-match regexp string))
      (let* ((host (match-string 1 string))
	     (source (auth-source-search :host host))
	     (user))
	(when source
	  (setf user (plist-get (car source) :user)))
	(unless user
	  (setf user (read-string string)))
	(setq comint-auto-latest-user-and-host (list (current-time) user host))
	(comint-simple-send (get-buffer-process (current-buffer)) user)))))

(defun comint-auto-ssh-fingerprint-filter (string)
  "Automatically accept SSH key fingerprint."
  (when (and comint-auto-mode
	     comint-auto-accept-ssh-fingerprints
	     (string-match "Are you sure you want to continue connecting (yes/n" string))
    (comint-simple-send (get-buffer-process (current-buffer)) "yes")))

(defun comint-auto-password-finder (string)
  (when comint-auto-mode
    (let ((buf-host (if (file-remote-p default-directory)
			(with-parsed-tramp-file-name default-directory info
			  info-host)
		      "localhost"))
	  user host)
      (cond ((string-match
	      (format "[Pp]assword for \\(user \\)?['‘]?\\([a-z]+://\\)?\\(?1:%s\\)@?\\(?2:%s\\)?['’]?"
		      comint-auto-user-and-host-regexp
		      comint-auto-user-and-host-regexp)
	      string)
	     (setf host (or (match-string 2 string) buf-host)
		   user (match-string 1 string)))
	    ((string-match (format "\\(%s\\)@\\(%s\\)"
				   comint-auto-user-and-host-regexp
				   comint-auto-user-and-host-regexp)
			   string)
	     (setf user (match-string 1 string)
		   host (match-string 2 string)))
	    ((and comint-auto-latest-user-and-host
		  (time-less-p (current-time)
			       (time-add (car comint-auto-latest-user-and-host)
					 (seconds-to-time 1))))
	     (setf user (cadr comint-auto-latest-user-and-host)
		   host (caddr comint-auto-latest-user-and-host)
		   comint-auto-latest-user-and-host nil)))
      (when (and user host)
	(let ((source (or (auth-source-search :host host :user user)
			  (auth-source-search :host host :create t))))
	  (if source
	      (let ((secret (plist-get (car source) :secret)))
		(message "Password found for %s@%s" user host)
		(if (functionp secret)
		    (funcall secret)
		  secret))
	    (message "Could not a password for %s@%s" user host)))))))

(defun comint-auto-install-password-function ()
  (setq-local comint-password-function 'comint-auto-password-finder))

(define-minor-mode comint-auto-mode
  "Toggle comint automation."
  :global t
  :lighter " ComintAuto"
  (let ((hooks '((comint-output-filter-functions . comint-auto-username-filter)
		 (comint-output-filter-functions . comint-auto-ssh-fingerprint-filter)
		 (comint-mode-hook . comint-auto-install-password-function)))
	(add-or-remove-hook (if comint-auto-mode #'add-hook #'remove-hook)))
    (dolist (pair hooks)
      (funcall add-or-remove-hook (car pair) (cdr pair)))))

(provide 'comint-auto)
