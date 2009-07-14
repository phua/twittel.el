;;; twittel.el --- A Twitter API wrapper for Emacs Lisp

;; Author: Peter Hua
;; Version: $Revision: 1.0$
;; Keywords: twitter

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A Twitter API wrapper for Emacs Lisp

;; twitter-statuses-public-timeline
;; twitter-statuses-friends-timeline
;; twitter-statuses-user-timeline
;; twitter-statuses-mentions
;; twitter-statuses-update-prompt
;; twitter-statuses-update-region

;;; Code:

(require 'cl)
(require 'url)
(require 'url-http)
(require 'xml)

;;; Customizations

(defgroup twittel nil "Twittel" :group 'applications)

(defcustom twittel-username nil "username" :type '(string) :group 'twittel)

(defcustom twittel-password nil "password" :type 'string :group 'twittel)

;;; Twitter API

(defun twitter-url-retrieve (url callback cbargs)
  "Twittel url-retrieve wrapper with authentication.
Very insecure authentication using BasicAuth to twitter.com."
  (unless (assoc "twitter.com:443" url-http-real-basic-auth-storage)
    (push '("twitter.com:443" .
	    '("Twitter API" .
	      (base64-encode-string (concat twittel-username ":" twittel-password))))
	  url-http-real-basic-auth-storage))
  (url-retrieve url callback cbargs))

(defun twitter-error (status)
  (let ((e (plist-get status :error)))
    (when e
      (signal (car e) (cdr e)))))

;; Timeline methods

(defun twitter-statuses-timeline (url)
  (let ((url-request-method "GET")
	(url-request-data ""))
    (twitter-url-retrieve url 'twitter-timeline-callback '())))

(defun twitter-statuses-public-timeline ()
  (interactive)
  (let ((url-request-method "GET")
	(url-request-data ""))
    (url-retrieve "http://twitter.com/statuses/public_timeline.xml"
		  'twitter-timeline-callback '())))

(defun twitter-statuses-friends-timeline ()
  (interactive)
  (twitter-statuses-timeline "http://twitter.com/statuses/friends_timeline.xml"))

(defun twitter-statuses-user-timeline ()
  (interactive)
  (twitter-statuses-timeline "http://twitter.com/statuses/user_timeline.xml"))

(defun twitter-statuses-mentions ()
  (interactive)
  (twitter-statuses-timeline "http://twitter.com/statuses/mentions.xml"))

(defun twitter-timeline-callback (status)
  (let* ((statuses-node (car (xml-parse-region (point-min) (point-max))))
	 (contents (twitter-statuses-format statuses-node))
	 (buffer (get-buffer-create"*Twitter Public Timeline*")))
    (with-current-buffer buffer
      (erase-buffer)
      (kill-all-local-variables)
      (if (plist-get status :error)
	  (twitter-error status)
	(insert contents))
      (goto-char (point-min)))
    (view-buffer buffer 'kill-buffer)))

(defun twitter-statuses-format (statuses-node)
  (twitter-parse-statuses-node statuses-node))

(defun twitter-parse-statuses-node (statuses-node)
  (let ((message nil)
	(statuses (xml-get-children statuses-node 'status)))
    (mapcar #'(lambda (status-node)
		(let ((status-string (twitter-parse-status-node status-node)))
		  (setq message (concat message "\n" status-string))))
	    statuses)
    message))

(defun twitter-parse-status-node (status-node)
  (let* ((created-at-node (car (xml-get-children status-node 'created_at)))
	 (created-at-node-text (car (xml-node-children created-at-node)))
  	 (text-node (car (xml-get-children status-node 'text)))
  	 (text-node-text (car (xml-node-children text-node)))
	 (user-string (twitter-parse-user-node (car (xml-get-children status-node 'user)))))
    (concat user-string ": " text-node-text "\n" created-at-node-text "\n")))

(defun twitter-parse-user-node (user-node)
  (let ((screen-name-node (car (xml-get-children user-node 'screen_name))))
    (car (xml-node-children screen-name-node))))

;; Status methods

(defun twitter-statuses-show (id)
  (let ((url-request-method "GET")
	(url-request-data (concat "id=" (url-hexify-string id))))
    (twitter-url-retrieve
     (concat "http://twitter.com/statuses/show/" id ".xml"))))

(defun twitter-statuses-update-prompt (status)
  (interactive "sWhat are you doing? " status)
  (twitter-statuses-update status))

(defun twitter-statuses-update-region (start end)
  (interactive "r")
  (let ((status (buffer-substring start end)))
    (twitter-statuses-update status)))

(defun twitter-statuses-update (status &optional id)
  (let ((url-request-method "POST")
	(url-request-data (concat "status=" (url-hexify-string status)
				  "&source=" (url-hexify-string twittel-source))))
    (when id
      (setq url-request-data (concat url-request-data "&in_reply_to_status_id=" id)))
    (twitter-url-retrieve "http://twitter.com/statuses/update.xml" 'twitter-status-callback nil)))

(defun twitter-status-callback (status)
  (let* ((status (car (xml-parse-region (point-min) (point-max))))
	 (contents (twitter-parse-status-node status)))
    (message "%s" contents)))

(defun twitter-statuses-destroy (id)
  (let ((url-request-method "DELETE")
	(url-request-data (concat "id=" (url-hexify-string id))))
    (twitter-url-retrieve
     (concat "http://twitter.com/statuses/destroy/" id ".xml"))))

(provide 'twittel)