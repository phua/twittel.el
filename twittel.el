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

(require 'cl)
(require 'url)
(require 'url-http)
(require 'xml)

;;; Twitter API

(defun twitter-error (status)
  (let ((e (plist-get status :error)))
    (when e
      (signal (car e) (cdr e))
      (message "%s : %s" (car e) (cdr e)))))

;; Timeline methods

(defun twitter-statuses-public-timeline ()
  (interactive)
  (let ((url-request-method "GET")
	(url-request-data ""))
    (url-retrieve "http://twitter.com/statuses/public_timeline.xml"
		  'twitter-statuses-public-timeline-callback '())))

(defun twitter-statuses-public-timeline-callback (status)
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
		  (setq message (concat status-string "\n" message))))
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

(provide 'twittel)