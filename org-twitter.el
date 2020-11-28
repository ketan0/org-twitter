;;; org-twitter.el --- Tweet from org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ketan Agrawal
;;
;; Author: Ketan Agrawal <http://github.com/ketan0>
;; Maintainer: Ketan Agrawal <ketanjayagrawal@gmail.com>
;; Created: October 13, 2020
;; Modified: November 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/ketan0/org-twitter
;; Package-Requires: ((emacs 26.1) (org-ml "4.0") (cl-lib "0.5") (aio "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tweet from org-mode.
;;
;;; Code:
(require 'twittering-mode)
(require 'org-ml)
(require 'aio)

(defgroup org-twitter nil
  "Convert org-mode notes to tweets and threads."
  :group 'org
  :prefix "org-twitter-"
  :link '(url-link :tag "Github" "https://github.com/ketan0/org-twitter"))

(defcustom org-twitter-tweet-link-description "🐦"
  "Link description that is appended following tweeted text."
  :group 'org-twitter
  :type 'string)

(aio-defun org-twitter-tweet-selection (beg end)
  "Interactively tweet the currently selected text."
  (interactive "r")
  (let* ((saved-buffer (current-buffer))
         (status (buffer-substring-no-properties beg end))
         (status-id (alist-get 'id_str (aio-wait-for (org-twitter-tweet status)))))
    (message "status: %s" status)
    (org-twitter-add-tweet-link-to-selection saved-buffer beg end status status-id)))

(defun org-twitter-add-tweet-link-to-selection (saved-buffer beg end status status-id)
  (with-current-buffer saved-buffer
    (delete-region beg end)
    (insert (org-twitter-linkify-tweet status status-id))))

(aio-defun org-twitter-tweet-this-headline (saved-point)
  "Interactively tweet the headline under point."
  (interactive "d")
  (let* ((saved-buffer (current-buffer))
         (status (org-ml-get-property :raw-value (org-ml-parse-this-headline)))
         (status-id (alist-get 'id_str (aio-await (org-twitter-tweet status)))))
    (org-twitter-add-tweet-link-to-headline saved-buffer saved-point status status-id)))

(defun org-twitter-add-tweet-link-to-headline (saved-buffer saved-point status status-id)
  (with-current-buffer saved-buffer
    (org-ml-update-headline-at* saved-point
      (org-ml-set-property :title (org-ml-build-secondary-string!
        (org-twitter-linkify-tweet status status-id)) it))))

(defun org-twitter-linkify-tweet (status status-id)
  ;; TODO: include actual username in link rather than underscore
  ;; (format "[[https://twitter.com/_/status/%s][%s]]" status-id status)
  (format "%s ([[https://twitter.com/_/status/%s][%s]])" status status-id org-twitter-tweet-link-description))

(aio-defun org-twitter-tweet-thread (tweets)
  (let ((in-reply-to-status-id)
        (status-ids '()))
    (while tweets
      (message "tweeting \"%s\"" (car tweets))
      (let ((response (aio-await (org-twitter-tweet (car tweets) in-reply-to-status-id))))
        (if (stringp response) (error response)
          (progn (setq in-reply-to-status-id (alist-get 'id_str response))
                 (push in-reply-to-status-id status-ids))))
      (setq tweets (cdr tweets)))
    status-ids))

(aio-defun org-twitter-tweet (tweet-text &optional in-reply-to-status-id)
  (aio-await
   (org-twitter-call-api 'update-status
                         (append `((status . ,tweet-text))
                                 (when in-reply-to-status-id
                                   `((in-reply-to-status-id .  ,in-reply-to-status-id)))))))

;; awaitable
;; don't need aio-defun since I'm manually creating/returning the promise
(defun org-twitter-call-api (command args-alist)
  (let ((promise (aio-promise)))
    (if (twittering-ensure-preparation-for-api-invocation)
        (twittering-call-api command
                             (append args-alist
                                     `((sentinel . ,(org-twitter-create-sentinel promise)))))
      (aio-resolve promise (lambda () (error "Authorization failed"))))
    promise))

(defun org-twitter-create-sentinel (promise)
  (lambda (proc status connection-info header-info)
    (let ((status-code (cdr (assq 'status-code header-info))))
      (case-string
       status-code
       (("200")
        (let ((json-response (twittering-json-read))) ;; don't understand this, but ok
          (aio-resolve promise (lambda () json-response))))
       (t
        (aio-resolve promise (lambda () (format "Got erroneous response: %s"
                                                (twittering-get-error-message header-info connection-info)))))))))

(provide 'org-twitter)
;;; org-twitter.el ends here
