;;; org-twitter.el --- Tweet from org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Ketan Agrawal
;;
;; Author: Ketan Agrawal <http://github.com/ketan0>
;; Maintainer: Ketan Agrawal <ketanjayagrawal@gmail.com>
;; Created: October 13, 2020
;; Modified: November 21, 2020
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
  "Play twitter songs through org mode interfaces."
  :group 'org
  :prefix "org-twitter-"
  :link '(url-link :tag "Github" "https://github.com/ketan0/org-twitter"))

(aio-defun org-twitter-tweet-selection ()
  "Interactively tweet the currently selected text."
  (interactive)
  (let ((status (buffer-substring-no-properties
                 (region-beginning)
                 (region-end))))
    (org-twitter-tweet status)))

(aio-defun org-twitter-tweet-this-headline (saved-point)
  "Interactively tweet the currently selected text."
  (interactive "d")
  (let* ((saved-buffer (current-buffer))
         (headline (org-ml-parse-this-headline))
         (status (org-ml-get-property :raw-value headline)))
    (org-twitter-tweet status)))

(aio-defun org-twitter-tweet-thread (tweets)
  (let ((in-reply-to-status-id))
    (while tweets
      (let ((response (aio-await (org-twitter-tweet (car tweets) in-reply-to-status-id))))
        (if (stringp response) (error response)
          (setq in-reply-to-status-id (alist-get 'id_str response))))
      (setq tweets (cdr tweets)))))

(aio-defun org-twitter-tweet (tweet-text &optional in-reply-to-status-id)
  (let ((promise (aio-promise)))
    (twittering-call-api 'update-status
                         (append `((status . ,tweet-text)
                                   (sentinel . ,(org-twitter-create-tweet-sentinel promise)))
                                 (when in-reply-to-status-id
                                   `((in-reply-to-status-id .  ,in-reply-to-status-id)))))
    promise))

(defun org-twitter-create-tweet-sentinel (promise)
  (lambda (proc status connection-info header-info)
    (let ((status-code (cdr (assq 'status-code header-info))))
      (case-string
       status-code
       (("200")
        (let ((json-response (twittering-json-read)))
          (aio-resolve promise (lambda () json-response))))
       (t
        (aio-resolve promise (lambda () (format "Response: %s"
                                                (twittering-get-error-message header-info connection-info)))))))))

(provide 'org-twitter)
;;; org-twitter.el ends here
