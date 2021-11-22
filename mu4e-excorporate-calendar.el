;;; mu4e-excorporate-calendar.el --- Outlook appointments in mu4e 1.6+  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Ola Nilsson <ola.nilsson@gmail.com>

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Keywords: mail, calendar, excorporate, outlook, ical, mu4e
;; Package-Requires: ((mu4e "1.6") (excorporate "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Respond to outlook calendar invites from mu4e (gnus) article view.

;;; Code:

(require 'gnus-icalendar)
(require 'excorporate)

(defun mu4e-excorporate-calendar-meeting ()
  "Find the meeting on the exchange server.
Find meeting described in the attached icalendar file on the
server.  Use the icalendar UID as the key."
  (interactive)
  (cl-assert (derived-mode-p 'mu4e-view-mode))
  (unless gnus-icalendar-event
    (user-error "No calendar event (gnus-icalendar-event)"))
  ;; find ical data
  (message "--coan-mu4e-excp-meeting--")
  ;; Keep a local copy of gnus-icalendar-event for the closures/lambdas.
  (let ((ical (clone gnus-icalendar-event)))
    (cl-destructuring-bind
        (_sec _min _hour day month year &rest) (decode-time (gnus-icalendar-event:start-time ical))
      ;; do something like exco-org-show-day to find the outlook appointment
      (exco-connection-iterate
       ;; intialize-function
       #'ignore
       ;; exco-connection-iterate per-connection function
       (lambda (identifier callback)
         (exco-get-meetings-for-day identifier
                                    month day year
                                    callback))
       ;; exco-connection-iterate per-connection callback
       (lambda (identifier response connection-finalizer)
         (exco-calendar-item-with-details-iterate
          identifier response
          ;; exco-calendar-item-with-details-iterate callback
          (lambda (finalize
    			   _subject _start _end _location
    			   _main-invitees _optional-invitees
    			   icalendar-text)
            (let ((item-ical (with-temp-buffer
                               (insert icalendar-text)
                               (gnus-icalendar-event-from-buffer (current-buffer)))))
              (when (string= (gnus-icalendar-event:uid ical)
                             (gnus-icalendar-event:uid item-ical))
                (message "Found %s" (gnus-icalendar-event:summary item-ical))

                ))
            (funcall finalize))
          ;; exco-calendar-item-with-details-iterate finalize
          connection-finalizer))
       ;; exco-connection-iterate finalize function
       #'ignore ; (lambda () (message "exco finalize")))
       ;; callback-will-call-finalize must be non-nil as the
       ;; per-connection callback does asynchronous calls, see the doc
       ;; of exco-connection-iterate
       t))))

(provide 'mu4e-excorporate-calendar)
;;; mu4e-excorporate-calendar.el ends here
