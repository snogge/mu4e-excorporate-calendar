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

(defun mu4e-excorporate-calendar-accept-meeting ()
  "Accept the meeting in the *Article* buffer."
  (interactive)
  (with-current-buffer (get-buffer "*Article*")
    (mu4e-excorporate-calendar-meeting
     (with-suppressed-warnings ((obsolete gnus-icalendar-event))
       ;; gnus-icalendar-event is not actually obsolete, it's just the
       ;; defclass macro that sets variables of the same name as the
       ;; class as obsolete for reasons it seems no-one remembers.
       gnus-icalendar-event)
     (lambda () (message "++++++ meeting done")))))

(defun mu4e-excorporate-calendar--make-ical-event (icalendar-text)
  "Create a `gnus-icalendar-event' class object from ICALENDAR-TEXT."
  (with-temp-buffer
    (insert icalendar-text)
    (gnus-icalendar-event-from-buffer (current-buffer))))

(defun mu4e-excorporate-calendar-meeting (ical-event meeting-callback)
  "Find the exchange server meeting matching ICAL-EVENT.
Use the icalendar UID as the key.
Call MEETING-CALLBACK when completed."
  (message "--coan-mu4e-excp-meeting--")
  ;; Keep a local copy of the ical-event for the closures/lambdas.
  (let ((ical (clone ical-event)))
    (cl-destructuring-bind
        (_sec _min _hour day month year &rest) (decode-time (gnus-icalendar-event:start-time ical))
      ;; do something like exco-org-show-day to find the outlook appointment
      (exco-connection-iterate
       ;; intialize-function
       #'ignore
       ;; exco-connection-iterate per-connection function
       (lambda (identifier callback)
         ;; TODO: Better filtering is possible here, no need to
         ;; iterate over all of the days meetings.
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
            ;; TODO: Check some things before requesting the ical text
            ;; from the server Needs to be careful that the item on
            ;; the server has not been updated.  Fail in this case?
            (let ((item-ical (mu4e-excorporate-calendar--make-ical-event icalendar-text)))
              (when (string= (gnus-icalendar-event:uid ical)
                             (gnus-icalendar-event:uid item-ical))
                (message "Found %s" (gnus-icalendar-event:summary item-ical))
                ;; TODO: Pass the meeting data to the meeting-callback
                ))
            (funcall finalize))
          ;; exco-calendar-item-with-details-iterate finalize
          connection-finalizer))
       ;; exco-connection-iterate finalize function
       (lambda ()
         (funcall meeting-callback))
       ;; callback-will-call-finalize must be non-nil as the
       ;; per-connection callback does asynchronous calls, see the doc
       ;; of exco-connection-iterate
       t))))

(provide 'mu4e-excorporate-calendar)
;;; mu4e-excorporate-calendar.el ends here
