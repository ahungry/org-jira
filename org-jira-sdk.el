;;; org-jira-sdk.el -- SDK Layer for entities

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>
;;
;; Authors:
;; Matthew Carter <m@ahungry.com>
;;
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/org-jira
;; Version: 3.1.1
;; Keywords: ahungry jira org bug tracker
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (request "0.2.0") (s "0.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/> or write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This provides an SDK wrapper for more strictly defining the entities we interact with.

;;; Code:

(require 'jiralib)
(require 'cl-lib)
(require 's)
(require 'eieio)
(require 'dash)

(defclass org-jira-sdk-record ()
  ((id :initarg :id :type string :required t)
   (hfn :initform (lambda (id) (message "Not implemented."))))
  "The ID of the record.")

(defun org-jira-sdk-record-type-to-symbol (record-type)
  "Convert RECORD-TYPE such as :record to org-jira-sdk-record."
  (intern (format "org-jira-sdk-%s" (cl-subseq (symbol-name record-type) 1))))

(defun org-jira-sdk-new (record-type id)
  "Create a new eieio class of RECORD-TYPE bound to ID."
  (funcall (org-jira-sdk-record-type-to-symbol record-type) :id (format "%s" id)))

(cl-defmethod org-jira-sdk-hydrate ((rec org-jira-sdk-record))
  "Populate the record with data from the remote endpoint."
  (with-slots (id hfn) rec
      (funcall hfn id)))

(provide 'org-jira-sdk)
;;; org-jira-sdk.el ends here
