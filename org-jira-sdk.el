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
(require 'eieio)
(require 'dash)
(require 'cl-generic)

(defclass org-jira-sdk-record ()
  ((id :initarg :id :type string :required t)
   (data :initarg :data :documentation "The area to hold a big alist of data.")
   (hydrate-fn :initform (lambda (id) (message "Not implemented."))))
  "The ID of the record.")

(defun org-jira-sdk-string-but-first (s) (cl-subseq s 1))

(defun org-jira-sdk-to-string (s) (format "%s" s))

(defun org-jira-sdk-to-prefixed-string (s) (format "org-jira-sdk-%s" s))

(defun org-jira-sdk-record-type-to-symbol (record-type)
  (-> record-type symbol-name org-jira-sdk-string-but-first org-jira-sdk-to-prefixed-string intern))

(defun org-jira-sdk-create-from-id (record-type id &optional parent-id callback)
  (let ((rec (funcall (org-jira-sdk-record-type-to-symbol record-type)
                      :id (format "%s" id)
                      :parent-id parent-id)))
    (with-slots (data) rec
      (setf data (org-jira-sdk-hydrate rec callback))
      (org-jira-sdk-from-data rec))))

(defun org-jira-sdk-create-from-data (record-type data)
  (let ((rec (funcall (org-jira-sdk-record-type-to-symbol record-type) :data data)))
    (org-jira-sdk-from-data rec)))

(cl-defmethod org-jira-sdk-hydrate ((rec org-jira-sdk-record) &optional callback)
  "Populate the record with data from the remote endpoint."
  (with-slots (id hydrate-fn) rec
    (funcall hydrate-fn id callback)))

(cl-defgeneric org-jira-sdk-from-data ((rec org-jira-sdk-record)))

(cl-defmethod org-jira-sdk-dump ((rec org-jira-sdk-record))
  "A decent pretty print/object dump for working with the class items."
  (let ((slots (mapcar (lambda (slot) (aref slot 1)) (eieio-class-slots (eieio-object-class rec)))))
    (setq slots (cl-remove-if (lambda (s) (not (slot-boundp rec s))) slots))
    (apply #'concat
     (mapcar (lambda (slot)
               (let ((slot (intern (org-jira-sdk-to-string slot))))
                 (format "\n%+16s:   %s" slot (slot-value rec (intern (org-jira-sdk-to-string slot)))))
               )
             slots))))

(defun org-jira-sdk-path (alist key-chain)
  "Query a nested path in some type of ALIST by traversing down the keys of KEY-CHAIN."
  (cl-reduce (lambda (a k) (alist-get k a)) key-chain :initial-value alist))

(defclass org-jira-sdk-issue (org-jira-sdk-record)
  ((assignee :type (or null string) :initarg :assignee)
   (components :type string :initarg :components)
   (labels :type string :initarg :labels)
   (created :type string :initarg :created)
   (description :type (or null string) :initarg :description)
   (duedate :type (or null string) :initarg :duedate)
   (headline :type string :initarg :headline)
   (id :type string :initarg :id)       ; TODO: Probably remove me
   (issue-id :type string :initarg :issue-id :documentation "The common ID/key, such as EX-1.")
   (issue-id-int :type string :initarg :issue-id-int :documentation "The internal Jira ID, such as 12345.")
   (filename :type (or null string) :initarg :filename :documentation "The filename to write issue to.")
   (parent-key :type (or null string) :initarg :parent-key :documentation "The parent issue key if there is one")
   (priority :type (or null string) :initarg :priority)
   (proj-key :type string :initarg :proj-key)
   (reporter :type (or null string) :initarg :reporter)
   (resolution :type (or null string) :initarg :resolution)
   (sprint :type (or null string) :initarg :sprint)
   (start-date :type (or null string) :initarg :start-date)
   (status :type string :initarg :status)
   (summary :type string :initarg :summary)
   (type :type string :initarg :type)
   (type-id :type string :initarg :type-id)
   (updated :type string :initarg :updated)
   (data :initarg :data :documentation "The remote Jira data object (alist).")
   (hydrate-fn :initform #'jiralib-get-issue :initarg :hydrate-fn))
  "An issue on the end.  ID of the form EX-1, or a numeric such as 10000.")

(defclass org-jira-sdk-comment (org-jira-sdk-record)
  ((author :type string :initarg :author)
   (body :type string :initarg :body)
   (comment-id :type string :initarg :comment-id :documentation "The comment ID, such as 12345.")
   (created :type string :initarg :created)
   (headline :type string :initarg :headline)
   (parent-id :type string :initarg :parent-id :documentation "The parent issue-id such as EX-1.")
   (updated :type string :initarg :updated)
   (data :initarg :data :documentation "The reomte Jira data object (alist).")
   (hydrate-fn :initform #'jiralib-get-comment :initarg :hydrate-fn)))

(defclass org-jira-sdk-board (org-jira-sdk-record)
  ((name :type string :initarg :name :required t)
   (url  :type string :initarg :url  :required t)
   (board-type :type string :initarg :board-type)
   (jql  :type string :initarg :jql)
   (limit :type integer :initarg :limit)
   ;; unused
   (parent-id :type string :initarg :parent-id)
   (hydrate-fn :initform #'jiralib-get-board :initarg :hydrate-fn)))

(cl-defmethod org-jira-sdk-hydrate ((rec org-jira-sdk-comment) &optional callback)
  "Populate the record with data from the remote endpoint."
  (with-slots (id proj-key hydrate-fn) rec
    (funcall hydrate-fn proj-key id callback)))

(cl-defmethod org-jira-sdk-from-data ((rec org-jira-sdk-issue))
  (cl-flet ((path (keys) (org-jira-sdk-path (oref rec data) keys)))
    (org-jira-sdk-issue
     :assignee (path '(fields assignee displayName))
     :components (mapconcat (lambda (c) (org-jira-sdk-path c '(name))) (path '(fields components)) ", ")
     :labels (mapconcat (lambda (c) (format "%s" c)) (mapcar #'identity (path '(fields labels))) ", ")
     :created (path '(fields created))     ; confirm
     :description (or (path '(renderedFields description)) "")
     :duedate (or (path '(fields sprint endDate)) (path '(fields duedate)))         ; confirm
     :filename (path '(fields project key))
     :headline (path '(fields summary)) ; Duplicate of summary, maybe different.
     :id (path '(key))
     :issue-id (path '(key))
     :issue-id-int (path '(id))
     :parent-key (path '(fields parent key))
     :priority (path '(fields priority name))
     :proj-key (path '(fields project key))
     :reporter (path '(fields reporter displayName)) ; reporter could be an object of its own slot values
     :resolution (path '(fields resolution name))  ; confirm
     :sprint (path '(fields sprint name))
     :start-date (path '(fields start-date))  ; confirm
     :status (org-jira-decode (path '(fields status name)))
     :summary (path '(fields summary))
     :type (path '(fields issuetype name))
     :type-id (path '(fields issuetype id))
     :updated (path '(fields updated))  ; confirm
     ;; TODO: Remove this
     ;; :data (oref rec data)
     )))

(cl-defmethod org-jira-sdk-from-data ((rec org-jira-sdk-comment))
  (cl-flet ((path (keys) (org-jira-sdk-path (oref rec data) keys)))
    (org-jira-sdk-comment
     :author (path '(author displayName))
     :body (path '(body))
     :comment-id (path '(id))
     :created (path '(created))
     :headline (format "Comment: %s" (path '(author displayName)))
     :parent-id (if (slot-boundp rec 'parent-id) (oref rec parent-id) "")
     :updated (path '(updated))
     ;; TODO: Remove this
     ;; :data (oref rec data)
     )))

(cl-defmethod org-jira-sdk-from-data ((rec org-jira-sdk-board))
  (with-slots (data) rec
    (org-jira-sdk-board
     :id   (int-to-string (alist-get 'id data))
     :name (alist-get 'name data)
     :url  (alist-get 'self data)
     ;; TODO: remove it? used by org-jira-sdk-create-from-id
     :parent-id ""
     :board-type (alist-get 'type data))))

;; Issue
(defun org-jira-sdk-create-issue-from-data (d) (org-jira-sdk-create-from-data :issue d))
(defun org-jira-sdk-create-issues-from-data-list (ds) (mapcar #'org-jira-sdk-create-issue-from-data ds))

;; Issue with custom filename setting
(defun org-jira-sdk-create-issue-from-data-with-filename (filename)
  (lambda (d)
    (let ((Issue (org-jira-sdk-create-from-data :issue d)))
      (setf (oref Issue filename) filename)
      Issue)))

(defun org-jira-sdk-create-issues-from-data-list-with-filename (filename ds)
  (let ((fn (org-jira-sdk-create-issue-from-data-with-filename filename)))
    (mapcar fn ds)))

;; Comment
(defun org-jira-sdk-create-comment-from-data (d) (org-jira-sdk-create-from-data :comment d))
(defun org-jira-sdk-create-comments-from-data-list (ds) (mapcar #'org-jira-sdk-create-comment-from-data ds))

(defun org-jira-sdk-isa-record? (i) (cl-typep i 'org-jira-sdk-record))
(defun org-jira-sdk-isa-issue? (i) (cl-typep i 'org-jira-sdk-issue))
(defun org-jira-sdk-isa-comment? (i) (cl-typep i 'org-jira-sdk-comment))

;; Board
(defun org-jira-sdk-create-board-from-data (d) (org-jira-sdk-create-from-data :board d))
(defun org-jira-sdk-create-boards-from-data-list (ds) (mapcar #'org-jira-sdk-create-board-from-data ds))

(provide 'org-jira-sdk)

;;; org-jira-sdk.el ends here
