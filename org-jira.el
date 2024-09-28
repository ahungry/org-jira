;;; org-jira.el --- Syncing between Jira and Org-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2016-2022 Matthew Carter <m@ahungry.com>
;; Copyright (C) 2011 Bao Haojun
;;
;; Authors:
;; Matthew Carter <m@ahungry.com>
;; Bao Haojun <baohaojun@gmail.com>
;;
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/org-jira
;; Version: 4.3.3
;; Keywords: ahungry jira org bug tracker
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (request "0.2.0") (dash "2.14.1"))

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

;; This provides an extension to org-mode for syncing issues with JIRA
;; issue servers.

;;; News:

;;;; Changes in 4.4.2
;; - Fix for org-insert-subheading behavior change in org 9.7+ in render-issues

;;;; Changes in 4.4.1
;; - Fix tag (4.3.3 was out of order - we had a 4.4.0 on repo)
;; - Fix for some crazy scoping issue in the org-jira-get-issue-val-from-org function

;;;; Changes in 4.3.3:
;; - Address issue with assignee property being removed when Unassigned

;;;; Changes in 4.3.2:
;; - Fixes issues with org-jira-add-comment and org-jira-update-comment

;;;; Changes in 4.3.1:
;; - Fix to make custom-jql results sync worklogs properly.

;;;; Changes in 4.3.0:
;; - Allow org-jira-set-issue-reporter call to dynamically set this value.

;;;; Changes in 4.1.0:
;; - Allow custom-jql to be specified and render in special files (see: README.md).

;;;; Changes in 4.0.0:
;; - Introduce SDK type for handling records vs random alist structures.

;;;; Changes since 3.1.0:
;; - Fix how we were ruining the kill-ring with kill calls.

;;;; Changes since 3.0.0:
;; - Add new org-jira-add-comment call (C-c c c)

;;;; Changes since 2.8.0:
;; - New version 3.0.0 deprecates old filing mechanism and files
;;   all of the changes under the top level ticket header.
;; - If you want other top level headers in the same file, this should
;;   work now, as long as they come after the main project one.

;;;; Changes since 2.7.0:
;; - Clean up multi-buffer handling, disable attachments call until
;; - refresh is compatible with it.

;;;; Changes since 2.6.3:
;; - Insert worklog import filter in the existing org-jira-update-worklogs-for-current-issue function
;; - Sync up org-clocks and worklogs!  Set org-jira-worklog-sync-p to nil to avoid.

;;;; Changes since 2.6.1:
;; - Fix bug with getting all issues when worklog is an error trigger.

;;;; Changes since 2.5.4:
;; - Added new org-jira-refresh-issues-in-buffer call and binding

;;;; Changes since 2.5.3:
;; - Re-introduce the commit that introduced a break into Emacs 25.1.1 list/array push
;;     The commit caused updates/comment updates to fail when a blank list of components
;;     was present - it will now handle both cases (full list, empty list).

;;;; Changes since 2.5.2:
;; - Revert a commit that introduced a break into Emacs 25.1.1 list/array push
;;     The commit caused updates/comment updates to fail

;;;; Changes since 2.5.1:
;; - Only set duedate if a DEADLINE is present in the tags and predicate is t

;;;; Changes since 2.5.0:
;; - Allow overriding the org property names with new defcustom

;;;; Changes since 2.4.0:
;; - Fix many deprecation/warning issues
;; - Fix error with allow-other-keys not being wrapped in cl-function

;;;; Changes since 2.3.0:
;; - Integration with org deadline and Jira due date fields

;;;; Changes since 2.2.0:
;; - Selecting issue type based on project key for creating issues

;;;; Changes since 2.1.0:
;; - Allow changing to unassigned user
;; - Add new shortcut for quick assignment

;;;; Changes since 2.0.0:
;; - Change statusCategory to status value
;; - Clean out some redundant code
;; - Add ELPA tags in keywords

;;;; Changes since 1.0.1:
;; - Converted many calls to async
;; - Removed minor annoyances (position resets etc.)

;;; Code:

(eval-when-compile (require 'cl))
(require 'org)
(require 'org-clock)
(require 'cl-lib)
(require 'url)
(require 'ls-lisp)
(require 'dash)
(require 'jiralib)
(require 'org-jira-sdk)

(defconst org-jira-version "4.4.2"
  "Current version of org-jira.el.")

(defgroup org-jira nil
  "Customization group for org-jira."
  :tag "Org JIRA"
  :group 'org)

(defcustom org-jira-working-dir "~/.org-jira"
  "Folder under which to store org-jira working files."
  :group 'org-jira
  :type 'directory)

(defcustom org-jira-project-filename-alist nil
  "Alist translating project keys to filenames.

Each element has a structure:

  (PROJECT-KEY . NEW-FILE-NAME)

where both are strings.  NEW-FILE-NAME is relative to
`org-jira-working-dir'."
  :group 'org-jira
  :type '(alist :key-type (string :tag "Project key")
                :value-type (string :tag "File name for this project")))

(defcustom org-jira-default-jql
  "assignee = currentUser() and resolution = unresolved ORDER BY
  priority DESC, created ASC"
  "Default jql for querying your Jira tickets."
  :group 'org-jira
  :type 'string)

(defcustom org-jira-ignore-comment-user-list
  '("admin")
  "Jira usernames that should have comments ignored."
  :group 'org-jira
  :type '(repeat (string :tag "Jira username:")))

(defcustom org-jira-reverse-comment-order nil
  "If non-nil, order comments from most recent to least recent."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-done-states
  '("Closed" "Resolved" "Done")
  "Jira states that should be considered as DONE for `org-mode'."
  :group 'org-jira
  :type '(repeat (string :tag "Jira state name:")))

(defcustom org-jira-users
  '(("Full Name" . "account-id"))
  "A list of displayName and key pairs."
  :group 'org-jira
  :type 'list)

(defcustom org-jira-progress-issue-flow
  '(("To Do" . "In Progress")
    ("In Progress" . "Done"))
  "Quickly define a common issue flow."
  :group 'org-jira
  :type 'list)

(defcustom org-jira-property-overrides (list)
  "An assoc list of property tag overrides.

The KEY . VAL pairs should both be strings.

For instance, to change the :assignee: property in the org :PROPERTIES:
block to :WorkedBy:, you can set this as such:

  (setq org-jira-property-overrides (list (cons \"assignee\" \"WorkedBy\")))

or simply:

  (add-to-list (quote org-jira-property-overrides)
               (cons (\"assignee\" \"WorkedBy\")))

This will work for most any of the properties, with the
exception of ones with unique functionality, such as:

  - deadline
  - summary
  - description"
  :group 'org-jira
  :type 'list)

(defcustom org-jira-serv-alist nil
  "Association list to set information for each jira server.
Each element of the alist is a jira server name.  The CAR of each
element is a string, uniquely identifying the server.  The CDR of
each element is a well-formed property list with an even number
of elements, alternating keys and values, specifying parameters
for the server.

     (:property value :property value ... )

When a property is given a value in org-jira-serv-alist, its
setting overrides the value of the corresponding user
variable (if any) during syncing.

Most properties are optional, but some should always be set:

  :url        soap url of the jira server.
  :username   username to be used.
  :host       hostname of the jira server (TODO: compute it from ~url~).

All the other properties are optional.  They override the global
variables.

  :password   password to be used, will be prompted if missing."
  :group 'org-jira
  :type '(alist :value-type plist))

(defcustom org-jira-use-status-as-todo nil
  "Use the JIRA status as the TODO tag value."
  :group 'org-jira)

(defcustom org-jira-deadline-duedate-sync-p t
  "Keep org deadline and jira duedate fields synced.
You may wish to set this to nil if you track org deadlines in
your buffer that you do not want to send back to your Jira
instance."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-worklog-sync-p t
  "Keep org clocks and jira worklog fields synced.
You may wish to set this to nil if you track org clocks in
your buffer that you do not want to send back to your Jira
instance."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-download-dir "~/Downloads"
  "Name of the default jira download library."
  :group 'org-jira
  :type 'string)

(defcustom org-jira-download-ask-override t
  "Ask before overriding tile."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-jira-status-to-org-keyword-alist nil
  "Custom alist of jira status stored in car and `org-mode' keyword stored in cdr."
  :group 'org-jira
  :type '(alist :key-type string :value-type string))

(defcustom org-jira-priority-to-org-priority-omit-default-priority nil
  "Whether to omit insertion of priority when it matches the default.

When set to t, will omit the insertion of the matched value from
`org-jira-priority-to-org-priority-alist' when it matches the `org-default-priority'."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-priority-to-org-priority-alist nil
  "Alist mapping jira priority keywords to `org-mode' priority cookies.

A sample value might be
  (list (cons \"High\" ?A)
        (cons \"Medium\" ?B)
        (cons \"Low\" ?C)).

See `org-default-priority' for more info."
  :group 'org-jira
  :type '(alist :key-type string :value-type character))

(defcustom org-jira-boards-default-limit 50
  "Default limit for number of issues retrieved from agile boards."
  :group 'org-jira
  :type 'integer)

;; FIXME: Issue with using this - issues are grouped under a headline incorrectly.
(defcustom org-jira-custom-jqls
  '(
    (:jql " assignee = currentUser() and createdDate < '2019-01-01' order by created DESC "
          :limit 100
          :filename "last-years-work")
    (:jql " assignee = currentUser() and createdDate >= '2019-01-01' order by created DESC "
          :limit 100
          :filename "this-years-work")
    )
  "A list of plists with :jql and :filename keys to run arbitrary user JQL."
  :group 'org-jira
  :type '(alist :value-type plist))

(defcustom org-jira-download-comments t
  "Set to nil if you don't want to update comments during issue rendering."
  :group 'org-jira
  :type 'boolean)

(defcustom org-jira-update-issue-details-include-reporter t
  "For Jira Cloud API we will get an error if `reporter' is sent with an update request."
  :group 'org-jira
  :type 'string)

(defvar org-jira-serv nil
  "Parameters of the currently selected blog.")

(defvar org-jira-serv-name nil
  "Name of the blog, to pick from `org-jira-serv-alist'.")

(defvar org-jira-projects-list nil
  "List of jira projects.")

(defvar org-jira-current-project nil
  "Currently selected (i.e., active project).")

(defvar org-jira-issues-list nil
  "List of jira issues under the current project.")

(defvar org-jira-server-rpc-url nil
  "Jira server soap URL.")

(defvar org-jira-server-userid nil
  "Jira server user id.")

(defvar org-jira-proj-id nil
  "Jira project ID.")

(defvar org-jira-logged-in nil
  "Flag whether user is logged-in or not.")

(defvar org-jira-buffer-name "*org-jira-%s*"
  "Name of the jira buffer.")

(defvar org-jira-buffer-kill-prompt t
  "Ask before killing buffer.")

(make-variable-buffer-local 'org-jira-buffer-kill-prompt)

(defvar org-jira-mode-hook nil
  "Hook to run upon entry into mode.")

(defvar org-jira-issue-id-history '()
  "Prompt history for issue id.")

(defvar org-jira-fixversion-id-history '()
  "Prompt history for fixversion id.")

(defvar org-jira-verbosity 'debug)

(defun org-jira-log (s) (when (eq 'debug org-jira-verbosity) (message "%s" s)))

(defmacro ensure-on-issue (&rest body)
  "Make sure we are on an issue heading, before executing BODY."
  (declare (debug t)
           (indent 0))
  `(save-excursion
    (save-restriction
      (widen)
      (unless (looking-at "^\\*\\* ")
        (search-backward-regexp "^\\*\\* " nil t)) ; go to top heading
      (let ((org-jira-id (org-jira-id)))
        (unless (and org-jira-id (string-match (jiralib-get-issue-regexp) (downcase org-jira-id)))
          (error "Not on an issue region!")))
      ,@body)))

(defmacro org-jira-with-callback (&rest body)
  "Simpler way to write the data BODY callbacks."
  (declare (debug t)
           (indent 0))
  `(lambda (&rest request-response)
     (declare (ignore cb-data))
     (let ((cb-data (cl-getf request-response :data)))
       ,@body)))

(defmacro org-jira-freeze-ui (&rest body)
  "Freeze the UI layout for the user as much as possible."
  (declare (debug t)
           (indent 0))
  `(save-excursion
     (save-restriction
       (widen)
       (org-save-outline-visibility t
         (outline-show-all)
         ,@body))))

(defvar org-jira-proj-key-override nil
  "String.  An override for the proj-key.  Set to nil to restore old behavior.")

;; We want some hooking system to override default-jql + this.
(defun org-jira--get-proj-key (issue-id)
  "Get the proper proj-key.  Typically derived from ISSUE-ID."
  (if org-jira-proj-key-override org-jira-proj-key-override
    (replace-regexp-in-string "-.*" "" issue-id)))

(defun org-jira--get-proj-key-from-issue (Issue)
  "Get the proper proj-key from an ISSUE instance."
  (oref Issue filename))

;; TODO: Merge these 3 ensure macros (or, scrap all but ones that work on Issue)
(defmacro ensure-on-issue-id (issue-id &rest body)
  "Just do some work on ISSUE-ID, execute BODY."
  (declare (debug t)
           (indent 1))
  (let ((issue-id-var (make-symbol "issue-id")))
    `(let* ((,issue-id-var ,issue-id)
            (proj-key (org-jira--get-proj-key ,issue-id-var))
            (project-file (org-jira--get-project-file-name proj-key))
            (project-buffer (or (find-buffer-visiting project-file)
                                (find-file project-file))))
       (with-current-buffer project-buffer
         (org-jira-freeze-ui
           (let ((p (org-find-entry-with-id ,issue-id-var)))
             (unless p (error "Issue %s not found!" ,issue-id-var))
             (goto-char p)
             (org-narrow-to-subtree)
             ,@body))))))

(defmacro ensure-on-issue-id-with-filename (issue-id filename &rest body)
  "Just do some work on ISSUE-ID, execute BODY."
  (declare (debug t)
           (indent 1))
  (let ((issue-id-var (make-symbol "issue-id"))
        (filename-var (make-symbol "filename")))
    `(let* ((,issue-id-var ,issue-id)
            (,filename-var ,filename)
            (proj-key ,filename-var)
            (project-file (org-jira--get-project-file-name proj-key))
            (project-buffer (or (find-buffer-visiting project-file)
                                (find-file project-file))))
       (with-current-buffer project-buffer
         (org-jira-freeze-ui
           (let ((p (org-find-entry-with-id ,issue-id-var)))
             (unless p (error "Issue %s not found!" ,issue-id-var))
             (goto-char p)
             (org-narrow-to-subtree)
             ,@body))))))

(defmacro ensure-on-issue-Issue (Issue &rest body)
  "Just do some work on ISSUE, execute BODY."
  (declare (debug t)
           (indent 1))
  (let ((Issue-var (make-symbol "Issue")))
    `(let ((,Issue-var ,Issue))
         (with-slots (issue-id) ,Issue-var
           (let* ((proj-key (org-jira--get-proj-key-from-issue ,Issue-var))
                  (project-file (org-jira--get-project-file-name proj-key))
                  (project-buffer (or (find-buffer-visiting project-file)
                                      (find-file project-file))))
             (with-current-buffer project-buffer
               (org-jira-freeze-ui
                 (let ((p (org-find-entry-with-id issue-id)))
                   (unless p (error "Issue %s not found!" issue-id))
                   (goto-char p)
                   (org-narrow-to-subtree)
                   ,@body))))))))

(defmacro ensure-on-todo (&rest body)
  "Make sure we are on an todo heading, before executing BODY."
  (declare (debug t)
           (indent 0))
  `(save-excursion
     (save-restriction
       (let ((continue t)
             (on-todo nil))
         (while continue
           (when (org-get-todo-state)
             (setq continue nil on-todo t))
           (unless (and continue (org-up-heading-safe))
             (setq continue nil)))
         (if (not on-todo)
             (error "TODO not found")
           (org-narrow-to-subtree)
           ,@body)))))

(defmacro ensure-on-comment (&rest body)
  "Make sure we are on a comment heading, before executing BODY."
  (declare (debug t)
           (indent 0))
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Comment:")
       (error "Not on a comment region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defmacro ensure-on-worklog (&rest body)
  "Make sure we are on a worklog heading, before executing BODY."
  (declare (debug t)
           (indent 0))
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Worklog:")
       (error "Not on a worklog region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defun org-jira--ensure-working-dir ()
  "Ensure that the org-jira-working-dir exists"
  (unless (file-exists-p org-jira-working-dir)
    (error (format "org-jira directory does not exist! Run (make-directory \"%s\")" org-jira-working-dir))
    )
  org-jira-working-dir
  )

(defvar org-jira-entry-mode-map
  (let ((org-jira-map (make-sparse-keymap)))
    (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
    (define-key org-jira-map (kbd "C-c bg") 'org-jira-get-boards)
    (define-key org-jira-map (kbd "C-c iv") 'org-jira-get-issues-by-board)
    (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
    (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
    (define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
    (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
    ;;(define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
    ;;(define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
    (define-key org-jira-map (kbd "C-c il") 'org-jira-update-issue-labels)
    (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
    (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
    (define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
    (define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
    ;(define-key org-jira-map (kbd "C-c isr") 'org-jira-set-issue-reporter)
    (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
    (define-key org-jira-map (kbd "C-c iR") 'org-jira-refresh-issues-in-buffer)
    (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
    (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
    (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
    (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
    (define-key org-jira-map (kbd "C-c cc") 'org-jira-add-comment)
    (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
    (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklogs-from-org-clocks)
    (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
    (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-by-fixversion)
    org-jira-map))

;;;###autoload
(define-minor-mode org-jira-mode
  "Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'."

  :init-value nil
  :lighter " jira"
  :group 'org-jira
  :keymap org-jira-entry-mode-map

  (if org-jira-mode
      (progn
        (set (make-local-variable 'org-element-use-cache) nil)
        (run-mode-hooks 'org-jira-mode-hook))
    (progn
      (kill-local-variable 'org-element-use-cache))))

(defun org-jira-maybe-activate-mode ()
  "Having hooks can be an expensive operation, and they are invoked each
time the mode is started - we should only ever re-activate the mode if
it isn't already on."
  (unless (bound-and-true-p org-jira-mode) (org-jira-mode t)))

(defun org-jira-get-project-name (proj)
  (org-jira-find-value proj 'key))

(defun org-jira-find-value (l &rest keys)
  (let* (key exists)
    (while (and keys (listp l))
      (setq key (car keys))
      (setq exists nil)
      (mapc (lambda (item)
              (when (equal key (car item))
                (setq exists t)))
            (if (and (listp l)
                     (listp (car l)))
                l
              nil))
      (setq keys (cdr keys))
      (if exists
          (setq l (cdr (assoc key l)))
        (setq l (or (cdr (assoc key l)) l))))
    l))

(defun org-jira--get-project-file-name (project-key)
  "Translate PROJECT-KEY into filename."
  (-if-let (translation (cdr (assoc project-key org-jira-project-filename-alist)))
      (expand-file-name translation (org-jira--ensure-working-dir))
    (expand-file-name (concat project-key ".org") (org-jira--ensure-working-dir))))

(defun org-jira-get-project-lead (proj)
  (org-jira-find-value proj 'lead 'name))

;; This is mapped to accountId and not username, so we need nil not blank string.
(defun org-jira-get-assignable-users (project-key)
  "Get the list of assignable users for PROJECT-KEY, adding user set jira-users first."
  (append
   '(("Unassigned" . nil))
   org-jira-users
   (mapcar (lambda (user)
             (cons (org-jira-decode (cdr (assoc 'displayName user)))
                   (org-jira-decode (cdr (assoc 'accountId user)))))
           (jiralib-get-users project-key))))

(defun org-jira-get-reporter-candidates (project-key)
  "Get the list of assignable users for PROJECT-KEY, adding user set jira-users first."
  (append
   org-jira-users
   (mapcar (lambda (user)
             (cons (org-jira-decode (cdr (assoc 'displayName user)))
                   (org-jira-decode (cdr (assoc 'accountId user)))))
           (jiralib-get-users project-key))))

(defun org-jira-entry-put (pom property value)
  "Similar to org-jira-entry-put, but with an optional alist of overrides.

At point-or-marker POM, set PROPERTY to VALUE.

Look at customizing org-jira-property-overrides if you want
to change the property names this sets."
  (unless (stringp property)
    (setq property (symbol-name property)))
  (let ((property (or (assoc-default property org-jira-property-overrides)
                      property)))
    (org-entry-put pom property (org-jira-decode value))))

(defun org-jira-kill-line ()
  "Kill the line, without 'kill-line' side-effects of altering kill ring."
  (interactive)
  (delete-region (point) (line-end-position)))

;; Appropriated from org.el
(defun org-jira-org-kill-line (&optional _arg)
  "Kill line, to tags or end of line."
  (cond
   ((or (not org-special-ctrl-k)
        (bolp)
        (not (org-at-heading-p)))
    (when (and (get-char-property (min (point-max) (point-at-eol)) 'invisible)
               org-ctrl-k-protect-subtree
               (or (eq org-ctrl-k-protect-subtree 'error)
                   (not (y-or-n-p "Kill hidden subtree along with headline? "))))
      (user-error "C-k aborted as it would kill a hidden subtree"))
    (call-interactively
     (if (bound-and-true-p visual-line-mode) 'kill-visual-line 'org-jira-kill-line)))
   ((looking-at ".*?\\S-\\([ \t]+\\(:[[:alnum:]_@#%:]+:\\)\\)[ \t]*$")
    (delete-region (point) (match-beginning 1))
    (org-set-tags nil t))
   (t (delete-region (point) (point-at-eol)))))

;;;###autoload
(defun org-jira-get-projects ()
  "Get list of projects."
  (interactive)
  (let ((projects-file (expand-file-name "projects-list.org" (org-jira--ensure-working-dir))))
    (or (find-buffer-visiting projects-file)
        (find-file projects-file))
    (org-jira-maybe-activate-mode)
    (save-excursion
      (let* ((oj-projs (jiralib-get-projects)))
        (mapc (lambda (proj)
                (let* ((proj-key (org-jira-find-value proj 'key))
                       (proj-headline (format "Project: [[file:%s.org][%s]]" proj-key proj-key)))
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (outline-show-all)
                    (setq p (org-find-exact-headline-in-buffer proj-headline))
                    (if (and p (>= p (point-min))
                             (<= p (point-max)))
                        (progn
                          (goto-char p)
                          (org-narrow-to-subtree)
                          (end-of-line))
                      (goto-char (point-max))
                      (unless (looking-at "^")
                        (insert "\n"))
                      (insert "* ")
                      (org-jira-insert proj-headline)
                      (org-narrow-to-subtree))
                    (org-jira-entry-put (point) "name" (org-jira-get-project-name proj))
                    (org-jira-entry-put (point) "key" (org-jira-find-value proj 'key))
                    (org-jira-entry-put (point) "lead" (org-jira-get-project-lead proj))
                    (org-jira-entry-put (point) "ID" (org-jira-find-value proj 'id))
                    (org-jira-entry-put (point) "url" (format "%s/browse/%s" (replace-regexp-in-string "/*$" "" jiralib-url) (org-jira-find-value proj 'key))))))
              oj-projs)))))

(defun org-jira-get-issue-components (issue)
  "Return the components the ISSUE belongs to."
  (mapconcat
   (lambda (comp)
     (org-jira-find-value comp 'name))
   (org-jira-find-value issue 'fields 'components) ", "))

(defun org-jira-get-issue-labels (issue)
  "Return the labels the ISSUE belongs to."
  (org-jira-find-value issue 'fields 'labels))

(defun org-jira-decode (data)
  "Decode text DATA.

It must receive a coercion to string, as not every time will it
be populated."
  (decode-coding-string
   (cl-coerce data 'string) jiralib-coding-system))

(defun org-jira-insert (&rest args)
  "Set coding to text provide by `ARGS' when insert in buffer."
  (insert (org-jira-decode (apply #'concat args))))

(defun org-jira-transform-time-format (jira-time-str)
  "Convert JIRA-TIME-STR to format \"%Y-%m-%d %T\".

Example: \"2012-01-09T08:59:15.000Z\" becomes \"2012-01-09
16:59:15\", with the current timezone being +0800."
  (condition-case ()
      (format-time-string
       "%Y-%m-%d %T"
       (apply
        'encode-time
        (parse-time-string (replace-regexp-in-string "T\\|\\.000" " " jira-time-str))))
    (error jira-time-str)))

(defun org-jira--fix-encode-time-args (arg)
  "Fix ARG for 3 nil values at the head."
  (cl-loop
   for n from 0 to 2 by 1 do
   (when (not (nth n arg))
     (setcar (nthcdr n arg) 0)))
  arg)

(defun org-jira-time-format-to-jira (org-time-str)
  "Convert ORG-TIME-STR back to jira time format."
  (condition-case ()
      (format-time-string
       "%Y-%m-%dT%T.000Z"
       (apply 'encode-time
              (org-jira--fix-encode-time-args (parse-time-string org-time-str)))
       t)
    (error org-time-str)))

(defun org-jira-get-comment-val (key comment)
  "Return the value associated with KEY of COMMENT."
  (org-jira-get-issue-val key comment))

(defun org-jira-time-stamp-to-org-clock (time-stamp)
  "Convert TIME-STAMP into org-clock format."
  (format-time-string "%Y-%m-%d %a %H:%M" time-stamp))

(defun org-jira-date-to-org-clock (date)
  "Convert DATE into a time stamp and then into org-clock format.
Expects a date in format such as: 2017-02-26T00:08:00.000-0500."
  (org-jira-time-stamp-to-org-clock (date-to-time date)))

(defun org-jira-worklogs-to-org-clocks (worklogs)
  "Get a list of WORKLOGS and convert to org-clocks."
  (mapcar
   (lambda (worklog)
     (let ((wl-start (cdr (assoc 'started worklog)))
           (wl-time (cdr (assoc 'timeSpentSeconds worklog)))
           (wl-end))
       (setq wl-start (org-jira-date-to-org-clock wl-start))
       (setq wl-end (org-jira-time-stamp-to-org-clock (time-add (date-to-time wl-start) wl-time)))
       (list
        wl-start
        wl-end
        (cdr (assoc 'comment worklog))
        (cdr (assoc 'id worklog))
        )
       ))
   worklogs)
  )

(defun org-jira-format-clock (clock-entry)
  "Format a CLOCK-ENTRY given the (list start end).
This format is typically generated from org-jira-worklogs-to-org-clocks call."
  (format "CLOCK: [%s]--[%s]" (car clock-entry) (cadr clock-entry)))

(defun org-jira-insert-clock (clock-entry)
  "Insert a CLOCK-ENTRY given the (list start end).
This format is typically generated from org-jira-worklogs-to-org-clocks call."
  (insert (org-jira-format-clock clock-entry))
  (org-beginning-of-line)
  (org-ctrl-c-ctrl-c) ;; @TODO Maybe not call directly?  does it matter? - used to resync the clock estimate
  (org-end-of-line)
  (insert "\n")
  (insert (format "  :id: %s\n" (cadddr clock-entry)))
  (when (caddr clock-entry) (insert (replace-regexp-in-string "^\\*" "-" (format "  %s\n" (org-jira-decode (caddr clock-entry)))))) ;; No comment is nil, so don't print it
  )

(defun org-jira-logbook-reset (issue-id filename &optional clocks)
  "Find logbook for ISSUE-ID in FILENAME, delete it.
Re-create it with CLOCKS.  This is used for worklogs."
  (interactive)
  (let ((existing-logbook-p nil))
    ;; See if the LOGBOOK already exists or not.
    (ensure-on-issue-id-with-filename issue-id filename
      (let ((drawer-name (or (org-clock-drawer-name) "LOGBOOK")))
        (when (search-forward (format ":%s:" drawer-name) nil 1 1)
          (setq existing-logbook-p t))))
    (ensure-on-issue-id-with-filename issue-id filename
      (let ((drawer-name (or (org-clock-drawer-name) "LOGBOOK")))
        (if existing-logbook-p
            (progn ;; If we had a logbook, drop it and re-create in a bit.
              (search-forward (format ":%s:" drawer-name) nil 1 1)
              (org-beginning-of-line)
              (delete-region (point) (search-forward ":END:" nil 1 1))
              )
          (progn ;; Otherwise, create a new one at the end of properties list
            (search-forward ":END:" nil 1 1)
            (forward-line)))
        (org-insert-drawer nil (format "%s" drawer-name)) ;; Doc says non-nil, but this requires nil
        (mapc #'org-jira-insert-clock clocks)
        ;; Clean up leftover newlines (we left 2 behind)
        (dotimes (n 2)
          (search-forward-regexp "^$" nil 1 1)
          (delete-region (point) (min (point-max) (1+ (point)))))))))

(defun org-jira-get-worklog-val (key WORKLOG)
  "Return the value associated with KEY of WORKLOG."
  (org-jira-get-comment-val key WORKLOG))

(defun org-jira-get-issue-val (key issue)
  "Return the value associated with key KEY of issue ISSUE."
  (let ((tmp  (or (org-jira-find-value issue 'fields key 'key) ""))) ;; For project, we need a key, not the name...
    (unless (stringp tmp)
      (setq tmp (org-jira-find-value issue key)))
    (unless (stringp tmp)
      (setq tmp (org-jira-find-value issue 'fields key 'displayName)))
    (unless (stringp tmp)
      (setq tmp ""))
    (cond ((eq key 'components)
           (org-jira-get-issue-components issue))
          ((eq key 'labels)
           (org-jira-get-issue-labels issue))
          ((member key '(created updated startDate))
           (org-jira-transform-time-format tmp))
          ((eq key 'status)
           (if jiralib-use-restapi
               (org-jira-find-value issue 'fields 'status 'name)
             (org-jira-find-value (jiralib-get-statuses) tmp)))
          ((eq key 'resolution)
           (if jiralib-use-restapi
               tmp
             (if (string= tmp "")
                 ""
               (org-jira-find-value (jiralib-get-resolutions) tmp))))
          ((eq key 'type)
           (if jiralib-use-restapi
               (org-jira-find-value issue 'fields 'issuetype 'name)
             (org-jira-find-value (jiralib-get-issue-types) tmp)))
          ((eq key 'priority)
           (if jiralib-use-restapi
               (org-jira-find-value issue 'fields 'priority 'name)
             (org-jira-find-value (jiralib-get-priorities) tmp)))
          ((eq key 'description)
           (org-trim tmp))
          (t
           tmp))))

(defvar org-jira-jql-history nil)

;;;###autoload
(defun org-jira-get-issue-list (&optional callback)
  "Get list of issues, using jql (jira query language), invoke CALLBACK after.

Default is unresolved issues assigned to current login user; with
a prefix argument you are given the chance to enter your own
jql."
  (org-jira-log (format "I was called, was it with a callback? %s" (if callback "yes" "no")))
  (let ((jql org-jira-default-jql))
    (when current-prefix-arg
      (setq jql (read-string "Jql: "
                             (if org-jira-jql-history
                                 (car org-jira-jql-history)
                               "assignee = currentUser() and resolution = unresolved")
                             'org-jira-jql-history
                             "assignee = currentUser() and resolution = unresolved")))
    (list (jiralib-do-jql-search jql nil callback))))

(defun org-jira-get-issue-by-id (id)
  "Get an issue by its ID."
  (push id org-jira-issue-id-history)
  (let ((jql (format "id = %s" id)))
    (jiralib-do-jql-search jql)))

(defun org-jira-get-issue-by-fixversion (fixversion-id)
  "Get an issue by its FIXVERSION-ID."
  (push fixversion-id org-jira-fixversion-id-history)
  (let ((jql (format "fixVersion = \"%s\""  fixversion-id)))
    (jiralib-do-jql-search jql)))

;;;###autoload
(defun org-jira-get-summary ()
  "Get issue summary from point and place next to issue id from jira"
  (interactive)
  (let ((jira-id (thing-at-point 'symbol)))
    (unless jira-id (error "ORG_JIRA_ERROR: JIRA-ID missing in org-jira-get-summary!"))
    (forward-symbol 1)
    (insert (format " - %s"
                    (cdr (assoc 'summary (assoc 'fields (car (org-jira-get-issue-by-id jira-id)))))))))

;;;###autoload
(defun org-jira-get-summary-url ()
  "Get issue summary from point and place next to issue id from jira, and make issue id a link"
  (interactive)
  (let ((jira-id (thing-at-point 'symbol)))
    (insert (format "[[%s][%s]] - %s"
                    (concatenate 'string jiralib-url "browse/" jira-id) jira-id
                    (cdr (assoc 'summary (car (org-jira-get-issue-by-id jira-id))))))))

;;;###autoload
(defun org-jira-get-issues-headonly (issues)
  "Get list of ISSUES, head only.

The default behavior is to return issues assigned to you and unresolved.

With a prefix argument, allow you to customize the jql.  See
`org-jira-get-issue-list'."

  (interactive
   (org-jira-get-issue-list))

  (let* ((issues-file (expand-file-name "issues-headonly.org" (org-jira--ensure-working-dir)))
         (issues-headonly-buffer (or (find-buffer-visiting issues-file)
                                     (find-file issues-file))))
    (with-current-buffer issues-headonly-buffer
      (widen)
      (delete-region (point-min) (point-max))

      (mapc (lambda (issue)
              (let ((issue-id (org-jira-get-issue-key issue))
                    (issue-summary (org-jira-get-issue-summary issue)))
                (org-jira-insert (format "- [jira:%s] %s\n" issue-id issue-summary))))
            issues))
    (switch-to-buffer issues-headonly-buffer)))

;;;###autoload
(defun org-jira-get-issue (id)
  "Get a JIRA issue, allowing you to enter the issue-id first."
  (interactive (list (read-string "Issue ID: " "" 'org-jira-issue-id-history)))
  (org-jira-get-issues (org-jira-get-issue-by-id id))
  (let ((issue-pos (org-find-entry-with-id id)))
    (when issue-pos
      (goto-char issue-pos)
      (recenter 0))))
;;;###autoload
(defun org-jira-get-issues-by-fixversion (fixversion)
  "Get list of issues by FIXVERSION."
  (interactive (list (read-string "Fixversion ID: " ""
                                  'org-jira-fixversion-id-history)))
  (org-jira-get-issues (org-jira-get-issue-by-fixversion fixversion)))

;;;###autoload
(defun org-jira-get-issue-project (issue)
  (org-jira-find-value issue 'fields 'project 'key))

(defun org-jira-get-issue-key (issue)
  (org-jira-find-value issue 'key))

(defun org-jira-get-issue-summary (issue)
  (org-jira-find-value issue 'fields 'summary))

(defvar org-jira-get-issue-list-callback
  (cl-function
   (lambda (&key data &allow-other-keys)
     "Callback for async, DATA is the response from the request call.

Will send a list of org-jira-sdk-issue objects to the list printer."
     (org-jira-log "Received data for org-jira-get-issue-list-callback.")
     (--> data
          (org-jira-sdk-path it '(issues))
          (append it nil)               ; convert the conses into a proper list.
          org-jira-sdk-create-issues-from-data-list
          org-jira-get-issues))))

(defvar org-jira-get-sprint-list-callback
  (cl-function
   (lambda (&key data &allow-other-keys)
     "Callback for async, DATA is the response from the request call.

Will send a list of org-jira-sdk-issue objects to the list printer."
     (org-jira-log "Received data for org-jira-get-sprint-list-callback.")
     (--> data
          (org-jira-sdk-path it '(sprint))
          (append it nil)               ; convert the conses into a proper list.
          org-jira-sdk-create-issues-from-data-list
          org-jira-get-issues))))


;;;###autoload
(defun org-jira-get-issues (issues)
  "Get list of ISSUES into an org buffer.

Default is get unfinished issues assigned to you, but you can
customize jql with a prefix argument.
See`org-jira-get-issue-list'"
  ;; If the user doesn't provide a default, async call to build an issue list
  ;; from the JQL style query
  (interactive
   (org-jira-get-issue-list org-jira-get-issue-list-callback))
  (org-jira-log "Fetching issues...")
  (when (> (length issues) 0)
    (org-jira--render-issues-from-issue-list issues)))

(defvar org-jira-original-default-jql nil)

(defun org-jira-get-issues-from-custom-jql-callback (filename list)
  "Generate a function that we can iterate over FILENAME and LIST with when callback finishes."
  (cl-function
   (lambda (&key data &allow-other-keys)
     "Callback for async, DATA is the response from the request call.

Will send a list of org-jira-sdk-issue objects to the list printer."
     (org-jira-log "Received data for org-jira-get-issues-from-custom-jql-callback.")
     (--> data
          (org-jira-sdk-path it '(issues))
          (append it nil)     ; convert the conses into a proper list.
          (org-jira-sdk-create-issues-from-data-list-with-filename filename it)
          org-jira-get-issues)
     (setq org-jira-proj-key-override nil)
     (let ((next (rest list)))
       (when next
         (org-jira-get-issues-from-custom-jql next))))))

;;;###autoload
(defun org-jira-get-issues-from-custom-jql (&optional jql-list)
  "Get JQL-LIST list of issues from a custom JQL and PROJ-KEY.

The PROJ-KEY will act as the file name, while the JQL will be any
valid JQL to populate a file to store PROJ-KEY results in.

Please note that this is *not* concurrent or race condition
proof.  If you try to run multiple calls to this function, it
will mangle things badly, as they rely on globals DEFAULT-JQL and
ORG-JIRA-PROJ-KEY-OVERRIDE being set before and after running."
  (interactive)
  (let* ((jl (or jql-list org-jira-custom-jqls))
         (uno (car jl))
         (filename (cl-getf uno :filename))
         (limit (cl-getf uno :limit))
         (jql (replace-regexp-in-string "[\n]" " " (cl-getf uno :jql))))
    (setq org-jira-proj-key-override filename)
    (jiralib-do-jql-search jql limit (org-jira-get-issues-from-custom-jql-callback filename jl))))

(defun org-jira--get-project-buffer (Issue)
  (let* ((proj-key (org-jira--get-proj-key-from-issue Issue))
         (project-file (org-jira--get-project-file-name proj-key))
         (project-buffer (find-file-noselect project-file)))
    project-buffer))

(defun org-jira--is-top-headline? (proj-key)
  "For PROJ-KEY, check if it is a top headline or not."
  (let ((elem (org-element-at-point)))
    (and (eq 'headline (car elem))
         (equal (format "%s-Tickets" proj-key)
                (org-element-property :title elem))
         (= 1 (org-element-property :level elem)))))

(defun org-jira--maybe-render-top-heading (proj-key)
  "Ensure that there is a headline for PROJ-KEY at the top of the file."
  (goto-char (point-min))
  (let ((top-heading (format ".*%s-Tickets" proj-key))
        (th-found? nil))
    (while (and (not (eobp))
                (not th-found?))
      (beginning-of-line)
      (when (org-jira--is-top-headline? proj-key) (setq th-found? t))
      (re-search-forward top-heading nil 1 1))
    (beginning-of-line)
    (unless (looking-at top-heading)
      (insert (format "\n* %s-Tickets\n" proj-key)))))

(defun org-jira--render-issue (Issue)
  "Render single ISSUE."
;;  (org-jira-log "Rendering issue from issue list")
;;  (org-jira-log (org-jira-sdk-dump Issue))
  (with-slots (filename proj-key issue-id summary status priority headline id) Issue
    (let (p)
      (with-current-buffer (org-jira--get-project-buffer Issue)
        (org-jira-freeze-ui
          (org-jira-maybe-activate-mode)
          (org-jira--maybe-render-top-heading proj-key)
          (setq p (org-find-entry-with-id issue-id))
          (save-restriction
            (if (and p (>= p (point-min))
                     (<= p (point-max)))
                (progn
                  (goto-char p)
                  (forward-thing 'whitespace)
                  (org-jira-kill-line))
              (goto-char (point-max))
              (unless (looking-at "^")
                (insert "\n"))
              (insert "** "))
            (org-jira-insert
             (concat (org-jira-get-org-keyword-from-status status)
                     " "
                     (org-jira-get-org-priority-cookie-from-issue priority)
                     headline))
            (save-excursion
              (unless (search-forward "\n" (point-max) 1)
                (insert "\n")))
            (org-narrow-to-subtree)
            (save-excursion
              (org-back-to-heading t)
              (org-set-tags-to (replace-regexp-in-string "-" "_" issue-id)))
            (org-jira-entry-put (point) "assignee" (or (slot-value Issue 'assignee) "Unassigned"))
            (mapc (lambda (entry)
                    (let ((val (slot-value Issue entry)))
                      (when (and val (not (string= val "")))
                        (org-jira-entry-put (point) (symbol-name entry) val))))
                  '(filename reporter type type-id priority labels resolution status components created updated sprint))

            (org-jira-entry-put (point) "ID" issue-id)
            (org-jira-entry-put (point) "CUSTOM_ID" issue-id)

            ;; Insert the duedate as a deadline if it exists
            (when org-jira-deadline-duedate-sync-p
              (let ((duedate (oref Issue duedate)))
                (when (> (length duedate) 0)
                  (org-deadline nil duedate))))

            (mapc
             (lambda (heading-entry)
               (ensure-on-issue-id-with-filename issue-id filename
                                                 (let* ((entry-heading
                                                         (concat (symbol-name heading-entry)
                                                                 (format ": [[%s][%s]]"
                                                                         (concat jiralib-url "/browse/" issue-id) issue-id))))
                                                   (setq p (org-find-exact-headline-in-buffer entry-heading))
                                                   (if (and p (>= p (point-min))
                                                            (<= p (point-max)))
                                                       (progn
                                                         (goto-char p)
                                                         (org-narrow-to-subtree)
                                                         (goto-char (point-min))
                                                         (forward-line 1)
                                                         (delete-region (point) (point-max)))
                                                     (if (org-goto-first-child)
                                                         (org-insert-heading)
                                                       (goto-char (point-max))
                                                       (open-line 1)
                                                       (org-insert-subheading t))
                                                     (org-jira-insert entry-heading "\n"))

                                                   ;;  Insert 2 spaces of indentation so Jira markup won't cause org-markup
                                                   (org-jira-insert
                                                    (replace-regexp-in-string
                                                     "^" "  "
                                                     (format "%s" (slot-value Issue heading-entry)))))))
             '(description))

            (when org-jira-download-comments
              (org-jira-update-comments-for-issue Issue)

              ;; FIXME: Re-enable when attachments are not erroring.
              ;;(org-jira-update-attachments-for-current-issue)
              )

            ;; only sync worklog clocks when the user sets it to be so.
            (when org-jira-worklog-sync-p
              (org-jira-update-worklogs-for-issue issue-id filename))))))))

(defun org-jira--render-issues-from-issue-list (Issues)
  "Add the issues from ISSUES list into the org file(s).

ISSUES is a list of `org-jira-sdk-issue' records."
  ;; FIXME: Some type of loading error - the first async callback does not know about
  ;; the issues existing as a class, so we may need to instantiate here if we have none.
  (when (eq 0 (->> Issues (cl-remove-if-not #'org-jira-sdk-isa-issue?) length))
    (setq Issues (org-jira-sdk-create-issues-from-data-list Issues)))

  ;; First off, we never ever want to run on non-issues, so check our types early.
  (setq Issues (cl-remove-if-not #'org-jira-sdk-isa-issue? Issues))
  (org-jira-log (format "About to render %d issues." (length Issues)))

  ;; If we have any left, we map over them.
  (mapc 'org-jira--render-issue Issues)

  ;; Prior text: "Oh, are you the culprit?" - Not sure if this caused an issue at some point.
  ;; We want to ensure we fix broken org narrowing though, by doing org-show-all and then org-cycle.
  (switch-to-buffer (org-jira--get-project-buffer (-last-item Issues)))
  (org-show-all)
  (org-cycle))

;;;###autoload
(defun org-jira-update-comment ()
  "Update a comment for the current issue."
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key)) ; Really the key
         (filename (org-jira-filename))
         (comment-id (org-jira-get-from-org 'comment 'id))
         (comment (replace-regexp-in-string "^  " "" (org-jira-get-comment-body comment-id))))
    (lexical-let ((issue-id issue-id)
                  (filename filename))
      (let ((callback-edit
             (cl-function
              (lambda (&key _data &allow-other-keys)
                (ensure-on-issue-id-with-filename
                    issue-id filename
                    (org-jira-update-comments-for-current-issue)))))
            (callback-add
             (cl-function
              (lambda (&key _data &allow-other-keys)
                (ensure-on-issue-id-with-filename
                    issue-id filename
                    ;; @TODO :optim: Has to be a better way to do this
                    ;; than delete region (like update the unmarked
                    ;; one)
                    (org-jira-delete-current-comment)
                    (org-jira-update-comments-for-current-issue))))))
        (if comment-id
            (jiralib-edit-comment issue-id comment-id comment callback-edit)
          (jiralib-add-comment issue-id comment callback-add))))))

(defun org-jira-add-comment (issue-id filename comment)
  "For ISSUE-ID in FILENAME, add a new COMMENT string to the issue region."
  (interactive
   (let* ((issue-id (org-jira-get-from-org 'issue 'id))
          (filename (org-jira-filename))
          (comment (read-string (format  "Comment (%s): " issue-id))))
     (list issue-id filename comment)))
  (lexical-let ((issue-id issue-id)
                (filename filename))
    (ensure-on-issue-id-with-filename issue-id filename
      (goto-char (point-max))
      (jiralib-add-comment
       issue-id comment
       (cl-function
        (lambda (&key _data &allow-other-keys)
          (ensure-on-issue-id-with-filename issue-id filename
            (org-jira-update-comments-for-current-issue))))))))

(defun org-jira-org-clock-to-date (org-time)
  "Convert ORG-TIME formatted date into a plain date string."
  (format-time-string
   "%Y-%m-%dT%H:%M:%S.000%z"
   (date-to-time org-time)))

(defun org-jira-worklog-time-from-org-time (org-time)
  "Take in an ORG-TIME and convert it into the portions of a worklog time.
Expects input in format such as: [2017-04-05 Wed 01:00]--[2017-04-05 Wed 01:46] =>  0:46"
  (let ((start (replace-regexp-in-string "^\\[\\(.*?\\)\\].*" "\\1" org-time))
        (end (replace-regexp-in-string ".*--\\[\\(.*?\\)\\].*" "\\1" org-time)))
    `((started . ,(org-jira-org-clock-to-date start))
      (time-spent-seconds . ,(time-to-seconds
                              (time-subtract
                               (date-to-time end)
                               (date-to-time start)))))))

(defun org-jira-org-clock-to-jira-worklog (org-time clock-content)
  "Given ORG-TIME and CLOCK-CONTENT, format a jira worklog entry."
  (let ((lines (split-string clock-content "\n"))
        worklog-id)
    ;; See if we look like we have an id
    (when (string-match ":id:" (first lines))
      (setq worklog-id
            (replace-regexp-in-string "^.*:id: \\([0-9]*\\)$" "\\1" (first lines)))
      (when (> (string-to-number worklog-id) 0) ;; pop off the first id line if we found it valid
        (setq lines (cdr lines))))
    (setq lines (reverse (cdr (reverse lines)))) ;; drop last line
    (let ((comment (org-trim (mapconcat 'identity lines "\n")))
          (worklog-time (org-jira-worklog-time-from-org-time org-time)))
      `((worklog-id . ,worklog-id)
        (comment . ,comment)
        (started . ,(cdr (assoc 'started worklog-time)))
        (time-spent-seconds . ,(cdr (assoc 'time-spent-seconds worklog-time)))
        ))))

(defun org-jira-worklog-to-hashtable (issue-id)
  "Given ISSUE-ID, return a hashtable of worklog-id -> jira worklog."
  (let ((worklog-hashtable (make-hash-table :test 'equal)))
    (mapc
     (lambda (worklog)
       (let ((worklog-id (cdr (assoc 'id worklog))))
         (puthash worklog-id worklog worklog-hashtable)))
     (jiralib-worklog-import--filter-apply
      (org-jira-find-value
       (jiralib-get-worklogs
        issue-id)
       'worklogs)))
    worklog-hashtable))

;;;###autoload
(defun org-jira-update-worklogs-from-org-clocks ()
  "Update or add a worklog based on the org clocks."
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (filename (org-jira-filename))
         ;; Fetch all workflogs for this issue
         (jira-worklogs-ht (org-jira-worklog-to-hashtable issue-id)))
    (org-jira-log (format "About to sync worklog for issue: %s in file: %s"
                  issue-id filename))
    (ensure-on-issue-id-with-filename issue-id filename
      (search-forward (format ":%s:" (or (org-clock-drawer-name) "LOGBOOK"))  nil 1 1)
      (org-beginning-of-line)
      ;; (org-cycle 1)
      (while (search-forward "CLOCK: " nil 1 1)
        (let ((org-time (buffer-substring-no-properties (point) (point-at-eol))))
          (forward-line)
          ;; See where the stuff ends (what point)
          (let (next-clock-point)
            (save-excursion
              (search-forward-regexp "\\(CLOCK\\|:END\\):" nil 1 1)
              (setq next-clock-point (point)))
            (let ((clock-content
                   (buffer-substring-no-properties (point) next-clock-point)))
              ;; Update via jiralib call
              (let* ((worklog (org-jira-org-clock-to-jira-worklog org-time clock-content))
                     (comment-text (cdr (assoc 'comment worklog)))
                     (comment-text (if (string= (org-trim comment-text) "") nil comment-text)))
                (if (cdr (assoc 'worklog-id worklog))
                    ;; If there is a worklog in jira for this ID, check if the worklog has changed.
                    ;; If it has changed, update the worklog.
                    ;; If it has not changed, skip.
                    (let ((jira-worklog (gethash (cdr (assoc 'worklog-id worklog)) jira-worklogs-ht)))
                      (when (and jira-worklog
                                 ;; Check if the entries are differing lengths.
                                 (or (not (= (cdr (assoc 'timeSpentSeconds jira-worklog))
                                         (cdr (assoc 'time-spent-seconds worklog))))
                                 ;; Check if the entries start at different times.
                                     (not (string= (cdr (assoc 'started jira-worklog))
                                               (cdr (assoc 'started worklog))))))
                        (jiralib-update-worklog
                         issue-id
                         (cdr (assoc 'worklog-id worklog))
                         (cdr (assoc 'started worklog))
                         (cdr (assoc 'time-spent-seconds worklog))
                         comment-text
                         nil))) ; no callback - synchronous
                  ;; else
                  (jiralib-add-worklog
                   issue-id
                   (cdr (assoc 'started worklog))
                   (cdr (assoc 'time-spent-seconds worklog))
                   comment-text
                   nil) ; no callback - synchronous
                  )
                )))))
      (org-jira-log (format "Updating worklog from org-jira-update-worklogs-from-org-clocks call"))
      (org-jira-update-worklogs-for-issue issue-id filename)
      )))

(defun org-jira-update-worklog ()
  "Update a worklog for the current issue."
  (interactive)
  (error "Deprecated, use org-jira-update-worklogs-from-org-clocks instead!")
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (worklog-id (org-jira-get-from-org 'worklog 'id))
         (timeSpent (org-jira-get-from-org 'worklog 'timeSpent))
         (timeSpent (if timeSpent
                        timeSpent
                      (read-string "Input the time you spent (such as 3w 1d 2h): ")))
         (timeSpent (replace-regexp-in-string " \\(\\sw\\)\\sw*\\(,\\|$\\)" "\\1" timeSpent))
         (startDate (org-jira-get-from-org 'worklog 'startDate))
         (startDate (if startDate
                        startDate
                      (org-read-date nil nil nil "Input when did you start")))
         (startDate (org-jira-time-format-to-jira startDate))
         (comment (replace-regexp-in-string "^  " "" (org-jira-get-worklog-comment worklog-id)))
         (worklog `((comment . ,comment)
                    (timeSpent . ,timeSpent)
                    (timeSpentInSeconds . 10)
                    (startDate . ,startDate)))
         (worklog (if worklog-id
                      (cons `(id . ,(replace-regexp-in-string "^worklog-" "" worklog-id)) worklog)
                    worklog)))
    (if worklog-id
        (jiralib-update-worklog worklog)
      (jiralib-add-worklog-and-autoadjust-remaining-estimate issue-id startDate timeSpent comment))
    (org-jira-delete-current-worklog)
    (org-jira-update-worklogs-for-current-issue)))

(defun org-jira-delete-current-comment ()
  "Delete the current comment."
  (ensure-on-comment
   (delete-region (point-min) (point-max))))

(defun org-jira-delete-current-worklog ()
  "Delete the current worklog."
  (ensure-on-worklog
   (delete-region (point-min) (point-max))))

;;;###autoload
(defun org-jira-copy-current-issue-key ()
  "Copy the current issue's key into clipboard."
  (interactive)
  (let ((issue-id (org-jira-get-from-org 'issue 'key)))
    (with-temp-buffer
      (insert issue-id)
      (kill-region (point-min) (point-max)))))

(defun org-jira-get-comment-id (comment)
  (org-jira-find-value comment 'id))

(defun org-jira-get-comment-author (comment)
  (org-jira-find-value comment 'author 'displayName))

(defun org-jira-isa-ignored-comment? (comment)
  (member-ignore-case (oref comment author) org-jira-ignore-comment-user-list))

(defun org-jira-maybe-reverse-comments (comments)
  (if org-jira-reverse-comment-order (reverse comments) comments))

(defun org-jira-extract-comments-from-data (data)
  (->> (append data nil)
       org-jira-sdk-create-comments-from-data-list
       org-jira-maybe-reverse-comments
       (cl-remove-if #'org-jira-isa-ignored-comment?)))

(defun org-jira--render-comment (Issue Comment)
  (with-slots (issue-id) Issue
    (with-slots (comment-id author headline created updated body) Comment
      (org-jira-log (format "Rendering a comment: %s" body))
      (ensure-on-issue-Issue Issue
        (setq p (org-find-entry-with-id comment-id))
        (when (and p (>= p (point-min))
                   (<= p (point-max)))
          (goto-char p)
          (org-narrow-to-subtree)
          (delete-region (point-min) (point-max)))
        (goto-char (point-max))
        (unless (looking-at "^")
          (insert "\n"))
        (insert "*** ")
        (org-jira-insert headline "\n")
        (org-narrow-to-subtree)
        (org-jira-entry-put (point) "ID" comment-id)
        (org-jira-entry-put (point) "created" created)
        (unless (string= created updated)
          (org-jira-entry-put (point) "updated" updated))
        (goto-char (point-max))
        ;;  Insert 2 spaces of indentation so Jira markup won't cause org-markup
        (org-jira-insert (replace-regexp-in-string "^" "  " (or body "")))))))

(defun org-jira-update-comments-for-issue (Issue)
  "Update the comments for the specified ISSUE issue."
  (with-slots (issue-id) Issue
    (jiralib-get-comments
     issue-id
     (org-jira-with-callback
       (org-jira-log "In the callback for org-jira-update-comments-for-issue.")
       (-->
        (org-jira-find-value cb-data 'comments)
        (org-jira-extract-comments-from-data it)
        (mapc (lambda (Comment) (org-jira--render-comment Issue Comment)) it))))))

(defun org-jira-update-comments-for-current-issue ()
  "Update comments for the current issue."
  (org-jira-log "About to update comments for current issue.")
  (let ((Issue (make-instance 'org-jira-sdk-issue
                              :issue-id (org-jira-get-from-org 'issue 'key)
                              :filename (org-jira-filename))))
    (-> Issue org-jira-update-comments-for-issue)))

(defun org-jira-delete-subtree ()
  "Derived from org-cut-subtree.

Like that function, without mangling the user's clipboard for the
purpose of wiping an old subtree."
  (let (beg end folded (beg0 (point)))
    (org-back-to-heading t) ; take what is really there
    (setq beg (point))
    (skip-chars-forward " \t\r\n")
    (save-match-data
      (save-excursion (outline-end-of-heading)
                      (setq folded (org-invisible-p))
                      (org-end-of-subtree t t)))
    ;; Include the end of an inlinetask
    (when (and (featurep 'org-inlinetask)
               (looking-at-p (concat (org-inlinetask-outline-regexp)
                                     "END[ \t]*$")))
      (end-of-line))
    (setq end (point))
    (goto-char beg0)
    (when (> end beg)
      (setq org-subtree-clip-folded folded)
      (org-save-markers-in-region beg end)
      (delete-region beg end))))

(defun org-jira-update-attachments-for-current-issue ()
  "Update the attachments for the current issue."
  (when jiralib-use-restapi
    (lexical-let ((issue-id (org-jira-get-from-org 'issue 'key)))
      ;; Run the call
      (jiralib-get-attachments
       issue-id
       (save-excursion
         (cl-function
          (lambda (&key data &allow-other-keys)
            ;; First, make sure we're in the proper buffer (logic copied from org-jira-get-issues.
            (let* ((proj-key (replace-regexp-in-string "-.*" "" issue-id))
                   (project-file (org-jira--get-project-file-name proj-key))
                   (project-buffer (or (find-buffer-visiting project-file)
                                       (find-file project-file))))
              (with-current-buffer project-buffer
                ;; delete old attachment node
                (ensure-on-issue
                  (if (org-goto-first-child)
                      (while (org-goto-sibling)
                        (forward-thing 'whitespace)
                        (when (looking-at "Attachments:")
                          (org-jira-delete-subtree)))))
                (let ((attachments (org-jira-find-value data 'fields 'attachment)))
                  (when (not (zerop (length attachments)))
                    (ensure-on-issue
                      (if (org-goto-first-child)
                          (progn
                            (while (org-goto-sibling))
                            (org-insert-heading-after-current))
                        (org-insert-subheading nil))

                      (insert "Attachments:")
                      (mapc
                       (lambda (attachment)
                         (let ((attachment-id (org-jira-get-comment-id attachment))
                               (author (org-jira-get-comment-author attachment))
                               (created (org-jira-transform-time-format
                                         (org-jira-find-value attachment 'created)))
                               (size (org-jira-find-value attachment 'size))
                               (mimeType (org-jira-find-value attachment 'mimeType))
                               (content (org-jira-find-value attachment 'content))
                               (filename (org-jira-find-value attachment 'filename)))
                           (if (looking-back "Attachments:")
                               (org-insert-subheading nil)
                             (org-insert-heading-respect-content))
                           (insert "[[" content "][" filename "]]")
                           (org-narrow-to-subtree)
                           (org-jira-entry-put (point) "ID" attachment-id)
                           (org-jira-entry-put (point) "Author" author)
                           (org-jira-entry-put (point) "Name" filename)
                           (org-jira-entry-put (point) "Created" created)
                           (org-jira-entry-put (point) "Size" (ls-lisp-format-file-size size t))
                           (org-jira-entry-put (point) "Content" content)
                           (widen)))
                       attachments)))))))))))))

(defun org-jira-sort-org-clocks (clocks)
  "Given a CLOCKS list, sort it by start date descending."
  ;; Expects data such as this:

  ;; ((\"2017-02-26 Sun 00:08\" \"2017-02-26 Sun 01:08\" \"Hi\" \"10101\")
  ;;  (\"2017-03-16 Thu 22:25\" \"2017-03-16 Thu 22:57\" \"Test\" \"10200\"))
  (sort clocks
        (lambda (a b)
          (> (time-to-seconds (date-to-time (car a)))
             (time-to-seconds (date-to-time (car b)))))))

(defun org-jira-update-worklogs-for-current-issue ()
  "Update the worklogs for the current issue."
  (let ((issue-id (org-jira-get-from-org 'issue 'key))
        (filename (org-jira-filename)))
    (org-jira-update-worklogs-for-issue issue-id filename)))

(defun org-jira-update-worklogs-for-issue (issue-id filename)
  "Update the worklogs for the current ISSUE-ID located in FILENAME."
  (org-jira-log (format "org-jira-update-worklogs-for-issue id: %s filename: %s"
                issue-id filename))
  ;; Run the call
  (jiralib-get-worklogs
   issue-id
   (org-jira-with-callback
     (ensure-on-issue-id-with-filename issue-id filename
       (let ((worklogs (org-jira-find-value cb-data 'worklogs)))
         (org-jira-log (format "org-jira-update-worklogs-for-issue cb id: %s fn: %s"
                       issue-id filename))
         (org-jira-logbook-reset issue-id filename
          (org-jira-sort-org-clocks (org-jira-worklogs-to-org-clocks
                                     (jiralib-worklog-import--filter-apply worklogs)))))))))

;;;###autoload
(defun org-jira-unassign-issue ()
  "Update an issue to be unassigned."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id))
        (filename (org-jira-parse-issue-filename)))
    (org-jira-update-issue-details issue-id filename :assignee nil)))

;;;###autoload
(defun org-jira-set-issue-reporter ()
  "Update an issue's reporter interactively."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id))
        (filename (org-jira-parse-issue-filename)))
    (if issue-id
        (let* ((project (replace-regexp-in-string "-[0-9]+" "" issue-id))
               (jira-users (org-jira-get-reporter-candidates project)) ;; TODO, probably a better option than org-jira-get-assignable-users here
               (user (completing-read
                      "Reporter: "
                      (append (mapcar 'car jira-users)
                              (mapcar 'cdr jira-users))))
               (reporter (or
                          (cdr (assoc user jira-users))
                          (cdr (rassoc user jira-users)))))
          (when (null reporter)
            (error "No reporter found, this should probably never happen."))
          (org-jira-update-issue-details issue-id filename :reporter (jiralib-get-user-account-id project reporter)))
      (error "Not on an issue"))))

;;;###autoload
(defun org-jira-assign-issue ()
  "Update an issue with interactive re-assignment."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id))
        (filename (org-jira-parse-issue-filename)))
    (if issue-id
        (let* ((project (replace-regexp-in-string "-[0-9]+" "" issue-id))
               (jira-users (org-jira-get-assignable-users project))
               (user (completing-read
                      "Assignee: "
                      (append (mapcar 'car jira-users)
                              (mapcar 'cdr jira-users))))
               (assignee (or
                          (cdr (assoc user jira-users))
                          (cdr (rassoc user jira-users)))))
          (when (null assignee)
            (error "No assignee found, use org-jira-unassign-issue to make the issue unassigned"))
          (org-jira-update-issue-details issue-id filename :assignee (jiralib-get-user-account-id project assignee)))
      (error "Not on an issue"))))

;;;###autoload
(defun org-jira-update-issue ()
  "Update an issue."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id))
        (filename (org-jira-parse-issue-filename)))
    (if issue-id
        (org-jira-update-issue-details issue-id filename)
      (error "Not on an issue"))))

;;;###autoload
(defun org-jira-todo-to-jira ()
  "Convert an ordinary todo item to a jira ticket."
  (interactive)
  (ensure-on-todo
   (when (org-jira-parse-issue-id)
     (error "Already on jira ticket"))
   (save-excursion (org-jira-create-issue
                    (org-jira-read-project)
                    (org-jira-read-issue-type)
                    (org-get-heading t t)
                    (org-get-entry)))
   (delete-region (point-min) (point-max))))

;;;###autoload
(defun org-jira-get-subtasks ()
  "Get subtasks for the current issue."
  (interactive)
  (ensure-on-issue
    (org-jira-get-issues-headonly (jiralib-do-jql-search (format "parent = %s" (org-jira-parse-issue-id))))))

;;;###autoload
(defun org-jira-update-issue-labels ()
  "Update jira issue labels."
  (interactive)
  (let* ((labels (org-jira-parse-issue-labels))
         (updated-labels (org-jira-read-labels (format "%s, " labels)))
         (updated-labels-string (mapconcat 'identity updated-labels ", ")))
    (org-set-property "labels" updated-labels-string)
    (org-jira-update-issue)))

(defvar org-jira-project-read-history nil)
(defvar org-jira-boards-read-history nil)
(defvar org-jira-sprints-read-history nil)
(defvar org-jira-components-read-history nil)
(defvar org-jira-priority-read-history nil)
(defvar org-jira-type-read-history nil)

(defun org-jira-read-project ()
  "Read project name."
  (completing-read
   "Project: "
   (jiralib-make-list (jiralib-get-projects) 'key)
   nil
   t
   nil
   'org-jira-project-read-history
   (car org-jira-project-read-history)))

(defun org-jira-read-board ()
  "Read board name. Returns cons pair (name . integer-id)"
  (let* ((boards-alist
          (jiralib-make-assoc-list (jiralib-get-boards) 'name 'id))
         (board-name
          (completing-read "Boards: "  boards-alist
                           nil  t  nil
                           'org-jira-boards-read-history
                           (car org-jira-boards-read-history))))
    (assoc board-name boards-alist)))

(defun org-jira-read-sprint (board)
  "Read sprint name. Returns cons pair (name . integer-id)"
  (let* ((sprints-alist
	  (jiralib-make-assoc-list (append (alist-get 'values (jiralib-get-board-sprints board)) nil) 'name 'id))
	  (sprint-name
	   (completing-read "Sprints: " sprints-alist
			    nil t nil
			    'org-jira-sprints-read-history
			    (car org-jira-sprints-read-history))))
       (assoc sprint-name sprints-alist)))

(defun org-jira-read-component (project)
  "Read the components options for PROJECT such as EX."
  (completing-read
   "Components (choose Done to stop): "
   (append '("Done") (mapcar 'cdr (jiralib-get-components project)))
   nil
   t
   nil
   'org-jira-components-read-history
   "Done"))

;; TODO: Finish this feature - integrate into org-jira-create-issue
(defun org-jira-read-components (project)
  "Types: string PROJECT : string (csv of components).

Get all the components for the PROJECT such as EX,
that should be bound to an issue."
  (let (components component)
    (while (not (equal "Done" component))
      (setq component (org-jira-read-component project))
      (unless (equal "Done" component)
        (push component components)))
    components))

(defun org-jira-read-priority ()
  "Read priority name."
  (completing-read
   "Priority: "
   (mapcar 'cdr (jiralib-get-priorities))
   nil
   t
   nil
   'org-jira-priority-read-history
   (car org-jira-priority-read-history)))

(defun org-jira-read-issue-type (&optional project)
  "Read issue type name.  PROJECT is the optional project key."
  (let* ((issue-types
          (mapcar 'cdr (if project
                           (jiralib-get-issue-types-by-project project)
                         (jiralib-get-issue-types))))
         (initial-input (when (member (car org-jira-type-read-history) issue-types)
                          org-jira-type-read-history)))

    ;; TODO: The completing-read calls as such are all over the place, and always tend
    ;; to follow this exact same call structure - we should abstract to a single fn
    ;; that will allow calling with fewer or keyword args
    (completing-read
     "Type: "                           ; PROMPT
     issue-types                        ; COLLECTION
     nil                                ; PREDICATE
     t                                  ; REQUIRE-MATCH
     nil                                ; INITIAL-INPUT
     'initial-input                     ; HIST
     (car initial-input))))             ; DEF

(defun org-jira-read-subtask-type ()
  "Read issue type name."
  (completing-read
   "Type: "
   (mapcar 'cdr (jiralib-get-subtask-types))
   nil
   t
   nil
   'org-jira-type-read-history
   (car org-jira-type-read-history)))

(defun org-jira-get-issue-struct (project type summary description &optional parent-id)
  "Create an issue struct for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((project-components (jiralib-get-components project))
         (jira-users (org-jira-get-assignable-users project))
         (user (completing-read "Assignee: " (mapcar 'car jira-users)))
         (priority (car (rassoc (org-jira-read-priority) (jiralib-get-priorities))))
         (labels (org-jira-read-labels))
         (ticket-struct
          `((fields
             (project (key . ,project))
             (parent (key . ,parent-id))
             (issuetype (id . ,(car (rassoc type (if (and (boundp 'parent-id) parent-id)
                                                     (jiralib-get-subtask-types)
                                                   (jiralib-get-issue-types-by-project project))))))
             (summary . ,(format "%s%s" summary
                                 (if (and (boundp 'parent-id) parent-id)
                                     (format " (subtask of [jira:%s])" parent-id)
                                   "")))
             (description . ,description)
             (priority (id . ,priority))
             (labels . ,labels)
             ;; accountId should be nil if Unassigned, not the key slot.
             (assignee (accountId . ,(or (cdr (assoc user jira-users)) nil)))))))
    ticket-struct))

;;;###autoload
(defun org-jira-create-issue (project type summary description)
  "Create an issue in PROJECT, of type TYPE, with given SUMMARY and DESCRIPTION."
  (interactive
   (let* ((project (org-jira-read-project))
          (type (org-jira-read-issue-type project))
          (summary (read-string "Summary: "))
          (description (read-string "Description: ")))
     (list project type summary description)))
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((parent-id nil)
         (ticket-struct (org-jira-get-issue-struct project type summary description)))
    (org-jira-get-issues (list (jiralib-create-issue ticket-struct)))))

;;;###autoload
(defun org-jira-create-subtask (project type summary description)
  "Create a subtask issue for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (interactive (ensure-on-issue (list (org-jira-read-project)
                                      (org-jira-read-subtask-type)
                                      (read-string "Summary: ")
                                      (read-string "Description: "))))
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((parent-id (org-jira-parse-issue-id))
         (ticket-struct (org-jira-get-issue-struct project type summary description parent-id)))
    (org-jira-get-issues (list (jiralib-create-subtask ticket-struct)))))

(defun org-jira-get-issue-val-from-org (key)
  "Return the requested value by KEY from the current issue."
  ;; There is some odd issue when not using any let-scoping, where myself
  ;; and an array of users are hitting a snag circa 2023-03-01 time frame
  ;; in which the setq portion of a when clause is being hit even when it
  ;; evaluates to false - the bug only manifests on a first launch of Emacs - it
  ;; doesn't occur when re-evaluating this function.  However, wrapping it "fixes"
  ;; the issue.
  ;;
  ;; The first link has the most troubleshooting/diagnosis around the particulars of
  ;; this bug.
  ;;
  ;; See: https://github.com/ahungry/org-jira/issues/319
  ;; See: https://github.com/ahungry/org-jira/issues/296
  ;; See: https://github.com/ahungry/org-jira/issues/316
  (lexical-let ((my-key key))
    (ensure-on-issue
      (cond ((eq my-key 'description)
             (org-goto-first-child)
             (forward-thing 'whitespace)
             (if (looking-at "description: ")
                 (org-trim (org-get-entry))
               (error "Can not find description field for this issue")))

            ((eq my-key 'summary)
             (ensure-on-issue
               (org-get-heading t t)))

            ;; org returns a time tuple, we need to convert it
            ((eq my-key 'deadline)
             (let ((encoded-time (org-get-deadline-time (point))))
               (when encoded-time
                 (cl-reduce (lambda (carry segment)
                              (format "%s-%s" carry segment))
                            (reverse (cl-subseq (decode-time encoded-time) 3 6))))))

            ;; default case, just grab the value in the properties block
            (t
             (when (symbolp my-key)
               (setq my-key (symbol-name my-key)))

             (setq my-key (or (assoc-default my-key org-jira-property-overrides)
                              my-key))

             ;; This is the "impossible" to hit setq that somehow gets hit without the let
             ;; wrapper around the function input args.
             (when (string= my-key "key")
               (setq my-key "ID"))

             ;; The variable `org-special-properties' will mess this up
             ;; if our search, such as 'priority' is within there, so
             ;; don't bother with it for this (since we only ever care
             ;; about the local properties, not any hierarchal or special
             ;; ones).
             (let ((org-special-properties nil))
               (or (org-entry-get (point) my-key t)
                   "")))))))

(defun org-jira-read-action (actions)
  "Read issue workflow progress ACTIONS."
  (let ((action (completing-read
                 "Action: "
                 (mapcar 'cdr actions)
                 nil
                 t
                 nil)))
    (or
     (car (rassoc action actions))
     (user-error "You specified an empty action, the valid actions are: %s" (mapcar 'cdr actions)))))

(defun org-jira-read-labels (&optional current-labels)
  "Pick multiple labels which will be added or updating existing
CURRENT-LABELS and save with the jira issue."
  (unless current-labels (setq current-labels nil))
  (if jiralib-labels-cache
      (completing-read-multiple "Labels: " jiralib-labels-cache nil nil current-labels)
    (jiralib-get-labels)
    (completing-read-multiple "Labels: " jiralib-labels-cache nil nil current-labels)))

(defvar org-jira-fields-history nil)
(defun org-jira-read-field (fields)
  "Read (custom) FIELDS for workflow progress."
  (let ((field-desc (completing-read
                     "More fields to set: "
                     (cons "Thanks, no more fields are *required*." (mapcar 'org-jira-decode (mapcar 'cdr fields)))
                     nil
                     t
                     nil
                     'org-jira-fields-history))
        field-name)
    (setq field-name (car (rassoc field-desc fields)))
    (if field-name
        (intern field-name)
      field-name)))


(defvar org-jira-rest-fields nil
  "Extra fields are held here for usage between two endpoints.
Used in org-jira-read-resolution and org-jira-progress-issue calls.")

(defvar org-jira-resolution-history nil)
(defun org-jira-read-resolution ()
  "Read issue workflow progress resolution."
  (if (not jiralib-use-restapi)
      (let ((resolution (completing-read
                         "Resolution: "
                         (mapcar 'cdr (jiralib-get-resolutions))
                         nil
                         t
                         nil
                         'org-jira-resolution-history
                         (car org-jira-resolution-history))))
        (car (rassoc resolution (jiralib-get-resolutions))))
    (let* ((resolutions (org-jira-find-value org-jira-rest-fields 'resolution 'allowedValues))
           (resolution-name (completing-read
                             "Resolution: "
                             (mapcar (lambda (resolution)
                                       (org-jira-find-value resolution 'name))
                                     resolutions))))
      (cons 'name resolution-name))))

(defun org-jira-refresh-issues-in-buffer-loose ()
  "Iterates over all level 1-2 headings in current buffer, refreshing on issue :ID:.
It differs with `org-jira-refresh-issues-in-buffer' in that it accepts the current buffer
and its corresponding filename, regardless of whether it has been previously registered
as an org-jira project file or not."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((org-ids (org-map-entries 'org-id-get "LEVEL=1|LEVEL=2"))
             (org-ids (delq nil org-ids))
             (file-name (file-name-sans-extension buffer-file-name)))
        (mapcar (lambda (org-id) (org-jira--refresh-issue org-id file-name))
                org-ids)))))

(defun org-jira-refresh-issues-in-buffer ()
  "Iterate across all level 1-2 headings in current buffer, refreshing on issue :ID:.
Where issue-id will be something such as \"EX-22\"."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((org-ids (org-map-entries 'org-id-get "LEVEL=1|LEVEL=2"))
             (org-ids (delq nil org-ids)))
        ;; It's possible we could be on a non-org-jira headline, but
        ;; that should be an exceptional case and not necessitating a
        ;; fix atm.
        (mapcar 'org-jira--refresh-issue-by-id org-ids)))))

;;;###autoload
(defun org-jira-refresh-issue ()
  "Refresh current issue from jira to org."
  (interactive)
  (ensure-on-issue
    (org-jira--refresh-issue (org-jira-id) (org-jira-filename))))

(defun org-jira--refresh-issue (issue-id &optional filename)
  "Refresh issue from jira to org using ISSUE-ID."
  (unless filename (setq filename (replace-regexp-in-string "-[0-9]+" "" issue-id)))
  (jiralib-get-issue
   issue-id
   (org-jira-with-callback
     (org-jira-log (format "Received refresh issue data for id: %s in file: %s" issue-id filename))
     (--> cb-data
          list
          (org-jira-sdk-create-issues-from-data-list-with-filename filename it)
          org-jira--render-issues-from-issue-list))))

(defun org-jira--refresh-issue-by-id (issue-id)
  "Refresh issue from jira to org using ISSUE-ID."
  (ensure-on-issue-id issue-id
    (org-jira--refresh-issue issue-id)))

(defvar org-jira-fields-values-history nil)
;;;###autoload
(defun org-jira-progress-issue ()
  "Progress issue workflow."
  (interactive)
  (ensure-on-issue
    (let* ((issue-id (org-jira-id))
           (actions (jiralib-get-available-actions
                     issue-id
                     (org-jira-get-issue-val-from-org 'status)))
           (action (org-jira-read-action actions))
           (fields (jiralib-get-fields-for-action issue-id action))
           (org-jira-rest-fields fields)
           (field-key)
           (custom-fields-collector nil)
           (custom-fields
            (progn
              ;; delete those elements in fields, which have
              ;; already been set in custom-fields-collector
              (while fields
                (setq fields
                      (cl-remove-if
                       (lambda (strstr)
                         (cl-member-if (lambda (symstr)
                                         (string= (car strstr)  (symbol-name (car symstr))))
                                       custom-fields-collector))
                       fields))
                (setq field-key (org-jira-read-field fields))
                (if (not field-key)
                    (setq fields nil)
                  (setq custom-fields-collector
                        (cons
                         (funcall (if jiralib-use-restapi
                                      #'list
                                    #'cons)
                                  field-key
                                  (if (eq field-key 'resolution)
                                      (org-jira-read-resolution)
                                    (let ((field-value (completing-read
                                                        (format "Please enter %s's value: "
                                                                (cdr (assoc (symbol-name field-key) fields)))
                                                        org-jira-fields-values-history
                                                        nil
                                                        nil
                                                        nil
                                                        'org-jira-fields-values-history)))
                                      (if jiralib-use-restapi
                                          (cons 'name field-value)
                                        field-value))))
                         custom-fields-collector))))
              custom-fields-collector)))
      (jiralib-progress-workflow-action
       issue-id
       action
       custom-fields
       (cl-function
        (lambda (&key data &allow-other-keys)
          (org-jira-refresh-issue)))))))

(defun org-jira-progress-next-action (actions current-status)
  "Grab the user defined 'next' action from ACTIONS, given CURRENT-STATUS."
  (let* ((next-action-name (cdr (assoc current-status org-jira-progress-issue-flow)))
         (next-action-id (caar (cl-remove-if-not
                                (lambda (action)
                                  (equal action next-action-name)) actions :key #'cdr))))
    next-action-id))

;;;###autoload
(defun org-jira-progress-issue-next ()
  "Progress issue workflow."
  (interactive)
  (ensure-on-issue
    (let* ((issue-id (org-jira-id))
           (filename (org-jira-filename))
           (actions (jiralib-get-available-actions
                     issue-id
                     (org-jira-get-issue-val-from-org 'status)))
           (action (org-jira-progress-next-action actions (org-jira-get-issue-val-from-org 'status)))
           (fields (jiralib-get-fields-for-action issue-id action))
           (org-jira-rest-fields fields)
           (field-key)
           (custom-fields-collector nil)
           (custom-fields
            (progn
              ;; delete those elements in fields, which have
              ;; already been set in custom-fields-collector
              (while fields
                (setq fields
                      (cl-remove-if
                       (lambda (strstr)
                         (cl-member-if (lambda (symstr)
                                         (string= (car strstr)  (symbol-name (car symstr))))
                                       custom-fields-collector))
                       fields))
                (setq field-key (org-jira-read-field fields))
                (if (not field-key)
                    (setq fields nil)
                  (setq custom-fields-collector
                        (cons
                         (funcall (if jiralib-use-restapi
                                      #'list
                                    #'cons)
                                  field-key
                                  (if (eq field-key 'resolution)
                                      (org-jira-read-resolution)
                                    (let ((field-value (completing-read
                                                        (format "Please enter %s's value: "
                                                                (cdr (assoc (symbol-name field-key) fields)))
                                                        org-jira-fields-values-history
                                                        nil
                                                        nil
                                                        nil
                                                        'org-jira-fields-values-history)))
                                      (if jiralib-use-restapi
                                          (cons 'name field-value)
                                        field-value))))
                         custom-fields-collector))))
              custom-fields-collector)))
      (if action
          (jiralib-progress-workflow-action
           issue-id
           action
           custom-fields
           (org-jira-with-callback
             (ensure-on-issue-id-with-filename issue-id filename
               (org-jira-refresh-issue))))
        (error "No action defined for that step!")))))

(defun org-jira-get-id-name-alist (name ids-to-names)
  "Find the id corresponding to NAME in IDS-TO-NAMES and return an alist with id and name as keys."
  (let ((id (car (rassoc name ids-to-names))))
    `((id . ,id)
      (name . ,name))))

(defun org-jira-build-components-list (project-components org-issue-components)
  "Given PROJECT-COMPONENTS, attempt to build a list.

If the PROJECT-COMPONENTS are nil, this should return:

  (list components []), which will translate into the JSON:

  {\"components\": []}

otherwise it should return:

  (list components (list (cons id comp-id) (cons name item-name))),

  which will translate into the JSON:

{\"components\": [{\"id\": \"comp-id\", \"name\": \"item\"}]}"
  (if (not project-components) (vector) ;; Return a blank array for JSON
    (apply 'list
           (cl-mapcan
            (lambda (item)
              (let ((comp-id (car (rassoc item project-components))))
                (if comp-id
                    `(((id . ,comp-id)
                       (name . ,item)))
                  nil)))
            (split-string org-issue-components ",\\s *")))))

(defun org-jira-strip-priority-tags (s)
  "Given string S, remove any priority tags in the brackets."
  (->> s (replace-regexp-in-string "\\[#.*?\\]" "") org-trim))

(defun org-jira-update-issue-details (issue-id filename &rest rest)
  "Update the details of issue ISSUE-ID in FILENAME.  REST will contain optional input."
  (ensure-on-issue-id-with-filename issue-id filename
    ;; Set up a bunch of values from the org content
    (let* ((org-issue-components (org-jira-get-issue-val-from-org 'components))
           (org-issue-labels (org-jira-get-issue-val-from-org 'labels))
           (org-issue-description (org-trim (org-jira-get-issue-val-from-org 'description)))
           (org-issue-priority (org-jira-get-issue-val-from-org 'priority))
           (org-issue-type (org-jira-get-issue-val-from-org 'type))
           (org-issue-type-id (org-jira-get-issue-val-from-org 'type-id))
           (org-issue-assignee (cl-getf rest :assignee (org-jira-get-issue-val-from-org 'assignee)))
           (org-issue-reporter (cl-getf rest :reporter (org-jira-get-issue-val-from-org 'reporter)))
           (project (replace-regexp-in-string "-[0-9]+" "" issue-id))
           (project-components (jiralib-get-components project)))

      ;; Lets fire off a worklog update async with the main issue
      ;; update, why not?  This is better to fire first, because it
      ;; doesn't auto-refresh any areas, while the end of the main
      ;; update does a callback that reloads the worklog entries (so,
      ;; we hope that won't occur until after this successfully syncs
      ;; up).  Only do this sync if the user defcustom defines it as such.
      (when org-jira-worklog-sync-p
        (org-jira-update-worklogs-from-org-clocks))

      ;; Send the update to jira
      (let ((update-fields
             (list (cons
                    'components
                    (or (org-jira-build-components-list
                         project-components
                         org-issue-components) []))
                   (cons 'labels (split-string org-issue-labels ",\\s *"))
                   (cons 'priority (org-jira-get-id-name-alist org-issue-priority
                                                       (jiralib-get-priorities)))
                   (cons 'description org-issue-description)
                   (cons 'assignee (list (cons 'id (jiralib-get-user-account-id project org-issue-assignee))))
                   (cons 'summary (org-jira-strip-priority-tags (org-jira-get-issue-val-from-org 'summary)))
                   (cons 'issuetype `((id . ,org-issue-type-id)
      (name . ,org-issue-type))))))

        (if org-jira-update-issue-details-include-reporter
            (setq update-fields
                  (append update-fields
                          (list (cons 'reporter (list (cons 'id (jiralib-get-user-account-id project org-issue-reporter))))))))

        ;; If we enable duedate sync and we have a deadline present
        (when (and org-jira-deadline-duedate-sync-p
                   (org-jira-get-issue-val-from-org 'deadline))
          (setq update-fields
                (append update-fields
                        (list (cons 'duedate (org-jira-get-issue-val-from-org 'deadline))))))

        ;; TODO: We need some way to handle things like assignee setting
        ;; and refreshing the proper issue in the proper buffer/filename.
        (jiralib-update-issue
         issue-id
         update-fields
         ;; This callback occurs on success
         (org-jira-with-callback
           (message (format "Issue '%s' updated!" issue-id))
           (jiralib-get-issue
            issue-id
            (org-jira-with-callback
              (org-jira-log "Update get issue for refresh callback hit.")
              (-> cb-data list org-jira-get-issues))))
         ))
      )))



(defun org-jira-parse-issue-id ()
  "Get issue id from org text."
  (save-excursion
    (let ((continue t)
          issue-id)
      (while continue
        (when (string-match (jiralib-get-issue-regexp)
                            (or (setq issue-id (org-entry-get (point) "ID"))
                                ""))
          (setq continue nil))
        (unless (and continue (org-up-heading-safe))
          (setq continue nil)))
      issue-id)))

(defun org-jira-parse-issue-filename ()
  "Get filename from org text."
  (save-excursion
    (let ((continue t)
          filename)
      (while continue
        (when (setq filename (org-entry-get (point) "filename"))
          (setq continue nil))
        (unless (and continue (org-up-heading-safe))
          (setq continue nil)))
      filename)))

(defun org-jira-parse-issue-labels ()
  "Get issue labels from org text."
  (save-excursion
    (let ((continue t)
          labels)
      (while continue
        (when (setq labels (org-entry-get (point) "labels"))
          (setq continue nil))
        (unless (and continue (org-up-heading-safe))
          (setq continue nil)))
      labels)))

(defun org-jira-get-from-org (type entry)
  "Get an org property from the current item.

TYPE is the type to of the current item, and can be 'issue, or 'comment.

ENTRY will vary, and is the name of the property to return.  If
it is a symbol, it will be converted to string."
  (when (symbolp entry)
    (setq entry (symbol-name entry)))
  (cond
   ((eq type 'issue)
    (org-jira-get-issue-val-from-org entry))
   ((eq type 'comment)
    (org-jira-get-comment-val-from-org entry))
   ((eq type 'worklog)
    (org-jira-get-worklog-val-from-org entry))
   (t (error "Unknown type %s" type))))

(defun org-jira-get-comment-val-from-org (entry)
  "Get the JIRA issue field value ENTRY of the current comment item."
  (ensure-on-comment
   (when (symbolp entry)
     (setq entry (symbol-name entry)))
   (when (string= entry "id")
     (setq entry "ID"))
   (org-entry-get (point) entry)))

(defun org-jira-get-worklog-val-from-org (entry)
  "Get the JIRA issue field value ENTRY of the current worklog item."
  (ensure-on-worklog
   (when (symbolp entry)
     (setq entry (symbol-name entry)))
   (when (string= entry "id")
     (setq entry "ID"))
   (org-entry-get (point) entry)))

(defun org-jira-get-comment-body (&optional comment-id)
  "Get the comment body of the comment with id COMMENT-ID."
  (ensure-on-comment
   (goto-char (point-min))
   ;; so that search for :END: won't fail
   (org-jira-entry-put (point) "ID" comment-id)
   (search-forward ":END:" nil 1 1)
   (forward-line)
   (org-trim (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-get-worklog-comment (&optional worklog-id)
  "Get the worklog comment of the worklog with id WORKLOG-ID."
  (ensure-on-worklog
   (goto-char (point-min))
   ;; so that search for :END: won't fail
   (org-jira-entry-put (point) "ID" worklog-id)
   (search-forward ":END:" nil 1 1)
   (forward-line)
   (org-trim (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-id ()
  "Get the ID entry for the current heading."
  (org-entry-get (point) "ID"))

(defun org-jira-filename ()
  "Get the ID entry for the current heading."
  (org-jira-get-from-org 'issue 'filename))

;;;###autoload
(defun org-jira-browse-issue ()
  "Open the current issue in external browser."
  (interactive)
  (ensure-on-issue
    (browse-url (concat (replace-regexp-in-string "/*$" "" jiralib-url) "/browse/" (org-jira-id)))))

(defun org-jira-url-copy-file (url newname)
  "Similar to url-copy-file but async."
  (lexical-let ((newname newname))
    (url-retrieve
     url
     (lambda (status)
       (let ((buffer (current-buffer))
             (handle nil)
             (filename (if (and (file-exists-p newname)
                                org-jira-download-ask-override)
                           (read-string "File already exists, select new name or press ENTER to override: " newname)
                         newname)))
         (if (not buffer)
             (error "Opening input file: No such file or directory, %s" url))
         (with-current-buffer buffer
           (setq handle (mm-dissect-buffer t)))
         (mm-save-part-to-file handle filename)
         (kill-buffer buffer)
         (mm-destroy-parts handle))))))

;;;###autoload
(defun org-jira-download-attachment ()
  "Download the attachment under cursor."
  (interactive)
  (when jiralib-use-restapi
    (save-excursion
      (org-up-heading-safe)
      (org-back-to-heading)
      (forward-thing 'whitespace)
      (unless (looking-at "Attachments:")
        (error "Not on a attachment region!")))
    (let ((filename (org-entry-get (point) "Name"))
          (url (org-entry-get (point) "Content"))
          (url-request-extra-headers `(,jiralib-token)))
      (org-jira-url-copy-file
       url
       (concat (file-name-as-directory org-jira-download-dir) filename)))))

;;;###autoload
(defun org-jira-get-issues-from-filter (filter)
  "Get issues from the server-side stored filter named FILTER.

Provide this command in case some users are not able to use
client side jql (maybe because of JIRA server version?)."
  (interactive
   (list (completing-read "Filter: " (mapcar 'cdr (jiralib-get-saved-filters)))))
  (org-jira-get-issues (jiralib-get-issues-from-filter (car (rassoc filter (jiralib-get-saved-filters))))))

;;;###autoload
(defun org-jira-get-issues-from-filter-headonly (filter)
  "Get issues *head only* from saved filter named FILTER.
See `org-jira-get-issues-from-filter'."
  (interactive
   (list (completing-read "Filter: " (mapcar 'cdr (jiralib-get-saved-filters)))))
  (org-jira-get-issues-headonly (jiralib-get-issues-from-filter (car (rassoc filter (jiralib-get-saved-filters))))))

(org-add-link-type "jira" 'org-jira-open)

;; This was only added in org 9.0, not sure all org users will have
;; that version, so keep the deprecated one from above for now.

;;(org-link-set-parameters "jira" ((:follow . 'org-jira-open)))

(defun org-jira-open (path)
  "Open a Jira Link from PATH."
  (org-jira-get-issue path))

;;;###autoload
(defun org-jira-get-issues-by-board ()
  "Get list of ISSUES from agile board."
  (interactive)
  (let* ((board (org-jira-read-board))
         (board-id (cdr board)))
    (jiralib-get-board-issues board-id
                              :callback org-jira-get-issue-list-callback
                              :limit (org-jira-get-board-limit board-id)
                              :query-params (org-jira--make-jql-queryparams board-id))))

;;;###autoload
(defun org-jira-get-issues-by-sprint ()
  "Get list of ISSUES from sprint."
  (interactive)
  (let* ((board (org-jira-read-board))
	 (board-id (cdr board))
	 (sprint (org-jira-read-sprint board-id))
	 (sprint-id (cdr sprint)))
    (jiralib-get-sprint-issues sprint-id
			       :callback org-jira-get-issue-list-callback
			       :limit (org-jira-get-board-limit board-id)
			       :query-params (org-jira--make-jql-queryparams board-id))))

(defun org-jira-get-board-limit (id)
  "Get limit for number of retrieved issues for a board
id - integer board id"
  (let ((board (org-jira--get-board-from-buffer id)))
    (if (and board (slot-boundp board 'limit))
        (oref board limit)
      org-jira-boards-default-limit)))

(defun org-jira--make-jql-queryparams (board-id)
  "make GET query parameters for jql, returns nil if JQL query is not set"
  (let* ((board (org-jira--get-board-from-buffer board-id))
         (jql (if (and board (slot-boundp board 'jql))
                  (oref board jql))))
    (if (and jql (not (string-blank-p jql))) `((jql ,jql)))))

;;;###autoload
(defun org-jira-get-issues-by-board-headonly ()
  "Get list of ISSUES from agile board, head only."
  (interactive)
  (let* ((board (org-jira-read-board))
         (board-id (cdr board)))
    (org-jira-get-issues-headonly
     (jiralib-get-board-issues board-id
                               :limit (org-jira-get-board-limit board-id)
                               :query-params (org-jira--make-jql-queryparams board-id)))))


(defun org-jira--render-boards-from-list (boards)
  "Add the boards from list into the org file.

boards -  list of `org-jira-sdk-board' records."
  (mapc 'org-jira--render-board  boards))


(defun org-jira--render-board (board)
  "Render single board"
  ;;(org-jira-sdk-dump board)
  (with-slots (id name url board-type jql limit) board
    (with-current-buffer (org-jira--get-boards-buffer)
      (org-jira-maybe-activate-mode)
      (org-jira-freeze-ui
        (org-save-outline-visibility t
          (save-restriction
            (outline-show-all)
            (widen)
            (goto-char (point-min))
            (let* ((board-headline
                    (format "Board: [[%s][%s]]" url name))
                   (headline-pos
                    (org-find-exact-headline-in-buffer board-headline (current-buffer) t))
                   (entry-exists (and headline-pos (>= headline-pos (point-min)) (<= headline-pos (point-max))))
                   (limit-value  (if (slot-boundp board 'limit) (int-to-string  limit) nil))
                   (jql-value    (if (slot-boundp board 'jql) jql nil)))
              (if entry-exists
                  (progn
                    (goto-char headline-pos)
                    (org-narrow-to-subtree)
                    (end-of-line))
                (goto-char (point-max))
                (unless (looking-at "^")
                  (insert "\n"))
                (insert "* ")
                (org-jira-insert board-headline)
                (org-narrow-to-subtree))
              (org-jira-entry-put (point) "name" name)
              (org-jira-entry-put (point) "type" board-type)
              (org-jira-entry-put (point) "url"  url)
              ;; do not overwrite existing user properties with empty values
              (if (or (not entry-exists) limit-value)
                  (org-jira-entry-put (point) "limit" limit-value))
              (if (or (not entry-exists) jql-value)
                  (org-jira-entry-put (point) "JQL" jql-value ))
              (org-jira-entry-put (point) "ID"   id))))))))

(defun org-jira--get-boards-file ()
  (expand-file-name "boards-list.org" (org-jira--ensure-working-dir)))

(defun org-jira--get-boards-buffer ()
  "Return buffer for list of agile boards. Create one if it does not exist."
  (let* ((boards-file  (org-jira--get-boards-file))
         (existing-buffer (find-buffer-visiting boards-file)))
    (if existing-buffer
        existing-buffer
      (find-file-noselect boards-file))))

;;;###autoload
(defun org-jira-get-boards ()
  "Get list of boards and their properties."
  (interactive)
  (let* ((datalist (jiralib-get-boards))
         (boards (org-jira-sdk-create-boards-from-data-list datalist)))
    (org-jira--render-boards-from-list boards))
  (switch-to-buffer (org-jira--get-boards-buffer)))

(defun org-jira--get-board-from-buffer (id)
  "Parse board record from org file."
  (with-current-buffer (org-jira--get-boards-buffer)
    (org-jira-freeze-ui
      (let ((pos (org-find-property "ID" (int-to-string  id))))
        (if pos
            (progn
              (goto-char pos)
              (apply 'org-jira-sdk-board
                     (cl-reduce
                      #'(lambda (acc entry)
                          (let* ((pname   (car entry))
                                 (pval (cdr entry))
                                 (pair (and pval
                                            (not (string-empty-p pval))
                                            (cond
                                             ((equal pname "ID")
                                              (list :id pval))
                                             ((equal pname "URL")
                                              (list :url pval))
                                             ((equal pname "TYPE")
                                              (list :board-type pval))
                                             ((equal pname "NAME")
                                              (list :name pval))
                                             ((equal pname "LIMIT")
                                              (list :limit (string-to-number pval)))
                                             ((equal pname "JQL")
                                              (list :jql pval))
                                             (t nil)))))
                            (if pair  (append pair acc)  acc)))
                      (org-entry-properties) :initial-value  ()))))))))

(defun org-jira-get-org-keyword-from-status (status)
  "Gets an 'org-mode' keyword corresponding to a given jira STATUS."
  (if org-jira-use-status-as-todo
      (upcase (replace-regexp-in-string " " "-" status))
    (let ((known-keyword (assoc status org-jira-jira-status-to-org-keyword-alist)))
      (cond (known-keyword (cdr known-keyword))
            ((member (org-jira-decode status) org-jira-done-states) "DONE")
            ("TODO")))))

(defun org-jira-get-org-priority-string (character)
  "Return an org-priority-string based on CHARACTER and user settings."
  (cond ((not character) "")
        ((and org-jira-priority-to-org-priority-omit-default-priority
              (eq character org-default-priority)) "")
        (t (format "[#%c] " character))))

(defun org-jira-get-org-priority-cookie-from-issue (priority)
  "Get the `org-mode' [#X] PRIORITY cookie."
  (let ((character (cdr (assoc priority org-jira-priority-to-org-priority-alist))))
    (org-jira-get-org-priority-string character)))

(provide 'org-jira)
;;; org-jira.el ends here
