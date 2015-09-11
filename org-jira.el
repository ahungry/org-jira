;;; org-jira.el --- Syncing between Jira and Org-mode.

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/baohaojun/org-jira

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; This provides an extension to org-mode for syncing issues with JIRA
;; issue servers.
;;
;;; Code:

(eval-when-compile (load-library "cl-extra"))
(require 'org)
(require 'jiralib)
(require 'cl-lib)
(require 'cl)

(defgroup org-jira nil
  "Customisation group for org-jira."
  :tag "Org JIRA"
  :group 'org)

(defvar org-jira-working-dir "~/.org-jira"
  "Folder under which to store org-jira working files.")

(defcustom org-jira-default-jql
  "assignee = currentUser() and resolution = unresolved ORDER BY
  priority DESC, created ASC"
  "Default jql for querying your Jira tickets."
  :group 'org-jira
  :type 'string)

(defcustom org-jira-done-states
  '("Closed" "Resolved" "Done")
  "Jira states that should be considered as DONE for `org-mode'."
  :group 'org-jira
  :type '(repeat (string :tag "Jira state name:")))

(defvar jira-users (list (cons "Full Name" "username"))
  "Jira has not api for discovering all users, so we should provide it somewhere else.")

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

(defconst org-jira-version "0.1"
  "Current version of org-jira.el.")

(defvar org-jira-mode-hook nil
  "Hook to run upon entry into mode.")

(defvar org-jira-issue-id-history '()
  "Prompt history for issue id.")

(defmacro ensure-on-issue (&rest body)
  "Make sure we are on an issue heading, before executing BODY."

  `(save-excursion
     (while (org-up-heading-safe)) ; go to the top heading
     (let ((org-jira-id (org-jira-id)))
       (unless (and org-jira-id (string-match (jiralib-get-issue-regexp) org-jira-id))
         (error "Not on a issue region!")))
     ,@body))

(defmacro ensure-on-issue-id (issue-id &rest body)
  "Make sure we are on an issue heading with id ISSUE-ID, before executing BODY."
  (declare (indent 1))
  `(save-excursion
     (save-restriction
       (widen)
       (show-all)
       (goto-char (point-min))
       (let (p)
         (setq p (org-find-entry-with-id ,issue-id))
         (unless p
           (error "Issue %s not found!" ,issue-id))
         (goto-char p)
         (org-narrow-to-subtree)
         ,@body))))

(defmacro ensure-on-todo (&rest body)
  "Make sure we are on an todo heading, before executing BODY."
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
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Worklog:")
       (error "Not on a worklog region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defun org-jira-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org-jira-buffer-kill-prompt
           (not (buffer-file-name)))
      (if (y-or-n-p "Save Jira? ")
          (progn
            (save-buffer)
            (org-jira-save-details (org-jira-parse-entry) nil
                                   (y-or-n-p "Published? "))))))

(defvar org-jira-entry-mode-map
  (let ((org-jira-map (make-sparse-keymap)))
    (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
    (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
    (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
    (define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
    (define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
    (define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
    (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
    (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
    (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
    (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
    (define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
    (define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
    (define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
    (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
    (define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklog)
    (define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
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
      (run-mode-hooks 'org-jira-mode-hook)))

;;;###autoload
(defun org-jira-get-projects ()
  "Get list of projects."
  (interactive)
  (let ((projects-file (expand-file-name "projects-list.org" org-jira-working-dir)))
    (or (find-buffer-visiting projects-file)
        (find-file projects-file))
    (org-jira-mode t)
    (save-excursion
      (let* ((oj-projs (jiralib-get-projects)))
        (mapc (lambda (proj)
                (let* ((proj-key (cdr (assoc 'key proj)))
                       (proj-headline (format "Project: [[file:%s.org][%s]]" proj-key proj-key)))
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (show-all)
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
                      (insert proj-headline)
                      (org-narrow-to-subtree))
                    (org-entry-put (point) "name" (cdr (assoc 'name proj)))
                    (org-entry-put (point) "key" (cdr (assoc 'key proj)))
                    (org-entry-put (point) "lead" (cdr (assoc 'lead proj)))
                    (org-entry-put (point) "ID" (cdr (assoc 'id proj)))
                    (org-entry-put (point) "url" (cdr (assoc 'url proj))))))
              oj-projs)))))

(defun org-jira-get-issue-components (issue)
  "Return the components the ISSUE belongs to."
  (mapconcat (lambda (comp)
               (cdr (assoc 'name comp)))
             (cdr (assoc 'components issue)) ", "))

(defun org-jira-transform-time-format (jira-time-str)
  "Convert JIRA-TIME-STR to format \"%Y-%m-%d %T\".

Example: \"2012-01-09T08:59:15.000Z\" becomes \"2012-01-09
16:59:15\", with the current timezone being +0800."
  (condition-case ()
      (format-time-string "%Y-%m-%d %T"
                          (apply
                           'encode-time
                           (parse-time-string (replace-regexp-in-string "T\\|\\.000" " " jira-time-str))))
    (error jira-time-str)))

(defun org-jira--fix-encode-time-args (arg)
  "Fix ARG for 3 nil values at the head."
  (loop
   for n from 0 to 2 by 1 do
   (when (not (nth n arg))
     (setcar (nthcdr n arg) 0)))
  arg)

(defun org-jira-time-format-to-jira (org-time-str)
  "Convert ORG-TIME-STR back to jira time format."
  (condition-case ()
      (format-time-string "%Y-%m-%dT%T.000Z"
                          (apply 'encode-time
                                 (org-jira--fix-encode-time-args (parse-time-string org-time-str))) t)
    (error org-time-str)))

(defun org-jira-get-comment-val (key comment)
  "Return the value associated with KEY of COMMENT."
  (org-jira-get-issue-val key comment))

(defun org-jira-get-worklog-val (key WORKLOG)
  "Return the value associated with KEY of WORKLOG."
  (org-jira-get-comment-val key WORKLOG))

(defun org-jira-get-issue-val (key issue)
  "Return the value associated with key KEY of issue ISSUE."
  (let ((tmp  (or (cdr (assoc key issue)) "")))
    (cond ((eq key 'components)
           (org-jira-get-issue-components issue))
          ((member key '(created updated startDate))
           (org-jira-transform-time-format tmp))
          ((eq key 'status)
           (cdr (assoc tmp (jiralib-get-statuses))))
          ((eq key 'resolution)
           (cdr (assoc tmp (jiralib-get-resolutions))))
          ((eq key 'type)
           (cdr (assoc tmp (jiralib-get-issue-types))))
          ((eq key 'priority)
           (cdr (assoc tmp (jiralib-get-priorities))))
          ((eq key 'description)
           (org-jira-strip-string tmp))
          (t
           tmp))))

(defvar org-jira-jql-history nil)
(defun org-jira-get-issue-list ()
  "Get list of issues, using jql (jira query language).

Default is unresolved issues assigned to current login user; with
a prefix argument you are given the chance to enter your own
jql."
  (let ((jql org-jira-default-jql))
    (when current-prefix-arg
      (setq jql (read-string "Jql: "
                             (if org-jira-jql-history
                                 (car org-jira-jql-history)
                               "assignee = currentUser() and resolution = unresolved")
                             'org-jira-jql-history
                             "assignee = currentUser() and resolution = unresolved")))
    (list (jiralib-do-jql-search jql))))

(defun org-jira-get-issue-by-id (id)
  "Get an issue by its ID."
  (interactive (list (read-string "Issue ID: " "IMINAN-" 'org-jira-issue-id-history)))
  (push id org-jira-issue-id-history)
  (let ((jql (format "id = %s" id)))
    (jiralib-do-jql-search jql)))

;;;###autoload
(defun org-jira-get-issues-headonly (issues)
  "Get list of ISSUES, head only.

The default behavior is to return issues assigned to you and unresolved.

With a prefix argument, allow you to customize the jql.  See
`org-jira-get-issue-list'."

  (interactive
   (org-jira-get-issue-list))

  (let* ((issues-file (expand-file-name "issues-headonly.org" org-jira-working-dir))
         (issues-headonly-buffer (or (find-buffer-visiting issues-file)
                                     (find-file issues-file))))
    (with-current-buffer issues-headonly-buffer
      (widen)
      (delete-region (point-min) (point-max))

      (mapc (lambda (issue)
              (let ((issue-id (org-jira-get-issue-val 'key issue))
                    (issue-summary (org-jira-get-issue-val 'summary issue)))
                (insert (format "- [jira:%s] %s\n" issue-id issue-summary))))
            issues))
    (switch-to-buffer issues-headonly-buffer)))

;;;###autoload
(defun org-jira-get-issue ()
  "Get a JIRA issue, allowing you to enter the issue-id first."
  (interactive)
  (org-jira-get-issues (call-interactively 'org-jira-get-issue-by-id)))

;;;###autoload
(defun org-jira-get-issues (issues)
  "Get list of ISSUES into an org buffer.

Default is get unfinished issues assigned to you, but you can
customize jql with a prefix argument.
See`org-jira-get-issue-list'"

  (interactive
   (org-jira-get-issue-list))
  (let (project-buffer)
    (mapc (lambda (issue)
            (let* ((proj-key (cdr (assoc 'project issue)))
                   (issue-id (cdr (assoc 'key issue)))
                   (issue-summary (cdr (assoc 'summary issue)))
                   (issue-headline issue-summary))
              (let ((project-file (expand-file-name (concat proj-key ".org") org-jira-working-dir)))
                (setq project-buffer (or (find-buffer-visiting project-file)
                                         (find-file project-file)))
                (with-current-buffer project-buffer
                  (org-jira-mode t)
                  (widen)
                  (show-all)
                  (goto-char (point-min))
                  (setq p (org-find-entry-with-id issue-id))
                  (save-restriction
                    (if (and p (>= p (point-min))
                             (<= p (point-max)))
                        (progn
                          (goto-char p)
                          (forward-thing 'whitespace)
                          (kill-line))
                      (goto-char (point-max))
                      (unless (looking-at "^")
                        (insert "\n"))
                      (insert "* "))
                    (let ((status (org-jira-get-issue-val 'status issue)))
                      (insert (concat (cond (org-jira-use-status-as-todo
                                             (upcase (replace-regexp-in-string " " "-" status)))
                                            ((member status org-jira-done-states) "DONE")
                                            ("TODO")) " "
                                            issue-headline)))
                    (save-excursion
                      (unless (search-forward "\n" (point-max) 1)
                        (insert "\n")))
                    (org-narrow-to-subtree)
                    (org-change-tag-in-region
                     (point-min)
                     (save-excursion
                       (forward-line 1)
                       (point))
                     (replace-regexp-in-string "-" "_" issue-id)
                     nil)

                    (mapc (lambda (entry)
                            (let ((val (org-jira-get-issue-val entry issue)))
                              (when (and val (not (string= val "")))
                                (org-entry-put (point) (symbol-name entry) val))))
                          '(assignee reporter type priority resolution status components created updated))
                    (org-entry-put (point) "ID" (cdr (assoc 'key issue)))

                    (mapc (lambda (heading-entry)
                            (ensure-on-issue-id
                                issue-id
                              (let* ((entry-heading (concat (symbol-name heading-entry) (format ": [[%s][%s]]" (concat jiralib-url "/browse/" issue-id) issue-id))))
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
                                    (org-insert-subheading t))
                                  (insert entry-heading "\n"))

                                (insert (replace-regexp-in-string "^" "  " (org-jira-get-issue-val heading-entry issue))))))
                          '(description))
                    (org-jira-update-comments-for-current-issue)
                    (org-jira-update-worklogs-for-current-issue)
                    )))))
          issues)
    (switch-to-buffer project-buffer)))

;;;###autoload
(defun org-jira-update-comment ()
  "Update a comment for the current issue."
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (comment-id (org-jira-get-from-org 'comment 'id))
         (comment (replace-regexp-in-string "^  " "" (org-jira-get-comment-body comment-id))))
    (if comment-id
        (jiralib-edit-comment comment-id comment)
      (jiralib-add-comment issue-id comment)
      (org-jira-delete-current-comment)
      (org-jira-update-comments-for-current-issue))))

(defun org-jira-update-worklog ()
  "Update a worklog for the current issue."
  (interactive)
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
                      (org-read-date nil nil nil "Inputh when did you start")))
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

(defun org-jira-update-comments-for-current-issue ()
  "Update the comments for the current issue."
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (comments (jiralib-get-comments issue-id)))
    (mapc (lambda (comment)
            (ensure-on-issue-id issue-id
              (let* ((comment-id (cdr (assoc 'id comment)))
                     (comment-author (or (car (rassoc
                                               (cdr (assoc 'author comment))
                                               jira-users))
                                         (cdr (assoc 'author comment))))
                     (comment-headline (format "Comment: %s" comment-author)))
                (setq p (org-find-entry-with-id comment-id))
                (when (and p (>= p (point-min))
                           (<= p (point-max)))
                  (goto-char p)
                  (org-narrow-to-subtree)
                  (delete-region (point-min) (point-max)))
                (goto-char (point-max))
                (unless (looking-at "^")
                  (insert "\n"))
                (insert "** ")
                (insert comment-headline "\n")
                (org-narrow-to-subtree)
                (org-entry-put (point) "ID" comment-id)
                (let ((created (org-jira-get-comment-val 'created comment))
                      (updated (org-jira-get-comment-val 'updated comment)))
                  (org-entry-put (point) "created" created)
                  (unless (string= created updated)
                    (org-entry-put (point) "updated" updated)))
                (goto-char (point-max))
                (insert (replace-regexp-in-string "^" "  " (or (cdr (assoc 'body comment)) ""))))))
          (cl-mapcan (lambda (comment) (if (string= (cdr (assoc 'author comment))
                                               "admin")
                                      nil
                                    (list comment)))
                     comments))))

(defun org-jira-update-worklogs-for-current-issue ()
  "Update the worklogs for the current issue."
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
         (worklogs (jiralib-get-worklogs issue-id)))
    (mapc (lambda (worklog)
            (ensure-on-issue-id issue-id
              (let* ((worklog-id (concat "worklog-" (cdr (assoc 'id worklog))))
                     (worklog-author (or (car (rassoc
                                               (cdr (assoc 'author worklog))
                                               jira-users))
                                         (cdr (assoc 'author worklog))))
                     (worklog-headline (format "Worklog: %s" worklog-author)))
                (setq p (org-find-entry-with-id worklog-id))
                (when (and p (>= p (point-min))
                           (<= p (point-max)))
                  (goto-char p)
                  (org-narrow-to-subtree)
                  (delete-region (point-min) (point-max)))
                (goto-char (point-max))
                (unless (looking-at "^")
                  (insert "\n"))
                (insert "** ")
                (insert worklog-headline "\n")
                (org-narrow-to-subtree)
                (org-entry-put (point) "ID" worklog-id)
                (let ((created (org-jira-get-worklog-val 'created worklog))
                      (updated (org-jira-get-worklog-val 'updated worklog)))
                  (org-entry-put (point) "created" created)
                  (unless (string= created updated)
                    (org-entry-put (point) "updated" updated)))
                (org-entry-put (point) "startDate" (org-jira-get-worklog-val 'startDate worklog))
                (org-entry-put (point) "timeSpent" (org-jira-get-worklog-val 'timeSpent worklog))
                (goto-char (point-max))
                (insert (replace-regexp-in-string "^" "  " (or (cdr (assoc 'comment worklog)) ""))))))
          worklogs)))


;;;###autoload
(defun org-jira-update-issue ()
  "Update an issue."
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id)))
    (if issue-id
        (org-jira-update-issue-details issue-id)
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

(defvar org-jira-project-read-history nil)
(defvar org-jira-priority-read-history nil)
(defvar org-jira-type-read-history nil)

(defun org-jira-read-project ()
  "Read project name."
  (completing-read
   "Project: "
   (jiralib-make-list (jiralib-get-projects) 'key)
   nil
   t
   (car org-jira-project-read-history)
   'org-jira-project-read-history))

(defun org-jira-read-priority ()
  "Read priority name."
  (completing-read
   "Priority: "
   (mapcar 'cdr (jiralib-get-priorities))
   nil
   t
   (car org-jira-priority-read-history)
   'org-jira-priority-read-history))

(defun org-jira-read-issue-type ()
  "Read issue type name."
  (completing-read
   "Type: "
   (mapcar 'cdr (jiralib-get-issue-types))
   nil
   t
   (car org-jira-type-read-history)
   'org-jira-type-read-history))

(defun org-jira-read-subtask-type ()
  "Read issue type name."
  (completing-read
   "Type: "
   (mapcar 'cdr (jiralib-get-subtask-types))
   nil
   t
   (car org-jira-type-read-history)
   'org-jira-type-read-history))

(defun org-jira-get-issue-struct (project type summary description)
  "Create an issue struct for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((project-components (jiralib-get-components project))
         (user (completing-read "Assignee: " (mapcar 'car jira-users)))
         (priority (car (rassoc (org-jira-read-priority) (jiralib-get-priorities))))
         (ticket-struct (list (cons 'project project)
                              (cons 'type (car (rassoc type (if (and (boundp 'parent-id) parent-id)
                                                                (jiralib-get-subtask-types)
                                                              (jiralib-get-issue-types)))))
                              (cons 'summary (format "%s%s" summary
                                                     (if (and (boundp 'parent-id) parent-id)
                                                         (format " (subtask of [jira:%s])" parent-id)
                                                       "")))
                              (cons 'description description)
                              (cons 'priority priority)
                              (cons 'assignee (cdr (assoc user jira-users))))))
    ticket-struct))
;;;###autoload
(defun org-jira-create-issue (project type summary description)
  "Create an issue in PROJECT, of type TYPE, with given SUMMARY and DESCRIPTION."
  (interactive (list (org-jira-read-project)
                     (org-jira-read-issue-type)
                     (read-string "Summary: ")
                     (read-string "Description: ")))
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
         (ticket-struct (org-jira-get-issue-struct project type summary description)))
    (org-jira-get-issues (list (jiralib-create-subtask ticket-struct parent-id)))))

(defun org-jira-strip-string (str)
  "Remove the beginning and ending white space for a string STR."
  (replace-regexp-in-string "\\`\n+\\|\n+\\'" "" str))

(defun org-jira-get-issue-val-from-org (key)
  "Return the requested value by KEY from the current issue."
  (ensure-on-issue
   (cond ((eq key 'description)
          (org-goto-first-child)
          (forward-thing 'whitespace)
          (if (looking-at "description: ")
              (org-jira-strip-string (org-get-entry))
            (error "Can not find description field for this issue")))
         ((eq key 'summary)
          (ensure-on-issue
           (org-get-heading t t)))
         (t
          (when (symbolp key)
            (setq key (symbol-name key)))
          (when (string= key "key")
            (setq key "ID"))
          (or (org-entry-get (point) key)
              "")))))

(defvar org-jira-actions-history nil)
(defun org-jira-read-action (actions)
  "Read issue workflow progress ACTIONS."
  (let ((action (completing-read
                 "Action: "
                 (mapcar 'cdr actions)
                 nil
                 t
                 (car org-jira-actions-history)
                 'org-jira-actions-history)))
    (car (rassoc action actions))))

(defvar org-jira-fields-history nil)
(defun org-jira-read-field (fields)
  "Read (custom) FIELDS for workflow progress."
  (let ((field-desc (completing-read
                     "More fields to set: "
                     (cons "Thanks, no more fields are *required*." (mapcar 'cdr fields))
                     nil
                     t
                     nil
                     'org-jira-fields-history))
        field-name)
    (setq field-name (car (rassoc field-desc fields)))
    (if field-name
        (intern field-name)
      field-name)))


(defvar org-jira-resolution-history nil)
(defun org-jira-read-resolution ()
  "Read issue workflow progress resolution."
  (let ((resolution (completing-read
                     "Resolution: "
                     (mapcar 'cdr (jiralib-get-resolutions))
                     nil
                     t
                     (car org-jira-resolution-history)
                     'org-jira-resolution-history)))
    (car (rassoc resolution (jiralib-get-resolutions)))))

;;;###autoload
(defun org-jira-refresh-issue ()
  "Refresh issue from jira to org."
  (interactive)
  (ensure-on-issue
   (let* ((issue-id (org-jira-id)))
     (org-jira-get-issues (list (jiralib-get-issue issue-id))))))

(defvar org-jira-fields-values-history nil)
;;;###autoload
(defun org-jira-progress-issue ()
  "Progress issue workflow."
  (interactive)
  (ensure-on-issue
   (let* ((issue-id (org-jira-id))
          (actions (jiralib-get-available-actions issue-id))
          (action (org-jira-read-action actions))
          (fields (jiralib-get-fields-for-action issue-id action))
          (field-key)
          (custom-fields-collector nil)
          (custom-fields (progn
                                        ; delete those elements in fields, which have
                                        ; already been set in custom-fields-collector

                           (while fields
                             (setq fields (cl-remove-if (lambda (strstr)
                                                       (cl-member-if (lambda (symstr)
                                                                    (string= (car strstr)  (symbol-name (car symstr))))
                                                                  custom-fields-collector))
                                                     fields))
                             (setq field-key (org-jira-read-field fields))
                             (if (not field-key)
                                 (setq fields nil)
                               (setq custom-fields-collector
                                     (cons
                                      (cons field-key
                                            (if (eq field-key 'resolution)
                                                (org-jira-read-resolution)
                                              (completing-read
                                               (format "Please enter %s's value: "
                                                       (cdr (assoc (symbol-name field-key) fields)))
                                               org-jira-fields-values-history
                                               nil
                                               nil
                                               nil
                                               'org-jira-fields-values-history)))
                                      custom-fields-collector))))
                           custom-fields-collector))
          (issue (jiralib-progress-workflow-action issue-id action custom-fields)))
     (org-jira-get-issues (list issue)))))


(defun org-jira-update-issue-details (issue-id)
  "Update the details of issue ISSUE-ID."
  (ensure-on-issue-id
      issue-id
    (let* ((org-issue-components (org-jira-get-issue-val-from-org 'components))
           (org-issue-description (replace-regexp-in-string "^  " "" (org-jira-get-issue-val-from-org 'description)))
           (org-issue-resolution (org-jira-get-issue-val-from-org 'resolution))
           (org-issue-priority (org-jira-get-issue-val-from-org 'priority))
           (org-issue-type (org-jira-get-issue-val-from-org 'type))
           (org-issue-assignee (org-jira-get-issue-val-from-org 'assignee))
           (org-issue-status (org-jira-get-issue-val-from-org 'status))
           (issue (jiralib-get-issue issue-id))
           (project (org-jira-get-issue-val 'project issue))
           (project-components (jiralib-get-components project)))

      (jiralib-update-issue issue-id ; (jiralib-update-issue "FB-1" '((components . ["10001" "10000"])))
                            (list (cons
                                   'components
                                   (apply 'vector
                                          (cl-mapcan
                                           (lambda (item)
                                             (let ((comp-id (car (rassoc item project-components))))
                                               (if comp-id
                                                   (list comp-id)
                                                 nil)))
                                           (split-string org-issue-components ",\\s *"))))
                                  (cons 'priority (car (rassoc org-issue-priority (jiralib-get-priorities))))
                                  (cons 'description org-issue-description)
                                  (cons 'assignee org-issue-assignee)
                                  (cons 'summary (org-jira-get-issue-val-from-org 'summary))))
      (org-jira-get-issues (list (jiralib-get-issue issue-id))))))


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
   (org-entry-put (point) "ID" comment-id)
   (search-forward ":END:")
   (forward-line)
   (org-jira-strip-string (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-get-worklog-comment (&optional worklog-id)
  "Get the worklog comment of the worklog with id WORKLOG-ID."
  (ensure-on-worklog
   (goto-char (point-min))
   ;; so that search for :END: won't fail
   (org-entry-put (point) "ID" worklog-id)
   (search-forward ":END:")
   (forward-line)
   (org-jira-strip-string (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-id ()
  "Get the ID entry for the current heading."
  (org-entry-get (point) "ID"))

;;;###autoload
(defun org-jira-browse-issue ()
  "Open the current issue in external browser."
  (interactive)
  (ensure-on-issue
   (browse-url (concat jiralib-url "/browse/" (org-jira-id)))))

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

(defun org-jira-open (path)
  "Open a Jira Link from PATH."
  (org-jira-get-issues (list (jiralib-get-issue path))))

(provide 'org-jira)
;;; org-jira.el ends here
