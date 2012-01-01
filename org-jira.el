;;; org-jira.el --- Syncing between Jira and Org-mode.

;; Author: Bao Haojun <baohaojun@gmail.com>

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


(require 'org)
(require 'jira2)

(defgroup org-jira nil
  "Post to weblogs from Emacs"
  :group 'org-jira)

(defvar org-jira-working-dir "~/../.org-jira"
  "Folder under which to store org-jira working files")
(defvar jira-users (list (cons "Full Name" "username"))
  "Jira has not api for discovering all users, so we should provide it somewhere else")

(defcustom org-jira-serv-alist nil
  "Association list to set information for each jira server.
Each element of the alist is a jira server name.  The CAR of each
element is a string, uniquely identifying the server.  The CDR
of each element is a well-formed property list with an even
number of elements, alternating keys and values, specifying
parameters for the server.

     (:property value :property value ... )

When a property is given a value in org-jira-serv-alist, its
setting overrides the value of the corresponding user
variable (if any) during syncing.

Most properties are optional, but some should always be set:

  :url                     soap url of the jira server.
  :username                username to be used.
  :host                    hostname of the jira server (TODO: compute it from ~url~).

All the other properties are optional. They over-ride the global variables.

  :password                password to be used, will be prompted if missing.
"
  :group 'org-jira
  :type '(alist :value-type plist))


(defvar org-jira-serv nil
  "Parameters of the currently selected blog.")

(defvar org-jira-serv-name nil
  "Name of the blog, to pick from `org-jira-serv-alist'")

(defvar org-jira-projects-list nil
  "List of jira projects.")

(defvar org-jira-current-project nil
  "currently selected (i.e., active project).")

(defvar org-jira-issues-list nil
  "List of jira issues under the current project.")

(defvar org-jira-server-rpc-url nil
  "Jira server soap URL")

(defvar org-jira-server-userid nil
  "Jira server user id")

(defvar org-jira-proj-id nil
  "Jira project ID")

(defvar org-jira-logged-in nil
  "Flag whether user is logged-in or not")

(defvar org-jira-buffer-name "*org-jira-%s*"
  "Name of the jira buffer")

(defvar org-jira-buffer-kill-prompt t
  "Ask before killing buffer")
(make-variable-buffer-local 'org-jira-buffer-kill-prompt)

(defconst org-jira-version "0.1"
  "Current version of org-jira.el")

(defvar org-jira-mode-hook nil
  "Hook to run upon entry into mode.")

(defun org-jira-kill-buffer-hook ()
  "Prompt before killing buffer."
  (if (and org-jira-buffer-kill-prompt
	   (not (buffer-file-name)))
      (if (y-or-n-p "Save Jira?")
	  (progn
	    (save-buffer)
	    (org-jira-save-details (org-jira-parse-entry) nil
				   (y-or-n-p "Published?"))))))

(defvar org-jira-entry-mode-map
  (let ((org-jira-map (make-sparse-keymap)))
    (define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
    (define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
    (define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
    (define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
    (define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
    (define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
    (define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
    (define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
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

(defun org-jira-get-projects ()
  "Get list of projects."
  (interactive)
  (let ((projects-file (expand-file-name "projects-list.org" org-jira-working-dir)))
    (or (find-buffer-visiting projects-file)
	(find-file projects-file))
    (org-jira-mode t)
    (save-excursion
      (let* ((oj-projs (jira2-get-projects)))
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
  "Return the components the issue belongs to."
  (mapconcat (lambda (comp)
	       (cdr (assoc 'name comp)))
	     (cdr (assoc 'components issue)) ", "))

(defun org-jira-get-issue-val (key issue)
  "Return the value associated with key KEY of issue ISSUE."
  (cond ((eq key 'components)
	 (org-jira-get-issue-components issue))
	((eq key 'status)
	 (or (cdr (assoc (cdr (assoc 'status issue)) (jira2-get-statuses))) ""))
	((eq key 'resolution)
	 (or (cdr (assoc (cdr (assoc 'resolution issue)) (jira2-get-resolutions))) ""))
	((eq key 'type)
	 (or (cdr (assoc (cdr (assoc 'type issue)) (jira2-get-issue-types))) ""))
	((eq key 'priority)
	 (or (cdr (assoc (cdr (assoc 'priority issue)) (jira2-get-prioritys))) ""))
	((eq key 'description)
	 (org-jira-strip-string (or (cdr (assoc 'description issue)) "")))
	(t
	 (or (cdr (assoc key issue)) ""))))


(defun org-jira-get-issues (issues)
  "Get list of issues."
  (interactive
   (list (jira2-do-jql-search (format "assignee = %s and resolution = unresolved ORDER BY priority DESC, created ASC" jira2-user-login-name))))
  (mapc (lambda (issue)
	    (let* ((proj-key (cdr (assoc 'project issue)))
		   (issue-id (cdr (assoc 'key issue)))
		   (issue-summary (cdr (assoc 'summary issue)))
		   (issue-headline issue-summary))
	      (let ((project-file (expand-file-name (concat proj-key ".org") org-jira-working-dir)))
		(with-current-buffer (or (find-buffer-visiting project-file)
					 (find-file project-file))
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
		    (insert (concat (if (member (org-jira-get-issue-val 'status issue) '("Closed" "Resolved"))
					"DONE "
				      "TODO ")
				    issue-headline))
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
			  '(assignee reporter type priority resolution status components))
		    (org-entry-put (point) "ID" (cdr (assoc 'key issue)))

		    (mapc (lambda (heading-entry)
			    (ensure-on-issue-id 
				issue-id
			      
			      (let* ((entry-heading (concat (symbol-name heading-entry) ": " issue-id)))
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

				(insert (org-jira-get-issue-val heading-entry issue)))))
			  '(description))
		    (org-jira-update-comments-for-current-issue)
		    )))))
	  issues))

(defun org-jira-update-comment ()
  "update a comment for the current issue"
  (interactive)
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
	 (comment-id (org-jira-get-from-org 'comment 'id))
	 (comment (org-jira-get-comment-body comment-id)))
    (if comment-id
	(jira2-edit-comment comment-id comment)
      (jira2-add-comment issue-id comment)
      (org-jira-delete-current-comment)
      (org-jira-update-comments-for-current-issue))))

(defun org-jira-delete-current-comment ()
  "delete the current comment"
  (ensure-on-comment
   (delete-region (point-min) (point-max))))

(defun org-jira-update-comments-for-current-issue ()
  (let* ((issue-id (org-jira-get-from-org 'issue 'key))
	 (comments (jira2-get-comments issue-id)))
    (mapc (lambda (comment)
	    (ensure-on-issue-id issue-id
	      (let* ((comment-id (cdr (assoc 'id comment)))
		     (comment-author (or (car (rassoc 
					       (cdr (assoc 'author comment))
					       jira-users))
					 (cdr (assoc 'author comment))))							  
		     (comment-headline (format "Comment: %s" comment-author)))
		(setq p (org-find-entry-with-id comment-id))
		(if (and p (>= p (point-min))
			 (<= p (point-max)))
		    (progn
		      (goto-char p)
		      (org-narrow-to-subtree)
		      (delete-region (point-min) (point-max))))
		(goto-char (point-max))
		(unless (looking-at "^")
		  (insert "\n"))
		(insert "** ")
		(insert comment-headline "\n")
		(org-narrow-to-subtree)
		(org-entry-put (point) "ID" comment-id)
		(goto-char (point-max))
		(insert (or (cdr (assoc 'body comment)) "")))))
	  (mapcan (lambda (comment) (if (string= (cdr (assoc 'author comment))
						 "admin")
					nil
				      (list comment))) 
		  comments))))
  

(defun org-jira-update-issue ()
  "update an issue"
  (interactive)
  (let ((issue-id (org-jira-parse-issue-id)))
    (if issue-id
	(org-jira-update-issue-details issue-id)
      (error "not on an issue"))))

(defun org-jira-todo-to-jira ()
  "convert an ordinary todo item to a jira ticket"
  (interactive)
  (ensure-on-todo
   (when (org-jira-parse-issue-id)
     (error "Already on jira ticket"))
   (let ((issue (org-jira-create-issue 
		 (org-jira-read-project)
		 (org-jira-read-issue-type)
		 (org-get-heading t t)
		 (org-get-entry))))
     (delete-region (point-min) (point-max))
     (org-jira-get-issues (list issue)))))

(defvar org-jira-project-read-history nil)
(defvar org-jira-type-read-history nil)

(defun org-jira-read-project ()
  "Read project name"
  (completing-read 
   "Project: "
   (jira2-make-list (jira2-get-projects) 'key)
   nil
   t
   (car org-jira-project-read-history)
   'org-jira-project-read-history))

(defun org-jira-read-issue-type ()
  "Read issue type name"
  (completing-read
   "Type: "
   (mapcar 'cdr (jira2-get-issue-types))
   nil
   t
   (car org-jira-type-read-history)
   'org-jira-type-read-history))
		   
(defun org-jira-create-issue (project type summary description)
  "create an issue"
  (interactive (list (org-jira-read-project)
		     (org-jira-read-issue-type)
		     (read-string "Summary: ")
                     (read-string "Description: ")))
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))
  (let* ((project-components (jira2-get-components project))
	 (user (completing-read "Assignee: " (mapcar 'car jira-users))))
    (setq ticket-alist (list (cons 'project project)
			     (cons 'type (car (rassoc type (jira2-get-issue-types)))) 
			     (cons 'summary summary) 
			     (cons 'description description)
			     (cons 'assignee (cdr (assoc user jira-users)))))
    (jira2-create-issue ticket-alist)))

(defun org-jira-strip-string (str)
  "remove the beginning and ending white space for a string"
  (replace-regexp-in-string "\\`\n+\\|\n+\\'" "" str))

(defun org-jira-get-issue-val-from-org (key)
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
  "Read issue workflow progress actions."
  (let ((action (completing-read
		 "Action: "
		 (mapcar 'cdr actions)
		 nil
		 t
		 (car org-jira-actions-history)
		 'org-jira-actions-history)))
    (car (rassoc action actions))))

(defvar org-jira-resolution-history nil)
(defun org-jira-read-resolution ()
  "Read issue workflow progress resolution."
  (let ((resolution (completing-read
		     "Resolution: "
		     (mapcar 'cdr (jira2-get-resolutions))
		     nil
		     t
		     (car org-jira-resolution-history)
		     'org-jira-resolution-history)))
    (car (rassoc resolution (jira2-get-resolutions)))))

(defun org-jira-refresh-issue ()
  "Refresh issue from jira to org"
  (interactive)
  (ensure-on-issue
   (let* ((issue-id (org-jira-id)))
     (org-jira-get-issues (list (jira2-get-issue issue-id))))))

(defun org-jira-progress-issue ()
  "Progress issue workflow"
  (interactive)
  (ensure-on-issue
   (let* ((issue-id (org-jira-id))
	  (actions (jira2-get-available-actions issue-id))
	  (action (org-jira-read-action actions))
	  (resolution (org-jira-read-resolution))
	  (issue (jira2-progress-workflow-action issue-id action `((resolution . ,resolution)))))
     (org-jira-get-issues (list issue)))))
     
     
(defun org-jira-update-issue-details (issue-id)
  (ensure-on-issue-id 
      issue-id
    (let* ((org-issue-components (org-jira-get-issue-val-from-org 'components))
	   (org-issue-description (org-jira-get-issue-val-from-org 'description))
	   (org-issue-resolution (org-jira-get-issue-val-from-org 'resolution))
	   (org-issue-priority (org-jira-get-issue-val-from-org 'priority))
	   (org-issue-type (org-jira-get-issue-val-from-org 'type))
	   (org-issue-status (org-jira-get-issue-val-from-org 'status))
	   (issue (jira2-get-issue issue-id))
	   (project (org-jira-get-issue-val 'project issue))
	   (project-components (jira2-get-components project)))
      
      (jira2-update-issue issue-id ;(jira2-update-issue "FB-1" '((components . ["10001" "10000"])))
			  (list (cons 
				 'components 
				 (apply 'vector 
					(mapcan 
					 (lambda (item)
					   (let ((comp-id (car (rassoc item project-components))))
					     (if comp-id
						 (list comp-id)
					       nil)))
					 (split-string org-issue-components ",\\s *"))))
				(cons 'priority (car (rassoc org-issue-priority (jira2-get-prioritys))))
				(cons 'description org-issue-description)
				(cons 'summary (org-jira-get-issue-val-from-org 'summary))))
      (org-jira-get-issues (list (jira2-get-issue issue-id))))))


(defun org-jira-parse-issue-id ()
  "get issue id from org text"
  (save-excursion
    (let ((continue t)
	  issue-id)
      (while continue
	(when (string-match (jira2-get-issue-regexp) 
			    (or (setq issue-id (org-entry-get (point) "ID"))
				""))
	  (setq continue nil))
	(unless (and continue (org-up-heading-safe))
	  (setq continue nil)))
      issue-id)))

(defun org-jira-get-from-org (type entry)
  "get org ENTRY for heading of TYPE.

TYPE can be 'issue, or 'comment.

ENTRY will vary with regard to the TYPE, if it is a symbol, it will be converted to string"

  (when (symbolp entry)
    (setq entry (symbol-name entry)))

  (if (eq type 'issue)
      (org-jira-get-issue-val-from-org entry)
    (if (eq type 'comment)
	(org-jira-get-comment-val-from-org entry)
      (error "unknown type %s" type))))

(defun org-jira-get-comment-val-from-org (entry)
  "get the jira issue field value for ENTRY of current comment item"
  (ensure-on-comment
   (when (symbolp entry)
     (setq entry (symbol-name entry)))
   (when (string= entry "id")
     (setq entry "ID"))
   (org-entry-get (point) entry)))

(defun org-jira-get-comment-body (&optional comment-id)
  (ensure-on-comment
   (goto-char (point-min))
   (org-entry-put (point) "ID" comment-id)
   (search-forward ":END:")
   (forward-line)
   (org-jira-strip-string (buffer-substring-no-properties (point) (point-max)))))

(defun org-jira-id ()
  "get the ID entry for the current HEADING."
  (org-entry-get (point) "ID"))

(defmacro ensure-on-issue (&rest body)
  "Make sure we are on an issue heading"

  `(save-excursion
     (while (org-up-heading-safe)) ; goto the top heading
     (let ((org-jira-id (org-jira-id)))
       (unless (and org-jira-id (string-match (jira2-get-issue-regexp) org-jira-id))
	 (error "Not on a issue region!")))
     ,@body))

(defmacro ensure-on-issue-id (issue-id &rest body)
  "Make sure we are on an issue heading with id ISSUE-ID"
  (declare (indent 1))
  `(save-excursion
     (save-restriction
       (widen)
       (show-all)
       (goto-char (point-min))
       (let (p)
	 (setq p (org-find-entry-with-id ,issue-id))
	 (unless p
	   (error "issue %s not found!" ,issue-id))
	 (goto-char p)
	 (org-narrow-to-subtree)
	 ,@body))))

(defmacro ensure-on-todo (&rest body)
  "Make sure we are on an todo heading"
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
  "Make sure we are on a comment heading"
  `(save-excursion
     (org-back-to-heading)
     (forward-thing 'whitespace)
     (unless (looking-at "Comment:")
       (error "Not on a comment region!"))
     (save-restriction
       (org-narrow-to-subtree)
       ,@body)))

(defun org-jira-browse-issue ()
  "Open the current issue in external browser."
  (interactive)
  (ensure-on-issue
   (w3m-external-view (concat jira2-url "/browse/" (org-jira-id)))))
(provide 'org-jira)

