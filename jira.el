;;; jira.el --- Connect to JIRA issue tracking software

;; Copyright (C) 2009 Brian Zwahr
;; original Copyright (C) 2007  Dave Benjamin

;; Authors: 
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>
;; Version: 0.3.3
;; Last modified: October 12, 2009

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; **********
;;; Commentary
;;; **********

;; This file provides jira-mode, an emacs major mode for connecting to and 
;; using a Jira server. (http://www.atlassian.com/software/jira/). This 
;; jira-mode is far from complete (more below) but is mostly usable as is 
;; for the features that are present.

;; Note that some functions/processes can be a bit slow. I believe this has 
;; something to do with XMLRPC.

;; Also, XMLRPC access to jira is incomplete. Certain Jira features cannot be 
;; used via XMLRPC such as (but not limited to):
;; - Changing ticket status
;; - Closing/resolving tickets
;; - Watching a ticket

;; All of the XML-RPC API is wrapped, though not all of the API is exposed
;; via interactive functions. For API details, see:

;; http://confluence.atlassian.com/pages/viewpage.action?pageId=1035
;; http://www.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/xmlrpc/XmlRpcService.html

;;; *************
;;; Configuration
;;; *************

;; 1.) Load the file jira.el, either manuall or place (require 'jira) in your .emacs with jira.el in the load path.
;; 2.) Customize the variable jira-url to point to the XML-RPC url of the Jira
;; installation to be accessed.
;; 3.) The faces can be customized for different look/feel/highlighting.

;;; *****
;;; Usage
;;; *****

;; M-x jira-mode will load the major mode into a new buffer named *Jira*.
;; You will be asked to login; use the username/password for the Jira server.
;; A few internal lists should be populated automatically containing a list
;; of projects, issue types, etc. 

;; The following commands/keyboard shorcuts can be used:

;; li - jira-list-issues
;; lp - jira-list-projects
;; lf - jira-list-filters
;; si - jira-search-issues
;; sp - jira-search-project-issues
;; i - jira-show-issue
;; c - jira-create-ticket
;; o - jira-comment-ticket
;; r - jira-refresh-ticket
;; a - jira-assign-ticket
;; n - jira-next-comment
;; p - jira-previous-comment
;; jl - jira-login
;; jL - jira-logout
;; Q - jira-mode-quit

;; When viewing an issues, pressing o, r, etc. acts upon that issue. 
;; For instance, while viewing an issue, pressing o will ask for a comment. 
;; That comment will be posted to the issue currently being viewed.

;; Some prompts have tab completions in the minibuffer/echo area. Try it out to
;; see which prompts do and which do not.

;;; Code:
(require 'cl)
(require 'xml-rpc)

;; **************************
;; Jira Mode - by Brian Zwahr
;; **************************

(defgroup jira nil
  "Jira customization group."
  :group 'applications)

(defgroup jira-faces nil 
  "Faces for displaying Jira information."
  :group 'jira)

(defcustom jira-url ""
  "User customizable URL to Jira server."
  :group 'jira
  :type 'string
  :initialize 'custom-initialize-set)

(defcustom jira-host ""
  "User customizable host name of the Jira server, will be used to compute jira-url if the latter is not set."
  :group 'jira
  :type 'string
  :initialize 'custom-initialize-set)

(defface jira-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira-faces)

(defface jira-issue-info-header-face
  '((t (:bold t :inherit 'jira-issue-info-face)))
  "Base face for issue headers."
  :group 'jira-faces)

(defface jira-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira-faces)

(defface jira-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira-faces)

(defface jira-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira-faces)

(defface jira-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira-faces)

(defface jira-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira-faces)

(defface jira-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira-faces)

(defvar jira-mode-hook nil)

(defvar jira-mode-map nil)

(if jira-mode-map
    nil
  (progn
    (setq jira-mode-map (make-sparse-keymap))
    (define-key jira-mode-map "li" 'jira-list-issues)
    (define-key jira-mode-map "lp" 'jira-list-projects)
    (define-key jira-mode-map "lf" 'jira-list-filters)
    (define-key jira-mode-map "si" 'jira-search-issues)
    (define-key jira-mode-map "sp" 'jira-search-project-issues)
    (define-key jira-mode-map "i" 'jira-show-issue)
    (define-key jira-mode-map "c" 'jira-create-ticket)
    (define-key jira-mode-map "o" 'jira-comment-ticket)
    (define-key jira-mode-map "r" 'jira-refresh-ticket)
    (define-key jira-mode-map "a" 'jira-assign-ticket)
    (define-key jira-mode-map "n" 'jira-next-comment)
    (define-key jira-mode-map "p" 'jira-previous-comment)
    (define-key jira-mode-map "jl" 'jira-login)
    (define-key jira-mode-map "jL" 'jira-logout)
    (define-key jira-mode-map "Q" 'jira-mode-quit)
    (define-key jira-mode-map [return] 'jira-return)))

(defun jira-mode ()
  "A mode for working with the Jira ticketing system. XMLRPC is used via xmlrpc.el. Things run a bit slow, though sometimes they seems to run faster when doing multiple things at once to the same ticket: i.e. retrieve a ticket, its slow, comment the tickets, its faster, refresh the ticket its faster, wait a while then refresh and its slow again. 

\\{jira-mode-map}"
  (interactive)
  (if (and (or (not jira-host)
	      (equal jira-host ""))
	   (or (equal jira-url nil)
	       (equal jira-url "")))
      (message "jira-url not set! Please use 'M-x customize-variable RET jira-url RET'!")
    (when (or (equal jira-url nil)
	      (equal jira-url ""))
      (setq jira-url (concat "http://" jira-host "/jira/rpc/xmlrpc")))
    (progn
      (switch-to-buffer "*Jira*")
      (kill-all-local-variables)
      (setq major-mode 'jira-mode)
      (setq mode-name "Jira")
      (use-local-map jira-mode-map)
      (run-hooks 'jira-mode-hook)
      (jira-store-projects)
      (jira-store-priorities)
      (jira-store-statuses)
      (jira-store-types)
      (insert "Welcome to jira-mode!")
      (message "jira mode loaded!"))))

(defvar jira-current-issue nil
  "This holds the currently selected issue.")

(defvar jira-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira-types nil
  "This holds a list of issues types.")

(defvar jira-statuses nil
  "This holds a list of statuses.")

(defvar jira-priorities nil
  "This holds a list of priorities.")

(defvar jira-user-fullnames nil
  "This holds a list of user fullnames.")

(defun jira-mode-quit ()
  (interactive)
  (jira-logout)
  (kill-buffer "*Jira*"))

(defun jira-create-ticket (project type summary description)
  (interactive (list (read-string "Project: ")
                     (read-string "Type: ")
                     (read-string "Summary: ")
                     (read-string "Description: ")))
  (if (or (equal project "")
          (equal type "")
          (equal summary "")
          (equal description ""))
      (message "Must provide all information!")
    (progn
      (setq ticket-alist (list (cons "project" project) 
                               (cons "type" type) 
                               (cons "summary" summary) 
                               (cons "description" description)))
      (jira-create-issue ticket-alist))))

(defun jira-refresh-ticket ()
  (interactive)
  (jira-show-issue jira-current-issue))

(defun jira-comment-ticket (comment)
  (interactive (list (read-string "Comment: ")))
  (if (equal comment "")
      (message "Must provide comment!")
    (progn
      (jira-add-comment jira-current-issue comment)
      (jira-refresh-ticket))))

(defun jira-assign-ticket (assignee)
  (interactive (list (read-string "Assignee: ")))
  (if (equal assignee "")
      (message "Must provide assignee!")
    (progn
      (setq ticket-alist (list (cons "assignee" (vector assignee))))
      (jira-update-issue jira-current-issue ticket-alist)
      (jira-refresh-ticket))))

(defun jira-update-ticket-summary (summary)
  (interactive (list (read-string "Summary: ")))
  (if (equal summary "")
      (message "Must provide summary!")
    (progn
      (setq ticket-alist (list (cons "summary" (vector summary))))
      (jira-update-issue jira-current-issue ticket-alist)
      (jira-refresh-ticket))))

(defun jira-start-ticket ()
  (interactive)
  (setq ticket-alist (list (cons "status" (vector "3"))))
  (jira-update-issue jira-current-issue ticket-alist))

(defun jira-store-projects ()
  (setf jira-projects-list (jira-get-projects)))

(defun jira-store-types ()
  (setf jira-types (jira-get-issue-types)))

(defun jira-store-statuses ()
  (setf jira-statuses (jira-get-statuses)))

(defun jira-store-priorities ()
  (setf jira-priorities (jira-get-priorities)))

(defun jira-get-project-name (key)
  (let ((projects jira-projects-list)
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc "key" project)) key)
          (setf name (cdr (assoc "name" project)))))
    name))

(defun jira-get-type-name (id)
  (let ((types jira-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc "id" type)) id)
          (setf name (cdr (assoc "name" type)))))
    name))

(defun jira-get-status-name (id)
  (let ((statuses jira-statuses)
        (name nil))
    (dolist (status statuses)
      (if (equal (cdr (assoc "id" status)) id)
          (setf name (cdr (assoc "name" status)))))
    name))

(defun jira-get-priority-name (id)
  (let ((priorities jira-priorities)
        (name nil))
    (dolist (priority priorities)
      (if (equal (cdr (assoc "id" priority)) id)
          (setf name (cdr (assoc "name" priority)))))
    (message name)))

(defun jira-get-user-fullname (username)
  (if (assoc username jira-user-fullnames)
      (cdr (assoc username jira-user-fullnames))
    (progn
      (let ((user (jira-get-user username)))
        (setf jira-user-fullnames (append jira-user-fullnames (list (cons username (cdr (assoc "fullname" user))))))
        (cdr (assoc "fullname" user))))))

(defun jira-next-comment ()
  (interactive)
  (let ((p (point)))
    (if (search-forward "Comment #" nil t)
        (progn
          (if (equal p (- (point) 9))
              (search-forward "Comment #" nil t))
          (recenter 0)
          (beginning-of-line)))))

(defun jira-previous-comment ()
  (interactive)
  (if (search-backward "Comment #" nil t)
      (progn
        (recenter 0)
        (beginning-of-line))
    (goto-char 0)))

(defun jira-return ()
  (interactive)
  (if (equal (face-at-point) 'jira-link-issue-face)
      (jira-show-issue (thing-at-point 'sexp)))
  (if (equal (face-at-point) 'jira-link-project-face)
      (jira-search-project-issues (thing-at-point 'sexp) "" 20))
  (if (equal (face-at-point) 'jira-link-filter-face)
      (jira-list-issues (thing-at-point 'sexp))))

(defun point-on-issue-p ()
  (save-excursion
    (search-backward " ")))

(defun delete-eob-whitespace ()
  (end-of-buffer)
  (delete-horizontal-space)
  (delete-char -1)
  (beginning-of-buffer))

;; ***********************************
;; original functions by Dave Benjamin
;; modifications by Brian Zwahr noted
;; ***********************************

(defvar jira-token nil
  "JIRA token used for authentication")

(defun jira-login (username password)
  "Logs the user into JIRA."
  (interactive (let ((found (nth 0 (auth-source-search :max 1
                                           :host jira-host
                                           :port 80
                                           :require '(:user :secret)
                                           :create t)))
	  user secret)
      (when found
	  (setq user (plist-get found :user)
		secret
		(let ((sec (plist-get found :secret)))
		  (if (functionp sec)
		      (funcall sec)
		    sec)))
	  (list user secret))))
  (setq jira-token (jira-call-noauth 'jira1.login username password)))

(defun jira-logout ()
  "Logs the user out of JIRA"
  (interactive)
  (jira-call 'jira1.logout)
  (setq jira-token nil))

(defun jira-list-projects ()
  "Displays a list of all available JIRA projects"
  (interactive)
  (let ((projects (jira-get-projects)))
    (jira-with-jira-buffer
     (insert (number-to-string (length projects)) " JIRA projects found:\n\n")
     (dolist (project projects)
       (insert (format "%-12s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert 
           (cdr (assoc "key" project)))
          (point))
        '(face jira-link-project-face))
       (beginning-of-line)
       (forward-char 12)
       (insert (format "%s\n"
                       (cdr (assoc "name" project)))))))
  (delete-eob-whitespace))

(defun jira-list-filters ()
  "Displays a list of all saved JIRA filters"
  (interactive)
  (let ((filters (jira-get-saved-filters)))
    (jira-with-jira-buffer
     (insert (number-to-string (length filters)) " JIRA filters found:\n\n")
     (dolist (filter filters)
       (insert (format "%-8s" " "))
       (beginning-of-line)
       (add-text-properties
        (point)
        (save-excursion
          (insert (cdr (assoc "id" filter)))
          (point))
        '(face jira-link-filter-face))
       (beginning-of-line)
       (forward-char 8)
       (insert (format " %s\n"
                       (cdr (assoc "name" filter)))))))
  (delete-eob-whitespace))

(defun jira-list-issues (filter-id)
  "Displays a list of issues matching a filter"
  (interactive
   (list (let ((filter-alist (jira-get-filter-alist)))
           (cdr (assoc (completing-read "Filter: " filter-alist nil t)
                filter-alist)))))
    (when filter-id
      (let ((filter (jira-get-filter filter-id))
            (issues (jira-get-issues-from-filter filter-id)))
        (jira-with-jira-buffer
         (insert "Filter:\n" (cdr (assoc "name" filter))
                 " (" (cdr (assoc "id" filter)) ")\n\n")
         (when (cdr (assoc "description" filter))
           (insert "Description:\n")
           (let ((start (point)))
             (insert (cdr (assoc "description" filter)) "\n\n")
             (fill-region start (point))))
         (jira-display-issues issues)))))

(defun jira-search-issues (text)
  "Displays a list of issues maching a fulltext search"
  (interactive "sSearch: ")
  (let ((issues (jira-get-issues-from-text-search text)))
    (jira-with-jira-buffer
     (insert "Search: " text "\n\n")
     (jira-display-issues issues))))

(defun jira-search-project-issues (project text max-results)
  "Displays a list of issues within a project matching a fulltext search"
  (interactive
   (let ((project-keys
          (mapcar (lambda (project)
                    (cdr (assoc "key" project)))
                  (jira-get-projects))))
     (list
      (completing-read "Project Key: " project-keys nil t)
      (read-string "Search: ")
      (read-number "Max Results: " 20))))
  (let ((issues (jira-get-issues-from-text-search-with-project
                 (list project) (if (equal text "") " " text) max-results)))
    (jira-with-jira-buffer
     (insert "Project Key: " project "\n"
             "Search: " text "\n"
             "Max Results: " (number-to-string max-results) "\n\n")
     (jira-display-issues issues))))

; Modified by Brian Zwahr to store issue key and improve layout/readability.
(defun jira-show-issue (issue-key)
  "Displays details about a particular issue."
  (interactive "sIssue Key: ")
  (let ((issue (jira-get-issue issue-key))
        (comments (jira-get-comments issue-key)))
    (setf jira-current-issue issue-key)
    (jira-with-jira-buffer
     (setq truncate-lines nil)
     (let ((s "Project:   "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-project-name (cdr (assoc "project" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Key:       "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc "key" issue))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Type:      "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-type-name (cdr (assoc "type" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Status:    "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-status-name (cdr (assoc "status" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Priority:  "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-priority-name (cdr (assoc "priority" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Assignee:  "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-user-fullname (cdr (assoc "assignee" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Reporter:  "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (jira-get-user-fullname (cdr (assoc "reporter" issue)))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Created:   "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc "created" issue))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Updated:   "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (cdr (assoc "updated" issue))))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n"))

     (let ((s "Watchers:  "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s "N/A"))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n\n"))

     (let ((s "Component(s): "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (if (cdr (assoc "name" (cdr (assoc "components" issue)))) (cdr (assoc "name" (cdr (assoc "components" issue)))) "None")))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n\n"))

     (let ((s "Fix Version(s): "))
       (put-text-property 0 (length s) 'face 'jira-issue-info-header-face s)
       (insert s))
     (let ((s (if (cdr (assoc "fixVersions" issue)) (cdr (assoc "fixVersions" issue)) "None")))
       (put-text-property 0 (length s) 'face 'jira-issue-info-face s)
       (insert s "\n\n"))

     (let ((s (cdr (assoc "summary" issue))))
       (put-text-property 0 (length s) 'face 'jira-issue-summary-face s)
       (insert s "\n\n"))

     (insert (concatenate 'string (cdr (assoc "description" issue)) "\n\n"))

     (when comments
       (let ((count 1))
         (dolist (comment comments)
           (insert "Comment #" (int-to-string count) "\n")
           (let ((s (concatenate 'string (jira-get-user-fullname (cdr (assoc "author" comment))) " - " (cdr (assoc "created" comment)))))
             (put-text-property 0 (length s) 'face 'jira-comment-header-face s)
             (insert s "\n"))
           (let ((c (jira-strip-cr (cdr (assoc "body" comment)))))
             
             (put-text-property 0 (length c) 'face 'jira-comment-face c)
             (insert c "\n\n"))
           (setf count (1+ count))))))))

(defun jira-send-region-as-comment (start end issue-key)
  "Send the currently selected region as an issue comment"
  (interactive "r\nsIssue Key: ")
  (jira-add-comment issue-key (buffer-substring start end)))

(defun jira-get-filter (filter-id)
  "Returns a filter given its filter ID."
  (flet ((id-match (filter)
                   (equal filter-id (cdr (assoc "id" filter)))))
    (find-if 'id-match (jira-get-saved-filters))))

(defun jira-get-filter-alist ()
  "Returns an association list mapping filter names to IDs"
  (mapcar (lambda (filter)
            (cons (cdr (assoc "name" filter))
                  (cdr (assoc "id" filter))))
          (jira-get-saved-filters)))

(defun jira-get-status-abbrevs ()
  "Returns an association list of status IDs to abreviated names"
  (flet ((pair (status)
               (cons (cdr (assoc "id" status))
                     (let ((status-name (cdr (assoc "name" status))))
                       (substring (replace-regexp-in-string
                                   " *" "" status-name)
                                  0 (min 3 (length status-name)))))))
    (mapcar 'pair (jira-get-statuses))))

(defun jira-display-issues (issues)
  "Inserts a list of issues into the current buffer"
  (let ((status-abbrevs (jira-get-status-abbrevs))
        (last-status))
    (insert (number-to-string (length issues))
            " JIRA issues found:\n")
    (dolist (issue issues)
      (let ((status (cdr (assoc "status" issue)))
            (priority (cdr (assoc "priority" issue))))
        (when (not (equal last-status status))
          (setq last-status status)
          (insert "\n"))
        (insert (format "%-16s" " "))
        (beginning-of-line)
        (add-text-properties
         (point)
         (save-excursion
           (insert 
            (cdr (assoc "key" issue)))
           (point))
         '(face jira-link-issue-face))
        (beginning-of-line)
        (forward-char 16)
        (insert (format "%-10s %s %5s %s\n"
                        (cdr (assoc "assignee" issue))
                        (cdr (assoc status status-abbrevs))
                        (if priority
                            (make-string (- 6 (string-to-number priority))
                                         ?*)
                          "")
                        (cdr (assoc "summary" issue)))))))
  (delete-eob-whitespace))

(defun jira-add-comment (issue-key comment)
  "Adds a comment to an issue"
  (jira-call 'jira1.addComment issue-key comment))

(defun jira-create-issue (r-issue-struct)
  "Creates an issue in JIRA from a Hashtable object."
  (jira-call 'jira1.createIssue r-issue-struct))

(defun jira-get-comments (issue-key)
  "Returns all comments associated with the issue"
  (jira-call 'jira1.getComments issue-key))

(defun jira-get-components (project-key)
  "Returns all components available in the specified project"
  (jira-call 'jira1.getComponents project-key))

(defun jira-get-issue (issue-key)
  "Gets an issue from a given issue key."
  (jira-call 'jira1.getIssue issue-key))

(defun jira-get-issues-from-filter (filter-id)
  "Executes a saved filter"
  (jira-call 'jira1.getIssuesFromFilter filter-id))

(defun jira-get-issues-from-text-search (search-terms)
  "Find issues using a free text search"
  (jira-call 'jira1.getIssuesFromTextSearch search-terms))

(defun jira-get-issues-from-text-search-with-project
  (project-keys search-terms max-num-results)
  "Find issues using a free text search, limited to certain projects"
  (jira-call 'jira1.getIssuesFromTextSearchWithProject
             project-keys search-terms max-num-results))

(defun jira-get-issue-types ()
  "Returns all visible issue types in the system"
  (jira-call 'jira1.getIssueTypes))

(defun jira-get-priorities ()
  "Returns all priorities in the system"
  (jira-call 'jira1.getPriorities))

;; Modified by Brian Zwahr to use getProjectsNoSchemes instead of getProjects
(defun jira-get-projects ()
  "Returns a list of projects available to the user"
  (jira-call 'jira1.getProjectsNoSchemes))

(defun jira-get-resolutions ()
  "Returns all resolutions in the system"
  (jira-call 'jira1.getResolutions))

(defun jira-get-saved-filters ()
  "Gets all saved filters available for the currently logged in user"
  (jira-call 'jira1.getSavedFilters))

(defun jira-get-server-info ()
  "Returns the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jira-call 'jira1.getServerInfo))

(defun jira-get-statuses ()
  "Returns all statuses in the system"
  (jira-call 'jira1.getStatuses))

(defun jira-get-sub-task-issue-types ()
  "Returns all visible subtask issue types in the system"
  (jira-call 'jira1.getSubTaskIssueTypes))

(defun jira-get-user (username)
  "Returns a user's information given a username"
  (jira-call 'jira1.getUser username))

(defun jira-get-versions (project-key)
  "Returns all versions available in the specified project"
  (jira-call 'jira1.getVersions project-key))

(defun jira-update-issue (issue-key field-values)
  "Updates an issue in JIRA from a Hashtable object."
  (jira-call 'jira1.updateIssue issue-key field-values))

(defun jira-ensure-token ()
  "Makes sure that a JIRA token has been set, logging in if necessary."
  (unless jira-token
    (let ((found (nth 0 (auth-source-search :max 1
                                           :host jira-host
                                           :port 80
                                           :require '(:user :secret)
                                           :create t)))
	  user secret)
      (when found
	  (setq user (plist-get found :user)
		secret
		(let ((sec (plist-get found :secret)))
		  (if (functionp sec)
		      (funcall sec)
		    sec)))
	  (jira-login user secret)))))


(defun jira-call (method &rest params)
  "Calls an XML-RPC method on the JIRA server (low-level)"
  (jira-ensure-token)
  (apply 'jira-call-noauth method jira-token params))

(defun jira-call-noauth (method &rest params)
  "Calls an XML-RPC method on the JIRA server without authentication (low-level)"
  (let ((url-version "Exp")             ; hack due to status bug in xml-rpc.el
        (server-url jira-url))
    (apply 'xml-rpc-method-call server-url method params)))

(defun jira-strip-cr (string)
  "Removes carriage returns from a string"
  (when string (replace-regexp-in-string "\r" "" string)))

;; Modified by Brian Zwahr to a specific *Jira* buffer, not a temp buffer
(defmacro jira-with-jira-buffer (&rest body)
  "Sends all output and buffer modifications to *Jira* buffer."
  `(with-current-buffer "*Jira*" 
     (delete-region (point-min) (point-max))
     (setq truncate-lines t)
     ,@body
     (beginning-of-buffer)))

(provide 'jira)
;;; jira.el ends here
