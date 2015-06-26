;;; mantislib.el -- Provide connectivity to MANTIS SOAP service

;; Copyright (C) 2011 Bao Haojun

;; Authors:
;; Bao Haojun <baohaojun@gmail.com>

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

;; Author: Alexandru Harsanyi (AlexHarsanyi@gmail.com)
;; Created: December, 2009
;; Keywords: soap, web-services, mantis
;; Homepage: http://code.google.com/p/emacs-soap-client

;;; Commentary:
;; This file provides a programatic interface to MANTIS.  It provides access to
;; MANTIS from other programs, but no user level functionality.

(eval-when-compile (require 'cl))
(require 'soap-client)
(require 'url-parse)

;;; Code:
(defgroup mantislib nil
  "Mantislib customization group."
  :group 'applications)

(defgroup mantislib-faces nil
  "Faces for displaying Mantislib information."
  :group 'mantislib)

(defcustom mantislib-host ""
  "User customizable host name of the Mantislib server.

This will be used with USERNAME to compute password from
.authinfo file.  Will be calculated from mantislib-url if not set."
  :group 'mantislib
  :type 'string
  :initialize 'custom-initialize-set)

(defface mantislib-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'mantislib-faces)

(defface mantislib-issue-info-header-face
  '((t (:bold t :inherit 'mantislib-issue-info-face)))
  "Base face for issue headers."
  :group 'mantislib-faces)

(defface mantislib-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'mantislib-faces)

(defface mantislib-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'mantislib-faces)

(defface mantislib-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'mantislib-faces)

(defface mantislib-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'mantislib-faces)

(defface mantislib-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'mantislib-faces)

(defface mantislib-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'mantislib-faces)

(defvar mantislib-mode-hook nil)

(defvar mantislib-mode-map nil)

(defcustom mantislib-wsdl-descriptor-url
  ""
  "The location for the WSDL descriptor for the MANTIS service.
This is specific to your local MANTIS installation.  The URL is
tipically:

  http://YOUR_INSTALLATION/rpc/soap/mantissoapservice-v2?wsdl

The default value works if MANTIS is located at a hostname named
'mantis'."
  :type 'string
  :group 'mantislib)

(defcustom mantislib-url
  "http://localhost:18888/"
  "The address of the mantis host."
  :type 'string
  :group 'mantislib)

(defvar mantislib-token nil
  "MANTIS token used for authentication.")

(defvar mantislib-user-login-name nil
  "The name of the user logged into MANTIS.
This is maintained by `mantislib-login'.")

(defvar mantislib-wsdl nil)

(defun mantislib-load-wsdl ()
  "Load the MANTIS WSDL descriptor."
  (setq mantislib-wsdl (soap-load-wsdl-from-url (if (string-equal mantislib-wsdl-descriptor-url "")
                                                  (concat mantislib-url "/rpc/soap/mantissoapservice-v2?wsdl")
                                                mantislib-wsdl-descriptor-url))))

(defun mantislib-login (username password)
  "Login into MANTIS as user USERNAME with PASSWORD.

After a succesful login, store the authentication token in
`mantislib-token'."
  ;; NOTE that we cannot rely on `mantislib-call' because `mantislib-call' relies on
  ;; us ;-)
  (interactive
   (if (> 24 emacs-major-version)
       (let ((user (read-string "Username for Mantis server login? "))
             (password (read-passwd "Password for Mantis server login? ")))
         (list user password))
     (let ((found (nth 0 (auth-source-search :max 1
                                             :host (if (string= mantislib-host "")
                                                       (url-host (url-generic-parse-url mantislib-url))
                                                     mantislib-host)
                                             :port (url-port (url-generic-parse-url mantislib-url))
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
         (list user secret)))))
  (unless mantislib-wsdl
    (mantislib-load-wsdl))
  (setq mantislib-token
        (car (soap-invoke mantislib-wsdl "mantissoapservice-v2" "login" username password)))
  (setq mantislib-user-login-name username)

  ;; At this poing, soap-invoke didn't raise an error, so the login
  ;; credentials are OK.  use them to log into the web interface as
  ;; well, as this will be used to link issues (an operation which is
  ;; not exposed to the SOAP interface.
  ;;
  ;; Note that we don't validate the response at all -- not sure how we
  ;; would do it...

  (let ((url (concat mantislib-url "/secure/Dashboard.jspa?"
                     (format "&os_username=%s&os_password=%s&os_cookie=true"
                             username password))))
    (let ((url-request-method "POST")
          (url-package-name "Emacs mantislib.el")
          (url-package-version "1.0")
          (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
          (url-request-data "abc")
          (url-request-coding-system 'utf-8)
          (url-http-attempt-keepalives t))
      (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
        ;; correctly.  No error does not indicate a succesfull login,
        ;; we would have to parse the HTML page to find that out...
        (with-current-buffer buffer
          (declare (special url-http-response-status))
          (if (> url-http-response-status 299)
              (error "Error logging into MANTIS Web interface %s"
                     url-http-response-status)))
        (kill-buffer buffer)))))

(defun mantislib-call (method &rest params)
  "Invoke the MANTIS METHOD with supplied PARAMS.

This function should be used for all MANTIS interface calls, as the
method ensures the user is logged in and invokes `soap-invoke'
with the correct service name and authentication token.

All MANTIS inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter.  For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `mantislib-call', the call shoulbe be:

  (mantislib-call \"getIssue\" KEY)"
  (car (apply 'mantislib--call-it method params)))

(defun mantislib--call-it (method &rest params)
  "Invoke the MANTIS METHOD with supplied PARAMS.

Internal use, returns a list of responses, of which only the
first is normally used."
  (when (symbolp method)
    (setq method (symbol-name method)))
  (unless mantislib-token
    (call-interactively 'mantislib-login))
  (condition-case data
      (apply 'soap-invoke mantislib-wsdl "mantissoapservice-v2"
             method mantislib-token params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq mantislib-token nil)
     (call-interactively 'mantislib-login)
     (apply 'soap-invoke mantislib-wsdl "mantissoapservice-v2"
            method mantislib-token params))))


;;;; Some utility functions

(defun mantislib-make-list (data field)
  "Map all assoc elements in DATA to the value of FIELD in that element."
  (loop for element in data
        collect (cdr (assoc field element))))
(defun mantislib-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (loop for element in data
        collect (cons (cdr (assoc key-field element))
                      (cdr (assoc value-field element)))))

(defun mantislib-make-remote-field-values (fields)
  "Transform the (KEY . VALUE) list FIELDS into a RemoteFieldValue structure.

Each (KEY . VALUE) pair is transformed into
 ((id . KEY) (values . (VALUE)))

This method exists because Several MANTIS methods require a
RemoteFieldValue list, but it is easier to work with ALISTS in
emacs-lisp"
  (let ((remote-field-values))

    ;; we accept an ALIST of field-name field-values parameter, but we need to
    ;; construct a structure that encodes as a RemoteFieldValue which is what
    ;; updateIssue wants
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (when (symbolp name)
          (setq name (symbol-name name)))
        ;; Value must be an "array" (for which soap-client accepts lists) even
        ;; if it is just one value
        (unless (vectorp value)
          (setq value (vector value)))
        (push `((id . ,name) (values . ,value))
              remote-field-values)))

    (apply 'vector (nreverse remote-field-values))))

;;;; Wrappers around MANTIS methods

(defun mantislib-update-issue (key fields)
  "Update the issue with id KEY with the values in FIELDS."
  (mantislib-call "updateIssue" key (mantislib-make-remote-field-values fields)))


(defvar mantislib-status-codes-cache nil)

(defun mantislib-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask MANTIS for the list of codes once, then
will cache it."
  (unless mantislib-status-codes-cache
    (setq mantislib-status-codes-cache
          (mantislib-make-assoc-list (mantislib-call "getStatuses") 'id 'name)))
  mantislib-status-codes-cache)

(defvar mantislib-issue-types-cache nil)

(defun mantislib-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask MANTIS for the list of codes once, than
will cache it."
  (unless mantislib-issue-types-cache
    (setq mantislib-issue-types-cache
          (mantislib-make-assoc-list (mantislib-call "getIssueTypes") 'id 'name)))
  mantislib-issue-types-cache)

(defvar mantislib-priority-codes-cache nil)

(defun mantislib-get-priorities ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask MANTIS for the list of codes once, than
will cache it."
  (unless mantislib-priority-codes-cache
    (setq mantislib-priority-codes-cache
          (mantislib-make-assoc-list (mantislib-call "getPriorities") 'id 'name)))
  mantislib-priority-codes-cache)

(defvar mantislib-resolution-code-cache nil)

(defun mantislib-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask MANTIS for the list of codes once, than
will cache it."
  (unless mantislib-resolution-code-cache
    (setq mantislib-resolution-code-cache
          (mantislib-make-assoc-list (mantislib-call "getResolutions") 'id 'name)))
  mantislib-resolution-code-cache)

(defvar mantislib-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a MANTIS
;; connection to construct the regexp (the user might be prompted for a MANTIS
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun mantislib-get-issue-regexp ()
  "Return a regexp that will match an issue id.

The regexp is constructed from the project keys in the MANTIS
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless mantislib-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (mantislib-call "getProjectsNoSchemes"))))
      (setq mantislib-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  mantislib-issue-regexp)

(defun mantislib-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that MANTIS
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query."
  (unless (or limit (numberp limit))
    (setq limit 100))
  (mantislib-call "getIssuesFromJqlSearch" jql limit))

(defun mantislib-get-available-actions (issue-key)
  "Return the available workflow actions for ISSUE-KEY.
This runs the getAvailableActions SOAP method."
  (mantislib-make-assoc-list
   (mantislib-call "getAvailableActions" issue-key)
   'id 'name))

(defun mantislib-get-fields-for-action (issue-key action-id)
  "Return the required fields to change ISSUE-KEY to ACTION-ID."
  (mantislib-make-assoc-list
   (mantislib-call "getFieldsForAction" issue-key action-id)
   'id 'name))

(defun mantislib-progress-workflow-action (issue-key action-id params)
  "Progress issue with ISSUE-KEY to action ACTION-ID, and provide the needed PARAMS."
  (mantislib-call "progressWorkflowAction" issue-key action-id (mantislib-make-remote-field-values params)))

(defun mantislib-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begins at START-DATE and has a TIME-SPENT
duration.  MANTIS will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks.

COMMENT will be added to this worklog."
  (mantislib-call "addWorklogAndAutoAdjustRemainingEstimate"
                issue-key
                `((startDate . ,start-date)
                  (timeSpent . ,time-spent)
                  (comment   . ,comment))))

(defun mantislib-link-issue (issue-key link-type other-issue-key)
  "Link ISSUE-KEY with a link of type LINK-TYPE to OTHER-ISSUE-KEY.
LINK-TYPE is a string representing the type of the link, e.g
\"requires\", \"depends on\", etc.  I believe each MANTIS
installation can define its own link types."

  ;; IMPLEMENTATION NOTES: The linking mantis issues functionality is
  ;; not exposed through the SOAP api, we must use the web interface
  ;; to do the linking.  Unfortunately, we cannot parse the result, so
  ;; we don't know that the linking was succesfull or not.  To reduce
  ;; the risk, we use the SOAP api to retrieve the issues for
  ;; ISSUE-KEY and OTHER-ISSUE-KEY.  This will ensure that we are
  ;; logged in (see also mantislib-login) and that both issues exist. We
  ;; don't validate the LINK-TYPE, not sure how to do it.

  (let ((issue (mantislib-get-issue issue-key))
        (other-issue (mantislib-get-issue other-issue-key)))
    (let ((url (concat mantislib-url
                       "/secure/LinkExistingIssue.jspa?"
                       (format "linkDesc=%s&linkKey=%s&id=%s&Link=Link"
                               link-type other-issue-key (cdr (assq 'id issue))))))
      (let ((url-request-method "POST")
            (url-package-name "Emacs scratch.el")
            (url-package-version "1.0")
            (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
            (url-request-data "abc")
            (url-request-coding-system 'utf-8)
            (url-http-attempt-keepalives t)
            ;; see http://confluence.atlassian.com/display/MANTIS/Form+Token+Handling
            (url-request-extra-headers '(("X-Atlassian-Token" . "no-check"))))

        (let ((buffer (url-retrieve-synchronously url)))
          ;; This is just a basic check that the page was retrieved
          ;; correctly.  No error does not indicate a success as we
          ;; have to parse the HTML page to find that out...
          (with-current-buffer buffer
            (declare (special url-http-response-status))
            (if (> url-http-response-status 299)
                (error "Error linking issue through MANTIS Web interface %s"
                       url-http-response-status)))
          (kill-buffer buffer))))))


;;;; Issue field accessors

(defun mantislib-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun mantislib-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun mantislib-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)."
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (mantislib-get-statuses)))))

(defun mantislib-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))

(defvar mantislib-current-issue nil
  "This holds the currently selected issue.")

(defvar mantislib-projects-list nil
  "This holds a list of projects and their details.")

(defvar mantislib-types nil
  "This holds a list of issues types.")

(defvar mantislib-priorities nil
  "This holds a list of priorities.")

(defvar mantislib-user-fullnames nil
  "This holds a list of user fullnames.")

(defun mantislib-get-project-name (key)
  "Return the name of the MANTIS project with id KEY."
  (let ((projects mantislib-projects-list)
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc 'key project)) key)
          (setf name (cdr (assoc 'name project)))))
    name))

(defun mantislib-get-type-name (id)
  "Return the name of the issue type with ID."
  (let ((types mantislib-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc 'id type)) id)
          (setf name (cdr (assoc 'name type)))))
    name))

(defun mantislib-get-user-fullname (username)
  "Return the full name (display name) of the user with USERNAME."
  (if (assoc username mantislib-user-fullnames)
      (cdr (assoc username mantislib-user-fullnames))
    (progn
      (let ((user (mantislib-get-user username)))
        (setf mantislib-user-fullnames (append mantislib-user-fullnames (list (cons username (cdr (assoc 'fullname user))))))
        (cdr (assoc 'fullname user))))))


(defun mantislib-get-filter (filter-id)
  "Return a filter given its FILTER-ID."
  (cl-flet ((id-match (filter)
                      (equal filter-id (cdr (assoc 'id filter)))))
    (cl-find-if 'id-match (mantislib-get-saved-filters))))

(defun mantislib-get-filter-alist ()
  "Return an association list mapping filter names to IDs."
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (mantislib-get-saved-filters)))

(defun mantislib-add-comment (issue-key comment)
  "Add to issue with ISSUE-KEY the given COMMENT."
  (mantislib-call "addComment" issue-key `((body . ,comment))))

(defun mantislib-edit-comment (comment-id comment)
  "Edit comment with COMMENT-ID to reflect the new COMMENT."
  (mantislib-call "editComment" `((id . ,comment-id)
                                (body . ,comment))))

(defun mantislib-create-issue (issue)
  "Create a new ISSUE in MANTISLIB.

ISSUE is a Hashtable object."
  (mantislib-call "createIssue" issue))

(defun mantislib-create-subtask (subtask parent-issue-id)
  "Create SUBTASK for issue with PARENT-ISSUE-ID.

SUBTASK is a Hashtable object."
  (mantislib-call "createIssueWithParent" subtask parent-issue-id))


(defvar mantislib-subtask-types-cache nil)

(defun mantislib-get-subtask-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask MANTIS for the list of codes once, than
will cache it."
  (unless mantislib-subtask-types-cache
    (setq mantislib-subtask-types-cache
          (mantislib-make-assoc-list (mantislib-call "getSubTaskIssueTypes") 'id 'name)))
  mantislib-subtask-types-cache)


(defun mantislib-get-comments (issue-key)
  "Return all comments associated with issue ISSUE-KEY."
  (mantislib-call "getComments" issue-key))

(defun mantislib-get-components (project-key)
  "Return all components available in the project PROJECT-KEY."
  (mantislib-make-assoc-list (mantislib-call "getComponents" project-key) 'id 'name))

(defun mantislib-get-issue (issue-key)
  "Get the issue with key ISSUE-KEY."
  (mantislib-call "getIssue" issue-key))

(defun mantislib-get-issues-from-filter (filter-id)
  "Get the issues from applying saved filter FILTER-ID."
  (mantislib-call "getIssuesFromFilter" filter-id))

(defun mantislib-get-issues-from-text-search (search-terms)
  "Find issues using free text search SEARCH-TERMS."
  (mantislib-call "getIssuesFromTextSearch" search-terms))

(defun mantislib-get-issues-from-text-search-with-project
    (project-keys search-terms max-num-results)
  "Find issues in projects PROJECT-KEYS, using free text search SEARCH-TERMS.

Return no more than MAX-NUM-RESULTS."
  (mantislib-call "getIssuesFromTextSearchWithProject"
                (apply 'vector project-keys) search-terms max-num-results))

;; Modified by Brian Zwahr to use getProjectsNoSchemes instead of getProjects
(defun mantislib-get-projects ()
  "Return a list of projects available to the user."
  (if mantislib-projects-list
      mantislib-projects-list
    (setq mantislib-projects-list (mantislib-call "getProjectsNoSchemes"))))

(defun mantislib-get-saved-filters ()
  "Get all saved filters available for the currently logged in user."
  (mantislib-make-assoc-list (mantislib-call "getSavedFilters") 'id 'name))

(defun mantislib-get-server-info ()
  "Return the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (mantislib-call "getServerInfo"))

(defun mantislib-get-sub-task-issue-types ()
  "Return all visible subtask issue types in the system."
  (mantislib-call "getSubTaskIssueTypes"))

(defun mantislib-get-user (username)
  "Return a user's information given their USERNAME."
  (mantislib-call "getUser" username))

(defun mantislib-get-versions (project-key)
  "Return all versions available in project PROJECT-KEY."
  (mantislib-call "getVersions" project-key))

(defun mantislib-strip-cr (string)
  "Remove carriage returns from STRING."
  (when string (replace-regexp-in-string "\r" "" string)))

(provide 'mantislib)
;;; mantislib.el ends here
