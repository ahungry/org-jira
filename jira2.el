;;; jira2.el -- Provide connectivity to JIRA SOAP service

;; Copyright (C) 2011 Bao Haojun
;; original Copyright (C) 2009 Alex Harsanyi

;; Also, used some code from jira.el, which use xml-rpc instead of soap.
;; Thus Copyright (C) for jira.el related code:
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>

;; Authors: 
;; Bao Haojun <baohaojun@gmail.com>
;; Alex Harsanyi <AlexHarsanyi@gmail.com>

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
;; Keywords: soap, web-services, jira
;; Homepage: http://code.google.com/p/emacs-soap-client

;; This file provides a programatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; Jira References:
;;
;; http://confluence.atlassian.com/display/JIRA/Creating+a+SOAP+Client
;;
;; JavaDoc for the Jira SOAP service
;; http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html

(require 'cl)
(require 'soap-client)
(require 'url-parse)

(defgroup jira2 nil
  "Jira2 customization group."
  :group 'applications)

(defgroup jira2-faces nil 
  "Faces for displaying Jira2 information."
  :group 'jira2)

(defcustom jira2-host ""
  "User customizable host name of the Jira2 server, will be used with USERNAME to compute password from .authinfo file.

Will be calculated from jira2-url if not set."
  :group 'jira2
  :type 'string
  :initialize 'custom-initialize-set)

(defface jira2-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jira2-faces)

(defface jira2-issue-info-header-face
  '((t (:bold t :inherit 'jira2-issue-info-face)))
  "Base face for issue headers."
  :group 'jira2-faces)

(defface jira2-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jira2-faces)

(defface jira2-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jira2-faces)

(defface jira2-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jira2-faces)

(defface jira2-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jira2-faces)

(defface jira2-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jira2-faces)

(defface jira2-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jira2-faces)

(defvar jira2-mode-hook nil)

(defvar jira2-mode-map nil)

(defcustom jira2-wsdl-descriptor-url
  ""
  "The location for the WSDL descriptor for the JIRA service.
This is specific to your local JIRA installation.  The URL is
tipically:

  http://YOUR_INSTALLATION/rpc/soap/jirasoapservice-v2?wsdl

The default value works if JIRA is located at a hostname named
'jira'."
  :type 'string
  :group 'jira2)

(defcustom jira2-url
  "http://bible/jira"
  "The address of the jira host."
  :type 'string
  :group 'jira2)

(defvar jira2-token nil
  "JIRA token used for authentication")

(defvar jira2-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `jira2-login'.")

(defvar jira2-wsdl nil)



;;; jira2.el --- Connect to JIRA2 issue tracking software

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

;; This file provides jira2-mode, an emacs major mode for connecting to and 
;; using a Jira2 server. (http://www.atlassian.com/software/jira/). This 
;; jira2-mode is far from complete (more below) but is mostly usable as is 
;; for the features that are present.

;; Note that some functions/processes can be a bit slow. I believe this has 
;; something to do with XMLRPC.

;; Also, XMLRPC access to jira2 is incomplete. Certain Jira2 features cannot be 
;; used via XMLRPC such as (but not limited to):
;; - Changing ticket status
;; - Closing/resolving tickets
;; - Watching a ticket

;; All of the XML-RPC API is wrapped, though not all of the API is exposed
;; via interactive functions. For API details, see:

;; http://confluence.atlassian.com/pages/viewpage.action?pageId=1035
;; http://www.atlassian.com/software/jira/docs/api/rpc-jira2-plugin/latest/com/atlassian/jira/rpc/xmlrpc/XmlRpcService.html

;;; *************
;;; Configuration
;;; *************

;; 1.) Load the file jira2.el, either manuall or place (require 'jira2) in your .emacs with jira2.el in the load path.
;; 2.) Customize the variable jira2-url to point to the XML-RPC url of the Jira2
;; installation to be accessed.
;; 3.) The faces can be customized for different look/feel/highlighting.

;;; *****
;;; Usage
;;; *****

;; M-x jira2-mode will load the major mode into a new buffer named *Jira2*.
;; You will be asked to login; use the username/password for the Jira2 server.
;; A few internal lists should be populated automatically containing a list
;; of projects, issue types, etc. 

;; The following commands/keyboard shorcuts can be used:

;; li - jira2-list-issues
;; lp - jira2-list-projects
;; lf - jira2-list-filters
;; si - jira2-search-issues
;; sp - jira2-search-project-issues
;; i - jira2-show-issue
;; c - jira2-create-ticket
;; o - jira2-comment-ticket
;; r - jira2-refresh-ticket
;; a - jira2-assign-ticket
;; n - jira2-next-comment
;; p - jira2-previous-comment
;; jl - jira2-login
;; jL - jira2-logout
;; Q - jira2-mode-quit

;; When viewing an issues, pressing o, r, etc. acts upon that issue. 
;; For instance, while viewing an issue, pressing o will ask for a comment. 
;; That comment will be posted to the issue currently being viewed.

;; Some prompts have tab completions in the minibuffer/echo area. Try it out to
;; see which prompts do and which do not.

;;; Code:


;; **************************
;; Jira2 Mode - by Brian Zwahr
;; **************************

(defun jira2-load-wsdl ()
  "Load the JIRA WSDL descriptor."
  (setq jira2-wsdl (soap-load-wsdl-from-url (if (string-equal jira2-wsdl-descriptor-url "")
						(concat jira2-url "/rpc/soap/jirasoapservice-v2?wsdl")
					      jira2-wsdl-descriptor-url))))

(defun jira2-login (username password)
  "Login into JIRA and store the authentication token in `jira2-token'"
  ;; NOTE that we cannot rely on `jira2-call' because `jira2-call' relies on
  ;; us ;-)
  (interactive (let ((found (nth 0 (auth-source-search :max 1
                                           :host (if (string= jira2-host "")
						     (url-host (url-generic-parse-url jira2-url))
						   jira2-host)
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
  (unless jira2-wsdl 
    (jira2-load-wsdl))
  (setq jira2-token 
        (car (soap-invoke jira2-wsdl "jirasoapservice-v2" "login" username password)))
  (setq jira2-user-login-name username)

  ;; At this poing, soap-invoke didn't raise an error, so the login
  ;; credentials are OK.  use them to log into the web interface as
  ;; well, as this will be used to link issues (an operation which is
  ;; not exposed to the SOAP interface.  
  ;;
  ;; Note that we don't validate the response at all -- not sure how we
  ;; would do it...
  
  (let ((url (concat jira2-url "/secure/Dashboard.jspa?"
                     (format "&os_username=%s&os_password=%s&os_cookie=true" 
                             username password))))
    (let ((url-request-method "POST")
          (url-package-name "Emacs jira2.el")
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
              (error "Error logging into JIRA Web interface %s" 
                     url-http-response-status)))
        (kill-buffer buffer)))))

(defun jira2-call (method &rest params)
  (car (apply 'jira2-call-it method params)))

(defun jira2-call-it (method &rest params)
  "Invoke the JIRA METHOD with supplied PARAMS.
This should be used for all JIRA inteface calls, as the method
ensures the user is logged in and invokes `soap-invoke' with the
correct service name and authentication token.

All JIRA inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter. For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `jira2-call', the call shoulbe be:

  (jira2-call \"getIssue\" KEY)
"
  (when (symbolp method)
    (setq method (symbol-name method)))
  (unless jira2-token
    (call-interactively 'jira2-login))
  (condition-case data
      (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
             method jira2-token params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq jira2-token nil)
     (call-interactively 'jira2-login)
     (apply 'soap-invoke jira2-wsdl "jirasoapservice-v2" 
            method jira2-token params))))


;;;; Some utility functions
(defun jira2-make-list (data field)
  (loop for element in data
	collect (cdr (assoc field element))))
(defun jira2-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (loop for element in data
     collect (cons (cdr (assoc key-field element))
		   (cdr (assoc value-field element)))))

(defun jira2-make-remote-field-values (fields)
  "Transform a (KEY . VALUE) list into a RemoteFieldValue structure.

Each (KEY . VALUE) pair is transformed into 
 ((id . KEY) (values . (VALUE)))

This method exists because Several JIRA methods require a
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

;;;; Wrappers around JIRA methods

(defun jira2-update-issue (key fields)
  (jira2-call "updateIssue" key (jira2-make-remote-field-values fields)))


(defvar jira2-status-codes-cache nil)

(defun jira2-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-status-codes-cache
    (setq jira2-status-codes-cache
	  (jira2-make-assoc-list (jira2-call "getStatuses") 'id 'name)))
  jira2-status-codes-cache)

(defvar jira2-issue-types-cache nil)

(defun jira2-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-issue-types-cache
    (setq jira2-issue-types-cache
	  (jira2-make-assoc-list (jira2-call "getIssueTypes") 'id 'name)))
  jira2-issue-types-cache)

(defvar jira2-priority-codes-cache nil)

(defun jira2-get-prioritys ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-priority-codes-cache
    (setq jira2-priority-codes-cache
	  (jira2-make-assoc-list (jira2-call "getPriorities") 'id 'name)))
  jira2-priority-codes-cache)

(defvar jira2-resolution-code-cache nil)

(defun jira2-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jira2-resolution-code-cache
    (setq jira2-resolution-code-cache
	  (jira2-make-assoc-list (jira2-call "getResolutions") 'id 'name)))
  jira2-resolution-code-cache)

(defvar jira2-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun jira2-get-issue-regexp ()
  "Return a regexp that matches an issue name.
The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless jira2-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (jira2-call 'getProjectsNoSchemes))))
      (setq jira2-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  jira2-issue-regexp)

(defun jira2-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query." 
  (unless (or limit (numberp limit))
    (setq limit 100))
  (jira2-call "getIssuesFromJqlSearch" jql limit))

(defun jira2-get-available-actions (issue-key)
  "Return the available workflow actions for ISSUE-KEY.
This runs the getAvailableActions SOAP method."
  (jira2-make-assoc-list 
   (jira2-call "getAvailableActions" issue-key)
   'id 'name))

(defun jira2-get-fields-for-action (issue-key action-id)
  "Return the required fields for the ACTION-ID."
  (jira2-make-assoc-list
   (jira2-call "getFieldsForAction" issue-key action-id)
   'id 'name))

(defun jira2-progress-workflow-action (issue-key action-id params)
  (jira2-call "progressWorkflowAction" issue-key action-id (jira2-make-remote-field-values params)))

(defun jira2-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begings at START-DATE and has a TIME-SPENT
duration. JIRA will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z 

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks."
  (jira2-call "addWorklogAndAutoAdjustRemainingEstimate"
                   issue-key
                   `((startDate . ,start-date)
                     (timeSpent . ,time-spent)
                     (comment   . ,comment))))

(defun jira2-link-issue (issue-key link-type other-issue-key)
  "Create a link between ISSUE-KEY and OTHER-ISSUE-KEY.
LINK-TYPE is a string representing the type of the link, e.g
\"requires\", \"depends on\", etc.  I believe each JIRA
installation can define its own link types."
  
  ;; IMPLEMENTATION NOTES: The linking jira issues functionality is
  ;; not exposed through the SOAP api, we must use the web interface
  ;; to do the linking.  Unfortunately, we cannot parse the result, so
  ;; we don't know that the linking was succesfull or not.  To reduce
  ;; the risk, we use the SOAP api to retrieve the issues for
  ;; ISSUE-KEY and OTHER-ISSUE-KEY.  This will ensure that we are
  ;; logged in (see also jira2-login) and that both issues exist. We
  ;; don't validate the LINK-TYPE, not sure how to do it.
  ;;

  (let ((issue (jira2-get-issue issue-key))
        (other-issue (jira2-get-issue other-issue-key)))
    (let ((url (concat jira2-url 
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
            ;; see http://confluence.atlassian.com/display/JIRA/Form+Token+Handling
            (url-request-extra-headers '(("X-Atlassian-Token" . "no-check"))))

       (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
         ;; correctly.  No error does not indicate a success as we
         ;; have to parse the HTML page to find that out...
         (with-current-buffer buffer
           (declare (special url-http-response-status))
           (if (> url-http-response-status 299)
               (error "Error linking issue through JIRA Web interface %s" 
                      url-http-response-status)))
           (kill-buffer buffer))))))


;................................................ issue field accessors ....

(defun jira2-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun jira2-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun jira2-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)"
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (jira2-get-statuses)))))

(defun jira2-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))
  


(defvar jira2-current-issue nil
  "This holds the currently selected issue.")

(defvar jira2-projects-list nil
  "This holds a list of projects and their details.")

(defvar jira2-types nil
  "This holds a list of issues types.")

(defvar jira2-priorities nil
  "This holds a list of priorities.")

(defvar jira2-user-fullnames nil
  "This holds a list of user fullnames.")

(defun jira2-get-project-name (key)
  (let ((projects jira2-projects-list)
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc 'key project)) key)
          (setf name (cdr (assoc 'name project)))))
    name))

(defun jira2-get-type-name (id)
  (let ((types jira2-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc 'id type)) id)
          (setf name (cdr (assoc 'name type)))))
    name))

(defun jira2-get-user-fullname (username)
  (if (assoc username jira2-user-fullnames)
      (cdr (assoc username jira2-user-fullnames))
    (progn
      (let ((user (jira2-get-user username)))
        (setf jira2-user-fullnames (append jira2-user-fullnames (list (cons username (cdr (assoc 'fullname user))))))
        (cdr (assoc 'fullname user))))))



(defun jira2-get-filter (filter-id)
  "Returns a filter given its filter ID."
  (flet ((id-match (filter)
                   (equal filter-id (cdr (assoc 'id filter)))))
    (find-if 'id-match (jira2-get-saved-filters))))

(defun jira2-get-filter-alist ()
  "Returns an association list mapping filter names to IDs"
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (jira2-get-saved-filters)))

(defun jira2-add-comment (issue-key comment)
  "Adds a comment to an issue"
  (jira2-call 'addComment issue-key `((body . ,comment))))

(defun jira2-edit-comment (comment-id comment)
  "Edit the comment body for comment-id"
  (jira2-call 'editComment `((id . ,comment-id)
				      (body . ,comment))))

(defun jira2-create-issue (r-issue-struct)
  "Creates an issue in JIRA2 from a Hashtable object."
  (jira2-call 'createIssue r-issue-struct))

(defun jira2-get-comments (issue-key)
  "Returns all comments associated with the issue"
  (jira2-call 'getComments issue-key))

(defun jira2-get-components (project-key)
  "Returns all components available in the specified project"
  (jira2-make-assoc-list (jira2-call 'getComponents project-key) 'id 'name))

(defun jira2-get-issue (issue-key)
  "Gets an issue from a given issue key."
  (jira2-call 'getIssue issue-key))

(defun jira2-get-issues-from-filter (filter-id)
  "Executes a saved filter"
  (jira2-call 'getIssuesFromFilter filter-id))

(defun jira2-get-issues-from-text-search (search-terms)
  "Find issues using a free text search"
  (jira2-call 'getIssuesFromTextSearch search-terms))

(defun jira2-get-issues-from-text-search-with-project
  (project-keys search-terms max-num-results)
  "Find issues using a free text search, limited to certain projects"
  (jira2-call 'getIssuesFromTextSearchWithProject
             (apply 'vector project-keys) search-terms max-num-results))

;; Modified by Brian Zwahr to use getProjectsNoSchemes instead of getProjects
(defun jira2-get-projects ()
  "Returns a list of projects available to the user"
  (if jira2-projects-list
      jira2-projects-list
    (setq jira2-projects-list (jira2-call "getProjectsNoSchemes"))))

(defun jira2-get-saved-filters ()
  "Gets all saved filters available for the currently logged in user"
  (jira2-call 'getSavedFilters))

(defun jira2-get-server-info ()
  "Returns the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jira2-call 'getServerInfo))

(defun jira2-get-sub-task-issue-types ()
  "Returns all visible subtask issue types in the system"
  (jira2-call 'getSubTaskIssueTypes))

(defun jira2-get-user (username)
  "Returns a user's information given a username"
  (jira2-call 'getUser username))

(defun jira2-get-versions (project-key)
  "Returns all versions available in the specified project"
  (jira2-call 'getVersions project-key))

(defun jira2-strip-cr (string)
  "Removes carriage returns from a string"
  (when string (replace-regexp-in-string "\r" "" string)))

;; Modified by Brian Zwahr to a specific *Jira2* buffer, not a temp buffer
(defmacro jira2-with-jira2-buffer (&rest body)
  "Sends all output and buffer modifications to *Jira2* buffer."
  `(with-current-buffer "*Jira2*" 
     (delete-region (point-min) (point-max))
     (setq truncate-lines t)
     ,@body
     (beginning-of-buffer)))


(provide 'jira2)
