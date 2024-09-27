;;; jiralib.el -- Provide connectivity to JIRA SOAP/REST services.

;; Copyright (C) 2016-2022 Matthew Carter <m@ahungry.com>
;; Copyright (C) 2011 Bao Haojun
;; original Copyright (C) 2009 Alex Harsanyi

;; Also, used some code from jira.el, which use xml-rpc instead of soap.
;; Thus Copyright (C) for jira.el related code:
;; Brian Zwahr <echosa@gmail.com>
;; Dave Benjamin <dave@ramenlabs.com>

;; Authors:
;; Matthew Carter <m@ahungry.com>
;; Bao Haojun <baohaojun@gmail.com>
;; Alex Harsanyi <AlexHarsanyi@gmail.com>

;; Maintainer: Matthew Carter <m@ahungry.com>
;; Version: 3.0.0
;; Homepage: https://github.com/ahungry/org-jira

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

;; Author: Alexandru Harsanyi (AlexHarsanyi@gmail.com)
;; Created: December, 2009
;; Keywords: soap, web-services, jira
;; Homepage: http://code.google.com/p/emacs-soap-client

;;; Commentary:

;; This file provides a programmatic interface to JIRA.  It provides access to
;; JIRA from other programs, but no user level functionality.

;; Jira References:

;; Primary reference (on current Jira, only REST is supported):
;; https://developer.atlassian.com/jiradev/jira-apis/jira-rest-apis

;; Full API list reference:
;; https://docs.atlassian.com/jira/REST/cloud/

;; Legacy reference (unsupported and deprecated/unavailable):
;; http://confluence.atlassian.com/display/JIRA/Creating+a+SOAP+Client

;; JavaDoc for the Jira SOAP service
;; http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html

;;; News:

;;;; Changes since 2.6.3:
;; - Add worklog import filter and control variable for external worklogs.
;; - Add the worklog related endpoint/calls.

;;;; Changes since 2.1.0:
;; - Remove os_username / os_password manual http request as part of sign in process
;;     This produces sysadmin level warnings on Jira when these are used under the latest Jira.
;; - Remove unused function jiralib-link-issue
;; - Bring version up to match org-jira version so they can share tag

;;;; Changes since 2.0.0:
;; - Allow issue type query by project

;;;; Changes since 0.0.0:
;; - Converted many calls to async
;; - Converted many calls to make use of caching

;;; Code:

(eval-when-compile (require 'cl))
(require 'cl-seq)
(require 'soap-client)
(require 'request)
(require 'json)
(require 'url-parse)
(require 'url-util)

(defconst jiralib-version "3.0.0"
  "Current version of jiralib.el.")

(defgroup jiralib nil
  "Jiralib customization group."
  :group 'applications)

(defgroup jiralib-faces nil
  "Faces for displaying Jiralib information."
  :group 'jiralib)

(defcustom jiralib-use-restapi t
  "Use restapi instead of soap."
  :group 'jiralib
  :type 'boolean
  :initialize 'custom-initialize-set)

(defcustom jiralib-coding-system 'utf-8
  "Use custom coding system for Jiralib."
  :group 'jiralib)

(defcustom jiralib-host ""
  "User customizable host name of the Jiralib server.

This will be used with USERNAME to compute password from
.authinfo file.  Will be calculated from jiralib-url if not set."
  :group 'jiralib
  :type 'string
  :initialize 'custom-initialize-set)

(defcustom jiralib-user ""
  "User customizable user name of the Jiralib server.

This will be used with USERNAME to compute password from
.authinfo file.  Will be calculated from jiralib-url if not set."
  :group 'jiralib
  :type 'string
  :initialize 'custom-initialize-set)

(defface jiralib-issue-info-face
  '((t (:foreground "black" :background "yellow4")))
  "Base face for issue information."
  :group 'jiralib-faces)

(defface jiralib-issue-info-header-face
  '((t (:bold t :inherit 'jiralib-issue-info-face)))
  "Base face for issue headers."
  :group 'jiralib-faces)

(defface jiralib-issue-summary-face
  '((t (:bold t)))
  "Base face for issue summary."
  :group 'jiralib-faces)

(defface jiralib-comment-face
  '((t (:background "gray23")))
  "Base face for comments."
  :group 'jiralib-faces)

(defface jiralib-comment-header-face
  '((t (:bold t)))
  "Base face for comment headers."
  :group 'jiralib-faces)

(defface jiralib-link-issue-face
  '((t (:underline t)))
  "Face for linked issues."
  :group 'jiralib-faces)

(defface jiralib-link-project-face
  '((t (:underline t)))
  "Face for linked projects"
  :group 'jiralib-faces)

(defface jiralib-link-filter-face
  '((t (:underline t)))
  "Face for linked filters"
  :group 'jiralib-faces)

(defvar jiralib-mode-hook nil)
(defvar jiralib-mode-map nil)
(defvar jiralib-issue-regexp "\\<\\(?:[A-Za-z0-9]+\\)-[0-9]+\\>")

(defcustom jiralib-wsdl-descriptor-url
  ""
  "The location for the WSDL descriptor for the JIRA service.
This is specific to your local JIRA installation.  The URL is
typically:

  http://YOUR_INSTALLATION/rpc/soap/jirasoapservice-v2?wsdl

The default value works if JIRA is located at a hostname named
'jira'."
  :type 'string
  :group 'jiralib)

(defcustom jiralib-url
  "http://localhost:8081/"
  "The address of the jira host."
  :type 'string
  :group 'jiralib)

(defcustom jiralib-agile-page-size
  50
  "Page size for agile API retrieve. Limited by server property jira.search.views.default.max"
  :type 'integer
  :group 'jiralib)

(defvar jiralib-token nil
  "JIRA token used for authentication.")

(defvar jiralib-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `jiralib-login'.")

(defvar jiralib-wsdl nil)

(defcustom jiralib-worklog-import--filters-alist
  (list
   '(nil "WorklogUpdatedByCurrentUser"
         (lambda (wl)
           (let-alist wl
             (when
                 (and wl
                      (string-equal
                       (downcase
                        (or jiralib-user-login-name user-login-name ""))
                       (downcase (or .updateAuthor.name
                                     (car (split-string (or .updateAuthor.emailAddress "") "@"))
                                     ""))))
               wl))))
   '(nil "WorklogAuthoredByCurrentUser"
         (lambda (wl)
           (let-alist wl
             (when
                 (and wl
                      (string-equal
                       (downcase
                        (or jiralib-user-login-name user-login-name))
                       (downcase (or .author.name
                                     (car (split-string (or .author.emailAddress "") "@"))))))
               wl)))))
  "A list of triplets: ('Global-Enable 'Descriptive-Label 'Function-Definition)
that apply worklog predicate filters during import.

Example: (list '('t \"descriptive-predicate-label\" (lambda (x) x)))"
  :type '(repeat (list boolean string function))
  :group 'org-jira)


(defcustom jiralib-update-issue-fields-exclude-list nil
  "A list of symbols to check for exclusion on updates based on matching key.
Key names should be one of components, description, assignee, reporter, summary, issuetype."
  :type '(set (const :tag "Exclude components" components)
              (const :tag "Exclude description" description)
              (const :tag "Exclude assignee" assignee)
              (const :tag "Exclude reporter" reporter)
              (const :tag "Exclude summary" summary)
              (const :tag "Exclude priority" priority)
              (const :tag "Exclude issue type" issuetype))
  :group 'org-jira)

(defun jiralib-load-wsdl ()
  "Load the JIRA WSDL descriptor."
  (setq jiralib-wsdl (soap-load-wsdl-from-url (if (string-equal jiralib-wsdl-descriptor-url "")
                                                  (concat jiralib-url "/rpc/soap/jirasoapservice-v2?wsdl")
                                                jiralib-wsdl-descriptor-url))))

(defun jiralib-login (username password)
  "Login into JIRA as user USERNAME with PASSWORD.

After a successful login, store the authentication token in
`jiralib-token'."
  ;; NOTE that we cannot rely on `jiralib-call' because `jiralib-call' relies on
  ;; us ;-)
  (interactive
   (if (> 24 emacs-major-version)
       (let ((user (read-string "Username for Jira server login? "))
             (password (read-passwd "Password for Jira server login? ")))
         (list user password))
     (let ((found (nth 0 (auth-source-search :max 1
                                             :host (if (string= jiralib-host "")
                                                       (url-host (url-generic-parse-url jiralib-url))
                                                     jiralib-host)
                                             :user (if (string= jiralib-user "")
                                                       (url-user (url-generic-parse-url jiralib-url))
                                                     jiralib-user)
                                             ;; secrets.el wouldn’t accept a number.
                                             :port (list (number-to-string (url-port (url-generic-parse-url jiralib-url)))
                                                         (url-type (url-generic-parse-url jiralib-url)))
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
  (if jiralib-use-restapi
      (setq jiralib-token `("Authorization" . , (format "Basic %s" (base64-encode-string (concat username ":" password) t))))
    (unless jiralib-wsdl
      (jiralib-load-wsdl))
    (setq jiralib-token
          (car (soap-invoke jiralib-wsdl "jirasoapservice-v2" "login" username password))))
    (setq jiralib-user-login-name username))

(defvar jiralib-complete-callback nil)

(defun jiralib-call (method callback &rest params)
  "Invoke the Jira METHOD, then CALLBACK with supplied PARAMS.

This function should be used for all JIRA interface calls, as the
method ensures the user is logged in and invokes `soap-invoke'
with the correct service name and authentication token.

All JIRA interface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter.  For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `jiralib-call', the call should be:

  (jiralib-call \"getIssue\" KEY)

CALLBACK should be the post processing function to run with the
completed data from the request result, which can be accessed with:

  (cl-getf data :data)

as such, the CALLBACK should follow this type of form:

  (cl-function
    (lambda (&rest data &allow-other-keys)
      (print (cl-getf data :data))))

If CALLBACK is set to nil then the request will occur with sync.
This produces a noticeable slowdown and is not recommended by
request.el, so if at all possible, it should be avoided."
  ;; @TODO :auth: Probably pass this all the way down, but I think
  ;; it may be OK at the moment to just set the variable each time.
  
  (setq jiralib-complete-callback
        ;; Don't run with async if we don't have a login token yet.
        (if jiralib-token callback nil))

  ;; If we don't have a regex set, ensure it is set BEFORE any async
  ;; calls are processing, or we're going to have a bad time.
  ;; This should only end up running once per session.
  (unless jiralib-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (append (jiralib--rest-call-it
                                     "/rest/api/2/project"
                                     :params '((expand . "description,lead,url,projectKeys")))
                                    nil)
                            )))
      (when projects
        (setq jiralib-issue-regexp
              (concat "\\<" (regexp-opt projects) "-[0-9]+\\>")))))

  (if (not jiralib-use-restapi)
      (car (apply 'jiralib--call-it method params))
    (unless jiralib-token
      (call-interactively 'jiralib-login))
    (cl-case (intern method)
      ('getStatuses (jiralib--rest-call-it "/rest/api/2/status"))
      ('getIssueTypes (jiralib--rest-call-it "/rest/api/2/issuetype"))
      ('getSubTaskIssueTypes (jiralib--rest-call-it "/rest/api/2/issuetype"))
      ('getIssueTypesByProject
       (let ((response (jiralib--rest-call-it (format "/rest/api/2/project/%s" (first params)))))
         (cl-coerce (cdr (assoc 'issueTypes response)) 'list)))
      ('getUser (jiralib--rest-call-it "/rest/api/2/user" :params `((accountId . ,(first params)))))
      ('getVersions (jiralib--rest-call-it (format "/rest/api/2/project/%s/versions" (first params))))

      ;; Worklog calls
      ('getWorklogs
       (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))))

      ('addWorklog
       (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))
                              :type "POST"
                              :data (json-encode (second params))))

      ('updateWorklog
       (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog/%s" (first params) (second params))
                              :type "PUT"
                              :data (json-encode (third params))))

      ('addWorklogAndAutoAdjustRemainingEstimate
       (jiralib--rest-call-it (format "/rest/api/2/issue/%s/worklog" (first params))
                              :type "POST"
                              :data (json-encode (second params))))

      ('addComment (jiralib--rest-call-it
                    (format "/rest/api/2/issue/%s/comment" (first params))
                    :type "POST"
                    :data (json-encode (second params))))
      ('createIssue
       ;; Creating the issue doesn't return it, a second call must be
       ;; made to pull it in by using the self key in response.
       (let ((response (jiralib--rest-call-it
                        "/rest/api/2/issue"
                        :type "POST"
                        :data (json-encode (first params)))))
         (jiralib--rest-call-it (cdr (assoc 'self response)) :type "GET")
         ))
      ('createIssueWithParent
       (let ((response (jiralib--rest-call-it
                        "/rest/api/2/issue"
                        :type "POST"
                        :data (json-encode (first params)))))
         (jiralib--rest-call-it (cdr (assoc 'self response)) :type "GET")
         ))
      ('editComment (jiralib--rest-call-it
                     (format "/rest/api/2/issue/%s/comment/%s" (first params) (second params))
                     :data (json-encode `((body . ,(third params))))
                     :type "PUT"))
      ('getBoard  (jiralib--rest-call-it (format "/rest/agile/1.0/board/%s"  (first params))))
      ('getBoards (apply 'jiralib--agile-call-it "/rest/agile/1.0/board" 'values params))
      ('getComment (org-jira-find-value
                     (jiralib--rest-call-it
                      (format "/rest/api/2/issue/%s/comment/%s" (first params) (second params)))
                     'comments))
      ('getComments (org-jira-find-value
                     (jiralib--rest-call-it
                      (format "/rest/api/2/issue/%s/comment" (first params)))
                     'comments))
      ('getAttachmentsFromIssue (org-jira-find-value
                                 (jiralib--rest-call-it
                                  (format "/rest/api/2/issue/%s?fields=attachment" (first params)))
                                 'comments))
      ('getComponents (jiralib--rest-call-it
                       (format "/rest/api/2/project/%s/components" (first params))))
      ('getIssue (jiralib--rest-call-it
                  (format "/rest/api/2/issue/%s" (first params))))
      ('getIssuesFromBoard  (apply 'jiralib--agile-call-it
				   (format "rest/agile/1.0/board/%d/issue" (first params))
				   'issues
				   (cdr params)))
      ('getSprintsFromBoard  (jiralib--rest-call-it (format "/rest/agile/1.0/board/%s/sprint"  (first params))))
      ('getIssuesFromSprint  (apply 'jiralib--agile-call-it
				   (format "rest/agile/1.0/sprint/%d/issue" (first params))
				   'issues
				   (cdr params)))
      ('getIssuesFromJqlSearch  (append (cdr ( assoc 'issues (jiralib--rest-call-it
                                                              "/rest/api/2/search"
                                                              :type "POST"
                                                              :data (json-encode `((jql . ,(first params))
                                                                                   (maxResults . ,(second params)))))))
                                        nil))
      ('getPriorities (jiralib--rest-call-it
                       "/rest/api/2/priority"))
      ('getProjects (jiralib--rest-call-it "rest/api/2/project"))
      ('getProjectsNoSchemes (append (jiralib--rest-call-it
                                      "/rest/api/2/project"
                                      :params '((expand . "description,lead,url,projectKeys"))) nil))
      ('getResolutions (append (jiralib--rest-call-it
                                "/rest/api/2/resolution") nil))
      ('getAvailableActions
       (mapcar
        (lambda (trans)
          `(,(assoc 'name trans) ,(assoc 'id trans)))
        (cdadr (jiralib--rest-call-it (format "/rest/api/2/issue/%s/transitions" (first params))))))
      ('getFieldsForAction (org-jira-find-value (car (let ((issue (first params))
                                                           (action (second params)))
                                                       (seq-filter (lambda (trans)
                                                                     (or (string-equal action (org-jira-find-value trans 'id))
                                                                         (string-equal action (org-jira-find-value trans 'name))))
                                                                   (cdadr (jiralib--rest-call-it
                                                                           (format "/rest/api/2/issue/%s/transitions" (first params))
                                                                           :params '((expand . "transitions.fields")))))))
                                                'fields))
      ('progressWorkflowAction (jiralib--rest-call-it
                                (format "/rest/api/2/issue/%s/transitions" (first params))
                                :type "POST"
                                :data (json-encode `(,(car (second params)) ,(car (third params))))))
      ('getUsers
       (jiralib--rest-call-it (format "/rest/api/2/user/assignable/search?project=%s&maxResults=10000" (first params))
                              :type "GET"))
      ('updateIssue (jiralib--rest-call-it
                     (format "/rest/api/2/issue/%s" (first params))
                     :type "PUT"
                     :data (json-encode `((fields . ,(second params))))))
      ('getLabels (jiralib--rest-call-it (format "/rest/api/2/label?startAt=%s" (first params)))))))

(defun jiralib--soap-call-it (&rest args)
  "Deprecated SOAP call endpoint.  Will be removed soon.
Pass ARGS to jiralib-call."
  (let ((jiralib-token nil)
        (jiralib-use-restapi nil))
    (apply #'jiralib-call args)))

(defun jiralib--json-read ()
  "Read with json, force utf-8"
  (decode-coding-region (point) (point-max) jiralib-coding-system)
  (json-read))

(defun jiralib--rest-call-it (api &rest args)
  "Invoke the corresponding jira rest method API.
Invoking COMPLETE-CALLBACK when the
JIRALIB-COMPLETE-CALLBACK is non-nil, request finishes, and
passing ARGS to REQUEST."
  (unless api (error "jiralib--rest-call-it was called with a NIL api value."))
  (setq args
        (mapcar
         (lambda (arg)
           (if (stringp arg)
               (encode-coding-string arg jiralib-coding-system)
             arg))
         args))
  (append (request-response-data
           (apply #'request (if (string-match "^http[s]*://" api) api ;; If an absolute path, use it
                              (concat (replace-regexp-in-string "/*$" "/" jiralib-url)
                                      (replace-regexp-in-string "^/*" "" api)))
                  :sync (not jiralib-complete-callback)
                  :headers `(,jiralib-token ("Content-Type" . "application/json"))
                  :parser 'jiralib--json-read
                  :complete jiralib-complete-callback
                  ;; Ensure we have useful errors
                  :error
                  (lexical-let
                      ((my-api api)
                       (my-args args))
                    (cl-function
                       (lambda (&key data &allow-other-keys)
                         (print "JIRA_ERROR - see your *Messages* buffer for more details.")
                         (print "JIRA_ERROR REQUEST: ")
                         (print my-api)
                         (print my-args)
                         (print "JIRA_ERROR RESPONSE: ")
                         (print data)
                         (error "JIRA_ERROR - see your *Messages* buffer for more details.")
                         )))
                  args))
          nil))

(defun jiralib--call-it (method &rest params)
  "Invoke the JIRA METHOD with supplied PARAMS.

Internal use, returns a list of responses, of which only the
first is normally used."
  (when (symbolp method)
    (setq method (symbol-name method)))
  (unless jiralib-token
    (call-interactively 'jiralib-login))
  (condition-case data
      (apply 'soap-invoke jiralib-wsdl "jirasoapservice-v2"
             method jiralib-token params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq jiralib-token nil)
     (call-interactively 'jiralib-login)
     (apply 'soap-invoke jiralib-wsdl "jirasoapservice-v2"
            method jiralib-token params))))


;;;; Some utility functions

(defun jiralib-make-list (data field)
  "Map all assoc elements in DATA to the value of FIELD in that element."
  (cl-loop for element in data
        collect (cdr (assoc field element))))

(defun jiralib-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"  
  (cl-loop for element in data
        collect (cons (cdr (assoc key-field element))
                      (cdr (assoc value-field element)))))

(defun jiralib-make-remote-field-values (fields)
  "Transform the (KEY . VALUE) list FIELDS into a RemoteFieldValue structure.

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

(defun jiralib--rest-api-for-issue-key (key)
  "Return jira rest api for issue KEY."
  (concat "rest/api/2/issue/" key))

(defun jiralib-filter-fields-by-exclude-list (exclude-list fields)
  (cl-remove-if
   (lambda (el) (cl-member (car el) exclude-list)) fields))

(defun jiralib-update-issue (key fields &optional callback)
  "Update the issue with id KEY with the values in FIELDS, invoking CALLBACK."
  (let ((filtered-fields (jiralib-filter-fields-by-exclude-list
                          jiralib-update-issue-fields-exclude-list
                          fields)))
    (jiralib-call
     "updateIssue"
     callback
     key (if jiralib-use-restapi
             filtered-fields
           (jiralib-make-remote-field-values filtered-fields)))))

(defvar jiralib-status-codes-cache nil)

(defun jiralib-get-statuses ()
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, then
will cache it."
  (unless jiralib-status-codes-cache
    (setq jiralib-status-codes-cache
          (jiralib-make-assoc-list (jiralib-call "getStatuses" nil) 'id 'name)))
  jiralib-status-codes-cache)

(defvar jiralib-issue-types-cache nil)

(defun jiralib-get-issue-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it.

The issue types returned via getIssueTypes are all the ones
available to the user, but not necessarily available to the given
project.

This endpoint is essentially a master reference for when issue
types need a name lookup when given an id.

For applying issue types to a given project that is being created, see
the #'jiralib-get-issue-types-by-project call."
  (unless jiralib-issue-types-cache
    (setq jiralib-issue-types-cache
          (jiralib-make-assoc-list (jiralib-call "getIssueTypes" nil) 'id 'name)))
  jiralib-issue-types-cache)

(defvar jiralib-issue-types-by-project-cache nil "An alist of available issue types.")

(defun jiralib-get-issue-types-by-project (project)
  "Return the available issue types for PROJECT.

PROJECT should be the key, such as `EX' or `DEMO'."
  (unless (assoc project jiralib-issue-types-by-project-cache)
    (push (cons project
                (jiralib-make-assoc-list
                 (jiralib-call "getIssueTypesByProject" nil project)
                 'id 'name))
          jiralib-issue-types-by-project-cache))
  (cdr (assoc project jiralib-issue-types-by-project-cache)))

(defvar jiralib-priority-codes-cache nil)

(defun jiralib-get-priorities ()
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-priority-codes-cache
    (setq jiralib-priority-codes-cache
          (jiralib-make-assoc-list (jiralib-call "getPriorities" nil) 'id 'name)))
  jiralib-priority-codes-cache)

(defvar jiralib-resolution-code-cache nil)

(defun jiralib-get-resolutions ()
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-resolution-code-cache
    (setq jiralib-resolution-code-cache
          (jiralib-make-assoc-list (jiralib-call "getResolutions" nil) 'id 'name)))
  jiralib-resolution-code-cache)

;; NOTE: it is not such a good idea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.
;;
;; FIXME: Probably just deprecate/remove this, we can assert we're on
;; an issue with a general regexp that matches the common format, vs
;; needing to know specific user project list.
(defun jiralib-get-issue-regexp ()
  "Return a regexp that will match an issue id.

The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless jiralib-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (jiralib-call "getProjectsNoSchemes" nil))))
      (when projects
        (setq jiralib-issue-regexp
              (concat "\\<" (regexp-opt projects) "-[0-9]+\\>")))))
  jiralib-issue-regexp)

(defun jiralib-do-jql-search (jql &optional limit callback)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query."
  (unless (or limit (numberp limit))
    (setq limit 100))
  (jiralib-call "getIssuesFromJqlSearch" callback jql limit))

(defcustom jiralib-available-actions-cache-p t
  "Set to t to enable caching for jiralib-get-available-actions.

If nil, will disable caching for this endpoint.

Possible side-effects:

  - If the server has the project workflow updated, the cache
saved here will be incorrect.

  - If the issue is not up to date with the remote, the wrong
cache key may be queried."
  :type 'boolean
  :group 'jiralib)

(defvar jiralib-available-actions-cache nil "An alist of available actions.")

(defun jiralib-get-available-actions (issue-key &optional status)
  "Return the available workflow actions for ISSUE-KEY.
This uses STATUS as the cache key.
This runs the getAvailableActions SOAP method."
  (if (and jiralib-available-actions-cache-p status)
      (progn
        (unless (assoc status jiralib-available-actions-cache)
          (push (cons status
                      (jiralib-make-assoc-list
                       (mapcar (lambda (x)
                                 (let ((namestring (cdr (car x)))
                                       (id (cdr x)))
                                   (cons
                                    (cons 'name (org-jira-decode namestring))
                                    id)))
                               (jiralib-call "getAvailableActions" nil issue-key))
                       'id 'name))
                jiralib-available-actions-cache))
        (cdr (assoc status jiralib-available-actions-cache)))
    (progn
      (jiralib-make-assoc-list
       (mapcar (lambda (x)
                 (let ((namestring (cdr (car x)))
                       (id (cdr x)))
                   (cons
                    (cons 'name (org-jira-decode namestring))
                    id)))
               (jiralib-call "getAvailableActions" nil issue-key))
       'id 'name))))

(defcustom jiralib-fields-for-action-cache-p t
  "Set to t to enable caching for jiralib-get-fields-for-action.

If nil, will disable caching for this endpoint.

Possible side-effects:

  - If many tasks have different workflows, you may want to disable this."
  :type 'boolean
  :group 'jiralib)

(defvar jiralib-fields-for-action-cache nil "An alist of available fields.")

(defun jiralib-get-fields-for-action-with-cache (issue-key action-id)
  "Return the required fields to change ISSUE-KEY to ACTION-ID."
  (if (and jiralib-fields-for-action-cache-p action-id)
      (progn
        (unless (assoc action-id jiralib-fields-for-action-cache)
          (push (cons action-id
                      (jiralib-call "getFieldsForAction" nil issue-key action-id))
                jiralib-fields-for-action-cache))
        (cdr (assoc action-id jiralib-fields-for-action-cache)))
    (jiralib-call "getFieldsForAction" nil issue-key action-id)))

(defun jiralib-get-fields-for-action (issue-key action-id)
  "Return the required fields to change ISSUE-KEY to ACTION-ID."
  (if jiralib-use-restapi
      (let ((fields (jiralib-get-fields-for-action-with-cache issue-key action-id)))
        (mapcar (lambda (field)
                  (cons (symbol-name (car field))
                        (format "%s (required: %s)"
                                (org-jira-find-value field 'name)
                                (if (eq (org-jira-find-value field 'required) :json-false)
                                    "nil"
                                  "t"))))
                fields))
    (jiralib-make-assoc-list
     (jiralib-get-fields-for-action-with-cache issue-key action-id)
     'id 'name)))

(defun jiralib-progress-workflow-action (issue-key action-id params &optional callback)
  "Progress issue with ISSUE-KEY to action ACTION-ID, and provide the needed PARAMS.

When CALLBACK is present, this will run async."
  (if jiralib-use-restapi
      (jiralib-call "progressWorkflowAction"
                    callback issue-key `((transition (id . ,action-id)))
                    `((fields . ,params)))
    (jiralib-call "progressWorkflowAction"
                  callback issue-key action-id (jiralib-make-remote-field-values params))))


(defun jiralib-format-datetime (&optional datetime)
  "Convert a mixed DATETIME format into the Jira required datetime format.

This will produce a datetime string such as:

  2010-02-05T14:30:00.000+0000

for being consumed in the Jira API.

If DATETIME is not passed in, it will default to the current time."
  (let* ((defaults (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
         (datetime (concat datetime (subseq defaults (length datetime))))
         (parts (parse-time-string datetime)))
    (format "%04d-%02d-%02dT%02d:%02d:%02d.000+0000"
            (nth 5 parts)
            (nth 4 parts)
            (nth 3 parts)
            (nth 2 parts)
            (nth 1 parts)
            (nth 0 parts))))

(defvar jiralib-worklog-coming-soon-message
  "WORKLOG FEATURES ARE NOT IMPLEMENTED YET, COMING SOON!")

(defun jiralib-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begins at START-DATE and has a TIME-SPENT
duration.  JIRA will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks.

COMMENT will be added to this worklog."
  (let ((formatted-start-date (jiralib-format-datetime start-date)))
    (jiralib-call "addWorklogAndAutoAdjustRemainingEstimate"
                  nil
                  issue-key
                  ;; Expects data such as: '{"timeSpent":"1h", "started":"2017-02-21T00:00:00.000+0000", "comment":"woot!"}'
                  ;; and only that format will work (no loose formatting on the started date)
                  `((started   . ,formatted-start-date)
                    (timeSpent . ,time-spent)
                    (comment   . ,comment)))))


;;;; Issue field accessors

(defun jiralib-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun jiralib-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun jiralib-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)."
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (jiralib-get-statuses)))))

(defun jiralib-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))

(defvar jiralib-current-issue nil
  "This holds the currently selected issue.")

(defvar jiralib-projects-list nil
  "This holds a list of projects and their details.")

(defvar jiralib-types nil
  "This holds a list of issues types.")

(defvar jiralib-priorities nil
  "This holds a list of priorities.")

(defvar jiralib-user-fullnames nil
  "This holds a list of user fullnames.")

(defun jiralib-get-project-name (key)
  "Return the name of the JIRA project with id KEY."
  (let ((projects jiralib-projects-list)
        (name nil))
    (dolist (project projects)
      (if (equal (cdr (assoc 'key project)) key)
          (setf name (cdr (assoc 'name project)))))
    name))

(defun jiralib-get-type-name (id)
  "Return the name of the issue type with ID."
  (let ((types jiralib-types)
        (name nil))
    (dolist (type types)
      (if (equal (cdr (assoc 'id type)) id)
          (setf name (cdr (assoc 'name type)))))
    name))

;; (defun jiralib-get-user-fullname (account-id)
;;   "Return the full name (display name) of the user with USERNAME."
  ;; (if (assoc account-id jiralib-user-fullnames)
  ;;     (cdr (assoc account-id jiralib-user-fullnames))
  ;;   (progn
  ;;     (let ((user (jiralib-get-user account-id)))
  ;;       (setf jiralib-user-fullnames (append jiralib-user-fullnames (list (cons account-id (cdr (assoc 'fullname user))))))
  ;;       (cdr (assoc 'fullname user))))))

(defun jiralib-get-user-fullname (account-id)
  "Return the full name (displayName) of the user with ACCOUNT-ID."
  (cl-loop for user in (jiralib-get-users nil)
        when (rassoc account-id user)
        return (cdr (assoc 'displayName user))))

(defun jiralib-get-user-account-id (project full-name)
    "Return the account-id (accountId) of the user with FULL-NAME (displayName) in PROJECT."
  (cl-loop for user in (jiralib-get-users project)
        when (rassoc full-name user)
        return (cdr (assoc 'accountId user))))

(defun jiralib-get-filter (filter-id)
  "Return a filter given its FILTER-ID."
  (cl-flet ((id-match (filter)
                      (equal filter-id (cdr (assoc 'id filter)))))
    (cl-find-if 'id-match (jiralib-get-saved-filters))))

(defun jiralib-get-filter-alist ()
  "Return an association list mapping filter names to IDs."
  (mapcar (lambda (filter)
            (cons (cdr (assoc 'name filter))
                  (cdr (assoc 'id filter))))
          (jiralib-get-saved-filters)))

(defun jiralib-add-comment (issue-key comment &optional callback)
  "Add to issue with ISSUE-KEY the given COMMENT, invoke CALLBACK."
  (jiralib-call "addComment" callback issue-key `((body . ,comment))))

(defun jiralib-edit-comment (issue-id comment-id comment &optional callback)
  "Edit ISSUE-ID's comment COMMENT-ID to reflect the new COMMENT, invoke CALLBACK."
  (if (not jiralib-use-restapi)
      (jiralib-call "editComment" callback `((id . ,comment-id)
                                             (body . ,comment)))
    (jiralib-call "editComment" callback issue-id comment-id comment)))

(defun jiralib-create-issue (issue)
  "Create a new ISSUE in JIRALIB.

ISSUE is a Hashtable object."
  (jiralib-call "createIssue" nil issue))

(defun jiralib-create-subtask (subtask)
  "Create SUBTASK for issue with PARENT-ISSUE-ID.

SUBTASK is a Hashtable object."
  (jiralib-call "createIssueWithParent" nil subtask))

(defvar jiralib-subtask-types-cache nil)

(defun jiralib-get-subtask-types ()
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless jiralib-subtask-types-cache
    (setq jiralib-subtask-types-cache
          (jiralib-make-assoc-list (jiralib-call "getSubTaskIssueTypes" nil) 'id 'name)))
  jiralib-subtask-types-cache)

(defun jiralib-get-comment (issue-key comment-id &optional callback)
  "Return all comments associated with issue ISSUE-KEY, invoking CALLBACK."
  (jiralib-call "getComment" callback issue-key comment-id))

(defun jiralib-get-comments (issue-key &optional callback)
  "Return all comments associated with issue ISSUE-KEY, invoking CALLBACK."
  (jiralib-call "getComments" callback issue-key))

(defun jiralib-get-attachments (issue-key &optional callback)
  "Return all attachments associated with issue ISSUE-KEY, invoking CALLBACK."
  (jiralib-call "getAttachmentsFromIssue" callback issue-key))

(defun jiralib-get-worklogs (issue-key &optional callback)
  "Return all worklogs associated with issue ISSUE-KEY, invoking CALLBACK."
  (jiralib-call "getWorklogs" callback issue-key))

(defun jiralib-add-worklog (issue-id started time-spent-seconds comment &optional callback)
  "Add the worklog linked to ISSUE-ID.

Requires STARTED (a jira datetime), TIME-SPENT-SECONDS (integer) and a COMMENT.
CALLBACK will be invoked if passed in upon endpoint completion."
  ;; Call will fail if 0 seconds are set as the time, so always do at least one min.
  (setq time-spent-seconds (max 60 time-spent-seconds))
  (let ((worklog `((started . ,started)
                   ;; @TODO :worklog: timeSpentSeconds changes into incorrect values
                   ;; in the Jira API (for instance, 89600 = 1 day, but Jira thinks 3 days...
                   ;; We should convert to a Xd Xh Xm format from our seconds ourselves.
                   (timeSpentSeconds . ,time-spent-seconds)
                   (comment . ,comment))))
    (jiralib-call "addWorklog" callback issue-id worklog)))

(defun jiralib-update-worklog (issue-id worklog-id started time-spent-seconds comment &optional callback)
  "Update the worklog linked to ISSUE-ID and WORKLOG-ID.

Requires STARTED (a jira datetime), TIME-SPENT-SECONDS (integer) and a COMMENT.
CALLBACK will be invoked if passed in upon endpoint completion."
  ;; Call will fail if 0 seconds are set as the time, so always do at least one min.
  (setq time-spent-seconds (max 60 time-spent-seconds))
  (let ((worklog `((started . ,started)
                   ;; @TODO :worklog: timeSpentSeconds changes into incorrect values
                   ;; in the Jira API (for instance, 89600 = 1 day, but Jira thinks 3 days...
                   ;; We should convert to a Xd Xh Xm format from our seconds ourselves.
                   (timeSpentSeconds . ,time-spent-seconds)
                   (comment . ,comment))))
    (jiralib-call "updateWorklog" callback issue-id worklog-id worklog)))

(defvar jiralib-components-cache nil "An alist of project components.")

(defun jiralib-get-components (project-key)
  "Return all components available in the project PROJECT-KEY."
  (unless (assoc project-key jiralib-components-cache)
    (push (cons project-key
                (jiralib-make-assoc-list
                 (jiralib-call "getComponents" nil project-key) 'id 'name))
          jiralib-components-cache))
  (cdr (assoc project-key jiralib-components-cache)))

(defun jiralib-get-issue (issue-key &optional callback)
  "Get the issue with key ISSUE-KEY, running CALLBACK after."
  (jiralib-call "getIssue" callback issue-key))

(defun jiralib-get-issues-from-filter (filter-id)
  "Get the issues from applying saved filter FILTER-ID."
  (message "jiralib-get-issues-from-filter is NOT IMPLEMENTED!!  Do not use!")
  (jiralib-call "getIssuesFromFilter" nil filter-id))

(defun jiralib-get-issues-from-text-search (search-terms)
  "Find issues using free text search SEARCH-TERMS."
  (jiralib-call "getIssuesFromTextSearch" nil search-terms))

(defun jiralib-get-issues-from-text-search-with-project
    (project-keys search-terms max-num-results)
  "Find issues in projects PROJECT-KEYS, using free text search SEARCH-TERMS.

Return no more than MAX-NUM-RESULTS."
  (jiralib-call "getIssuesFromTextSearchWithProject"
                nil
                (apply 'vector project-keys) search-terms max-num-results))

;; Modified by Brian Zwahr to use getProjectsNoSchemes instead of getProjects
(defun jiralib-get-projects ()
  "Return a list of projects available to the user."
  (if jiralib-projects-list
      jiralib-projects-list
    (setq jiralib-projects-list
          (if jiralib-use-restapi
              (jiralib-call "getProjects" nil)
            (jiralib-call "getProjectsNoSchemes" nil)))))

(defun jiralib-get-saved-filters ()
  "Get all saved filters available for the currently logged in user."
  (jiralib-make-assoc-list (jiralib-call "getSavedFilters" nil) 'id 'name))

(defun jiralib-get-server-info ()
  "Return the Server information such as baseUrl, version, edition, buildDate, buildNumber."
  (jiralib-call "getServerInfo" nil))

(defun jiralib-get-sub-task-issue-types ()
  "Return all visible subtask issue types in the system."
  (jiralib-call "getSubTaskIssueTypes" nil))

(defun jiralib-get-user (account-id)
  "Return a user's information given their full name."
  (cond ((eq 0 (length account-id)) nil) ;; Unassigned
        (t (jiralib-call "getUser" nil account-id))))

(defvar jiralib-users-cache nil "Cached list of users.")

(defun jiralib-get-users (project-key)
  "Return assignable users information given the PROJECT-KEY."
  (unless jiralib-users-cache
    (setq jiralib-users-cache
          (jiralib-call "getUsers" nil project-key))
    (cl-loop for (name . id) in org-jira-users do
          (setf jiralib-users-cache (append (list (jiralib-get-user id)) jiralib-users-cache))))
  jiralib-users-cache)

(defun jiralib-get-versions (project-key)
  "Return all versions available in project PROJECT-KEY."
  (jiralib-call "getVersions" nil project-key))

(defun jiralib-strip-cr (string)
  "Remove carriage returns from STRING."
  (when string (replace-regexp-in-string "\r" "" string)))

(defun jiralib-worklog-import--filter-apply
    (worklog-obj &optional predicate-fn-lst unwrap-worklog-records-fn rewrap-worklog-records-fn)
  "Remove non-matching org-jira issue worklogs.

Variables:
  WORKLOG-OBJ is the passed in object
  PREDICATE-FN-LST is the list of lambdas used as match predicates.
  UNWRAP-WORKLOG-RECORDS-FN is the function used to produce the list of worklog records from within the worklog-obj
  REWRAP-WORKLOG-RECORDS-FN is the function used to reshape the worklog records back into the form they were received in.

Auxiliary Notes:
  Only the WORKLOG-OBJ variable is required.
  The value of PPREDICATE-FN-LST is filled from the jiralib-worklog-import--filters-alist variable by default.
  If PREDICATE-FN-LST is empty the unmodified value of WORKLOG-OBJ is returned.
  If PREDICATE-FN-LST contains multiple predicate functions, each predicate filters operates as a clause in an AND match.  In effect, a worklog must match all predicates to be returned.
  The variable 'jiralib-user-login-name is used by many lambda filters."

  (let
      ((unwrap-worklog-records-fn)
       (rewrap-worklog-records-fn)
       (predicate-fn-lst)
       (worklogs worklog-obj)
       (predicate-fn))
    ;; let-body
    (progn
      (setq unwrap-worklog-records-fn
            (if (and
                 (boundp 'unwrap-worklog-records-fn)
                 (functionp unwrap-worklog-records-fn))
                unwrap-worklog-records-fn
              (lambda (x) (cl-coerce x 'list))))
      (setq rewrap-worklog-records-fn
            (if (and
                 (boundp 'rewrap-worklog-records-fn)
                 (functionp rewrap-worklog-records-fn))
                rewrap-worklog-records-fn
              (lambda (x) (remove 'nil (cl-coerce x 'vector)))))
      (setq predicate-fn-lst
            (if (and (boundp 'predicate-fn-lst)
                     (not (null predicate-fn-lst))
                     (listp predicate-fn-lst))
                predicate-fn-lst
              (mapcar 'caddr
                      (remove 'nil
                              (mapcar (lambda (x) (unless (null (car x)) x))
                                      jiralib-worklog-import--filters-alist)))))
      ;; final condition/sanity checks before processing
      (cond
       ;; pass cases, don't apply filters, return unaltered worklog-obj
       ((or (not (boundp 'predicate-fn-lst)) (not (listp predicate-fn-lst)) (null predicate-fn-lst))
        worklog-obj)
       ;; default-case, apply worklog filters and return only matching worklogs
       (t
        (setq worklogs (funcall unwrap-worklog-records-fn worklogs))
        (while (setq predicate-fn (pop predicate-fn-lst))
          (setq worklogs (mapcar predicate-fn worklogs)))
        (funcall rewrap-worklog-records-fn worklogs))))))


(defun jiralib-get-board (id &optional callback)
  "Return details on given board"
  (jiralib-call "getBoard" nil id))

(defun jiralib-get-boards ()
  "Return list of jira boards"
  (jiralib-call "getBoards" nil))

(defun jiralib-get-board-sprints (id)
  "Return list of jira sprints in the specified jira board"
  (jiralib-call "getSprintsFromBoard" nil id))

(defun jiralib-get-sprint-issues (id &rest params)
  "Return list of issues in the specified sprint"
  (apply 'jiralib-call "getIssuesFromSprint"
	 (cl-getf params :callback) id params))

(defun jiralib-get-board-issues (board-id &rest params)
  "Return list of jira issues in the specified jira board"
  (apply 'jiralib-call "getIssuesFromBoard"
	 (cl-getf params :callback) board-id params))

(defvar jiralib-labels-cache nil)
(defun jiralib-get-labels ()
  "Return assignable labels that can be added to an issue."
  (unless jiralib-labels-cache
    (setq jiralib-labels-start-at 0)
    (while (progn
             (let* ((labels (jiralib-call "getLabels" nil jiralib-labels-start-at))
                    (max-results (alist-get 'maxResults labels))
                    (is-last (alist-get 'isLast labels))
                    (values (alist-get 'values labels)))
               (setq jiralib-labels-start-at (+ max-results jiralib-labels-start-at)
                     jiralib-labels-cache (append values jiralib-labels-cache))
               (not (eq is-last t)))))))

(defun jiralib--agile-not-last-entry (num-entries total start-at limit)
  "Return true if need to retrieve next page from agile api"
  (and (> num-entries 0)
       (or (not limit) ; not required to be set
	   (< limit 1) ; ignore invalid limit
	   (> limit start-at))
       (or (not total) ; not always returned
           (> total start-at))))

(defun jiralib--agile-limit-page-size (page-size start-at limit)
  (if (and limit
	   (> (+ start-at page-size) limit))
      (- limit  start-at)
    page-size))


(defun jiralib--agile-rest-call-it (api max-results start-at limit query-params)
  (let ((callurl
	 (format "%s?%s" api
		 (url-build-query-string
		  (append `((maxResults ,(jiralib--agile-limit-page-size max-results start-at limit))
			    (startAt ,start-at))
			  query-params)))))
    (jiralib--rest-call-it callurl)))

(defun jiralib--agile-call-it (api values-key &rest params)
  "Invoke Jira agile method api and retrieve the results using
paging.

If JIRALIB-COMPLETE-CALLBACK is non-nil, then the call will be
performed asynchronously and JIRALIB-COMPLETE-CALLBACK will be
called when all data are retrieved.

If JIRALIB-COMPLETE-CALLBACK is nil, then the call will be
performed synchronously and this function will return the
retrieved data.

API - path to called API that must start with /rest/agile/1.0.

VALUES-KEY - key of the actual reply data in the reply assoc list.

PARAMS - optional additional parameters.
:limit - limit total number of retrieved entries.
:query-params - extra query parameters in the format of url-build-query-string.
"
  (if jiralib-complete-callback
      (apply 'jiralib--agile-call-async api values-key params)
    (apply 'jiralib--agile-call-sync api values-key params)))

(defun jiralib--agile-call-sync (api values-key &rest params)
  "Syncroniously invoke Jira agile method api retrieve all the
results using paging and return results.

VALUES-KEY - key of the actual reply data in the reply assoc list.

PARAMS - extra parameters (as keyword arguments), the supported parameters are:

:limit - limit total number of retrieved entries.
:query-params - extra query parameters in the format of url-build-query-string.
"
  (setq jiralib-complete-callback nil)
  (let ((not-last t)
        (start-at 0)
	(limit (cl-getf params :limit))
	(query-params (cl-getf params :query-params))
	;; maximum page size, 50 is server side maximum
        (max-results jiralib-agile-page-size)
        (values ()))
    (while not-last
      (let* ((reply-alist
	      (jiralib--agile-rest-call-it api max-results start-at limit query-params))
             (values-array (cdr (assoc values-key reply-alist)))
             (num-entries (length values-array))
             (total (cdr (assq 'total reply-alist))))
        (setf values (append values (append values-array nil)))
        (setf start-at (+ start-at num-entries))
        (setf not-last (jiralib--agile-not-last-entry num-entries total start-at limit))))
    values))

(defun jiralib--agile-call-async  (api values-key &rest params)
  "Asyncroniously invoke Jira agile method api,
retrieve all the results using paging and call
JIRALIB-COMPLETE_CALLBACK when all the data are retrieved.

VALUES-KEY - key of the actual reply data in the reply assoc list.

PARAMS - extra parameters (as keyword arguments), the supported parameters are:

limit - limit total number of retrieved entries."
  (lexical-let
      ((start-at 0)
       (limit (cl-getf params :limit))
       (query-params (cl-getf params :query-params))
       ;; maximum page size, 50 is server side maximum
       (max-results jiralib-agile-page-size)
       (values-list ())
       (vk values-key)
       (url api)
       ;; save the call back to be called later after the last page
       (complete-callback jiralib-complete-callback))
    ;; setup new callback to be called after each page
    (setf jiralib-complete-callback
          (cl-function
           (lambda  (&rest data &allow-other-keys)
             (condition-case err
                 (let* ((reply-alist (cl-getf data :data))
                        (values-array (cdr (assoc vk reply-alist)))
                        (num-entries (length values-array))
                        (total (cdr (assq 'total reply-alist))))
                   (setf values-list (append values-list (append values-array nil)))
                   (setf start-at (+ start-at num-entries))
                   (message "jiralib agile retrieve: got %d values%s%s"
                            start-at
                            (if total " of " "")
                            (if total (int-to-string total) ""))
                   (if (jiralib--agile-not-last-entry num-entries total start-at limit)
		       (jiralib--agile-rest-call-it url max-results start-at limit query-params)
                     ;; last page: call originall callback
                     (message "jiralib agile retrieve: calling callback")
                     (setf jiralib-complete-callback complete-callback)
                     (funcall jiralib-complete-callback
                              :data  (list (cons vk  values-list)))
                     (message "jiralib agile retrieve: all done")))
               ('error (message (format "jiralib agile retrieve: caught error: %s" err)))))))
    (jiralib--agile-rest-call-it api max-results start-at limit query-params)))

(provide 'jiralib)
;;; jiralib.el ends here
