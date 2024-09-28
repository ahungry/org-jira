# org-jira mode

[![Join the chat at https://gitter.im/org-jira/Lobby](https://badges.gitter.im/org-jira/Lobby.svg)](https://gitter.im/org-jira/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![MELPA](http://melpa.org/packages/org-jira-badge.svg)](http://melpa.org/#/org-jira)

Use Jira in Emacs org-mode.

Sample of an org-jira buffer with a ticket synced using the
`org-jira-get-issues-from-custom-jql` functionality:

```org
* AHU-Tickets
** TODO [#C] a sample ticket with priority, in my AHU project           :AHU_39:
:PROPERTIES:
:assignee: Matthew Carter
:filename: this-years-work
:reporter: Matthew Carter
:type:     Story
:priority: Medium
:status:   To Do
:created:  2019-01-24T23:24:54.321-0500
:updated:  2021-07-19T18:40:30.722-0400
:ID:       AHU-39
:CUSTOM_ID: AHU-39
:type-id:  10100
:END:
:LOGBOOK:
CLOCK: [2022-02-24 Thu 20:30]--[2022-02-24 Thu 20:35] =>  0:05
  :id: 10359
  Sample time clock entry
:END:
*** description: [[https://example.atlassian.net/browse/AHU-39][AHU-39]]
  The summary is here
*** Comment: Matthew Carter
:PROPERTIES:
:ID:       10680
:created:  2019-01-24T23:25:19.455-0500
:updated:  2019-01-24T23:27:36.125-0500
:END:
  a sample comment on 39

```

## TOC

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [org-jira mode](#org-jira-mode)
    - [TOC](#toc)
    - [Contributing](#contributing)
    - [Setup](#setup)
        - [Installation](#installation)
        - [Configuration](#configuration)
    - [Usage](#usage)
        - [Getting Started](#getting-started)
        - [Keybinds](#keybinds)
        - [Customization](#customization)
            - [Get Issues from Custom JQL](#get-issues-from-custom-jql)
            - [Streamlined transition flow](#streamlined-transition-flow)
            - [Basic auth via personal API tokens (secure'ish)](#basic-auth-via-personal-api-tokens-secureish)
                - [Extra basic auth note (thanks @mujo-hash)](#extra-basic-auth-note-thanks-mujo-hash)
            - [Using Bearer authentication with Personal Access Tokens](#using-bearer-authentication-with-personal-access-tokens)
            - [Last Resort Authorization workaround (NOT secure)](#last-resort-authorization-workaround-not-secure)
        - [Optimizations](#optimizations)
            - [Optimizing available actions for status changes](#optimizing-available-actions-for-status-changes)
    - [About](#about)
        - [Maintainer](#maintainer)
        - [License](#license)

<!-- markdown-toc end -->

## Contributing
![CONTRIBUTING.md](https://github.com/ahungry/org-jira/blob/master/CONTRIBUTING.md)

## Setup
### Installation
To install, just grab it off of MELPA (ensure your ~/.emacs already
has MELPA set up):

```lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Then run `M-x package-install RET org-jira RET` and you're done!
### Configuration

Create the `org-jira-working-dir` directory:

```lisp
(make-directory "~/.org-jira")
```

In your ~/.emacs, you should set the variable as such:

```conf
(setq jiralib-url "https://your-site.atlassian.net")
```

The first time you try and connect to jira you will be asked for your
username and password. Make sure to enter your username and not your
email if they are different. If you use two-step verification you will
need to create an [APItoken](https://confluence.atlassian.com/cloud/api-tokens-938839638.html)
and use that instead of your password.

If you don't want to enter your credentials (login/password) each time
you go to connect, you can add to your ~/.authinfo.gpg or ~/.authinfo
file, in a format similar to:

```conf
machine your-site.atlassian.net login you@example.com password yourPassword port 80
```
_Please note that in the authinfo file, port 443 should be specified
if your jiralib-url is https._

## Usage
### Getting Started
org-jira mode is easy to use, to get started (after installing this
library) try running `M-x org-jira-get-issues`.  You should see that
it pulls in all issues that are assigned to you.

Following that, you can try out some of the org-jira mode commands by
visiting one of the files (they're named after your Jira project code,
for example, 'EX.org' for a project named 'EX').
### Keybinds
Some of the important keybindings:

```lisp
(define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
(define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
(define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
(define-key org-jira-map (kbd "C-c ij") 'org-jira-get-issues-from-custom-jql)
(define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
(define-key org-jira-map (kbd "C-c il") 'org-jira-update-issue-labels)
(define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
(define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
(define-key org-jira-map (kbd "C-c in") 'org-jira-progress-issue-next)
(define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
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
```

### Customization

#### Get Issues from Custom JQL

You can define one or more custom JQL queries to run and have your
results inserted into, as such:

```
(setq org-jira-custom-jqls
  '(
    (:jql " project IN (EX, AHU) and createdDate < '2019-01-01' order by created DESC "
          :limit 10
          :filename "last-years-work")
    (:jql " project IN (EX, AHU) and createdDate >= '2019-01-01' order by created DESC "
          :limit 10
          :filename "this-years-work")
    (:jql "
project IN (EX, AHU)
and status IN ('To Do', 'In Development')
AND (labels = EMPTY or labels NOT IN ('FutureUpdate'))
order by priority, created DESC "
          :limit 20
          :filename "ex-ahu-priority-items")
    ))
```

Please note this feature still requires some testing - things that may
work in the existing proj-key named buffers (EX.org etc.) may behave
unexpectedly in the custom named buffers.

One thing you may notice is if you create an issue in this type of
buffer, the auto-refresh of the issue will appear in the PROJ-KEY.org
specific buffer (you will then need to refresh this JQL buffer by
re-running the command `C-c ij`).

#### Streamlined transition flow

You can define your own streamlined issue progress flow as such:

```lisp
(defconst org-jira-progress-issue-flow
  '(("To Do" . "In Progress"
    ("In Progress" . "Done"))))
```
or using typical Emacs customize options, as its a defcustom.

This will allow you to quickly progress an issue based on its current
status, and what the next status should be.

If your Jira is set up to display a status in the issue differently
than what is shown in the button on Jira, your alist may look like
this (use the labels shown in the org-jira Status when setting it up,
or manually work out the workflows being used through
standard `C-c iw` options/usage):

```lisp
(defconst org-jira-progress-issue-flow
  '(("To Do" . "Start Progress")
    ("In Development" . "Ready For Review")
    ("Code Review" . "Done")
    ("Done" . "Reopen")))
```

#### Basic auth via personal API tokens (secure'ish)
Use a personal API token from here:

https://id.atlassian.com/manage/api-tokens

and your email address from here:

https://id.atlassian.com/manage-profile/email

and you should be able to work with basic auth, even if you are
required to auth via Google usually on the browser domain.

##### Extra basic auth note (thanks @mujo-hash)
For Basic Authentication on cloud Jira, an api token must be used now in place of a password:

https://developer.atlassian.com/cloud/jira/platform/basic-auth-for-rest-apis/

https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/

The self-hosted version of Jira appears to still support Basic Authentication with a user password:

https://developer.atlassian.com/server/jira/platform/basic-authentication/

#### Using Bearer authentication with Personal Access Tokens

Some JIRA instances might require usage of Authorization headers using Bearer
tokens as documented in [Using
PATs](https://confluence.atlassian.com/enterprise/using-personal-access-tokens-1026032365.html#UsingPersonalAccessTokens-UsingPATs)
Atlassian documentation.

As documented, PATs should be used without a username in Authorization header as
Bearer tokens. Following is an example of using PAT token stored in `authinfo`
to authenticate to JIRA:

```lisp
(setq jiralib-token
    (cons "Authorization"
          (concat "Bearer " (auth-source-pick-first-password
              :host "jira.company.com"))))
```

#### Last Resort Authorization workaround (NOT secure)
However, if all else fails (your Jira instance has disabled basic auth
entirely), you can still get in by copying your web browser's
cookie. Open up developer console and in the Network tab right click
the request for the JIRA page and select 'Copy request as cURL'. Paste
it into a file so you can copy out the value for the cookie
"cloud.session.token". Then set jiralib-token like this:

```lisp
(defconst jiralib-token
   '("Cookie" . "cloud.session.token=<YOUR COOKIE VALUE>"))
```

### Optimizations
It's possible some things in your Jira instance rarely if ever change - while org-jira
will dynamically query everything as needed, it makes use of some
variables/caching per-run of Emacs.  If you ever notice something was
changed on the Jira setup level, you may have to restart your Emacs
(or manually unset these variables).  By the same token, that makes it
possible to hardcode some of these values yourself, so org-jira never
needs to look them up.

Some samples may be:

#### Optimizing available actions for status changes
Take time inspecting jiralib-available-actions-cache variable as you
use org-jira, when you see the type of data it stores, you can then
just define it yourself, as such, so repeated usage will not need to
re-query the endpoints to get these lists:

```lisp
(defconst jiralib-available-actions-cache
  '(("To Do"
     ("71" . "Business Question")
     ("11" . "Start Progress"))
    ("Code Review"
     ("71" . "Business Question")
     ("91" . "Reject")
     ("171" . "Failed Peer Review")
     ("221" . "Done"))
    ("In Development"
     ("71" . "Business Question")
     ("21" . "Ready For Review")
     ("81" . "Reject")
     ("161" . "Stop Progress"))
    ("Done"
     ("71" . "Business Question")
     ("141" . "Re-open"))))
```

## About
### Maintainer

You can reach me directly: Matthew Carter <m@ahungry.com>, or file an
issue here on https://github.com/ahungry/org-jira.

### License

GPLv3
