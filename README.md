# org-jira mode

[![Join the chat at https://gitter.im/org-jira/Lobby](https://badges.gitter.im/org-jira/Lobby.svg)](https://gitter.im/org-jira/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Use Jira in Emacs org-mode.

## TOC

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [org-jira mode](#org-jira-mode)
    - [TOC](#toc)
    - [Setup](#setup)
        - [Installation](#installation)
        - [Configuration](#configuration)
    - [Usage](#usage)
        - [Getting Started](#getting-started)
        - [Keybinds](#keybinds)
        - [Customization](#customization)
            - [Authorization workaround (NOT secure)](#authorization-workaround-not-secure)
        - [Optimizations](#optimizations)
            - [Optimizing available actions for status changes](#optimizing-available-actions-for-status-changes)
    - [About](#about)
        - [Maintainer](#maintainer)
        - [License](#license)

<!-- markdown-toc end -->


## Setup
### Installation
To install, just grab it off of MELPA (ensure your ~/.emacs already
has MELPA set up):

```lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Then run `M-x package-install RET org-jira RET` and you're done!
### Configuration
In your ~/.emacs, you should set the variable as such:

```conf
(setq jiralib-url "https://your-site.atlassian.net")
```

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
(define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
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

#### Authorization workaround (NOT secure)
If your Jira instance has disabled basic auth, you can still get in
by copying your web browser's cookie (open up developer console, and
right click and 'Copy request as cURL', then copy/paste the cookie
into the jiralib-token variable):

```lisp
(defconst jiralib-token
  `("Cookie" . ,(format "__atl_path=...; studio.crowd.tokenkey=...")))
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
