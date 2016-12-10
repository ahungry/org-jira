# org-jira mode

Use Jira in Emacs org-mode.

## TOC

- [Setup](#setup)
  * [Install](#installation)
  * [Config](#configuration)
- [Usage](#usage)
  * [Getting Started](#gettingstarted)
  * [Keybinds](#keybinds)
- [About](#about)
  * [Maintainer](#maintainer)
  * [License](#license)

# Setup

## Installation

To install, just grab it off of MELPA (ensure your ~/.emacs already
has MELPA set up):

```lisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Then run `M-x package-install RET org-jira RET` and you're done!

## Configuration

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
_Please note that in the authinfo file, port 80 should be specified
even if your jiralib-url is https._

# Usage

## Getting Started

org-jira mode is easy to use, to get started (after installing this
library) try running `M-x org-jira-get-issues`.  You should see that
it pulls in all issues that are assigned to you.

Following that, you can try out some of the org-jira mode commands by
visiting one of the files (they're named after your Jira project code,
for example, 'EX.org' for a project named 'EX').
## Keybinds
Some of the important keybindings:

```lisp
(define-key org-jira-map (kbd "C-c pg") 'org-jira-get-projects)
(define-key org-jira-map (kbd "C-c ib") 'org-jira-browse-issue)
(define-key org-jira-map (kbd "C-c ig") 'org-jira-get-issues)
(define-key org-jira-map (kbd "C-c ih") 'org-jira-get-issues-headonly)
(define-key org-jira-map (kbd "C-c if") 'org-jira-get-issues-from-filter-headonly)
(define-key org-jira-map (kbd "C-c iF") 'org-jira-get-issues-from-filter)
(define-key org-jira-map (kbd "C-c iu") 'org-jira-update-issue)
(define-key org-jira-map (kbd "C-c iw") 'org-jira-progress-issue)
(define-key org-jira-map (kbd "C-c ia") 'org-jira-assign-issue)
(define-key org-jira-map (kbd "C-c ir") 'org-jira-refresh-issue)
(define-key org-jira-map (kbd "C-c ic") 'org-jira-create-issue)
(define-key org-jira-map (kbd "C-c ik") 'org-jira-copy-current-issue-key)
(define-key org-jira-map (kbd "C-c sc") 'org-jira-create-subtask)
(define-key org-jira-map (kbd "C-c sg") 'org-jira-get-subtasks)
(define-key org-jira-map (kbd "C-c cu") 'org-jira-update-comment)
(define-key org-jira-map (kbd "C-c wu") 'org-jira-update-worklog)
(define-key org-jira-map (kbd "C-c tj") 'org-jira-todo-to-jira)
```

# About

## Maintainer

You can reach me directly: Matthew Carter <m@ahungry.com>, or file an
issue here on https://github.com/ahungry/org-jira.

## License

GPLv3
