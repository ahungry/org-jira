org-jira mode
=============

Use [Jira](http://www.atlassian.com/software/jira/overview/) in Emacs
org-mode.

For usage, please watch [this youtube video](http://www.youtube.com/watch?v=6Sp3h1Qlf4w). Or watch it on [youku](http://v.youku.com/v_show/id_XNDc1NTIwOTY0.html) if you can't visit youtube for some reason. My apologies beforehand if you find the video too verbose and boring.

Commands supported:

```
org-jira-browse-issue	      M-x ... RET
   Open the current issue in external browser.
org-jira-copy-current-issue-key	M-x ... RET
   Copy the current issue's key into clipboard
org-jira-create-issue	      M-x ... RET
   Create an issue
org-jira-create-subtask	      M-x ... RET
   Create an subtask issue
org-jira-get-issues	      M-x ... RET
   Get list of issues. Default is get unfinished issues assigned
org-jira-get-issues-from-filter	M-x ... RET
   Get issues from filter which are jql created and saved on the
org-jira-get-issues-from-filter-headonly M-x ... RET
   Get issues *head only* from saved filter. See
   `org-jira-get-issues-from-filter'
org-jira-get-issues-headonly  M-x ... RET
   Get list of issues assigned to you and unresolved, head
org-jira-get-projects	      M-x ... RET
   Get list of projects.
org-jira-get-subtasks	      M-x ... RET
   Get subtasks for the current issue
org-jira-mode		      M-x ... RET
   Toggle org-jira mode.
org-jira-progress-issue	      M-x ... RET
   Progress issue workflow
org-jira-refresh-issue	      M-x ... RET
   Refresh issue from jira to org
org-jira-todo-to-jira	      M-x ... RET
   Convert an ordinary todo item to a jira ticket
org-jira-update-comment	      M-x ... RET
   Update a comment for the current issue
org-jira-update-issue	      M-x ... RET
   Update an issue
```
