# org-jira mode

Use [Jira](http://www.atlassian.com/software/jira/overview/) in Emacs org-mode.

For usage, please watch [this youtube video](http://www.youtube.com/watch?v=6Sp3h1Qlf4w). Or watch it on [youku](http://v.youku.com/v_show/id_XNDc1NTIwOTY0.html) if
you can't visit youtube for some reason. My apologies beforehand if
you find the video too verbose and boring.

To save the settings in your .emacs, you need only do 2 things:

1.  Set the `defcustom` variables defined in jiralib.el (actually only
    the `jiralib-url` is required, as the other 2 are usually computed
    from it correctly and is the case in the video demo):
    
        3 matches for "\bdefcustom\b" in buffer: jiralib.el
             55:(defcustom jiralib-host ""
            107:(defcustom jiralib-wsdl-descriptor-url
            120:(defcustom jiralib-url

2.  Write your password in `~/.authinfo`, with machine being the
    (computed) jiralib-host, and the port being 80 (even if you are
    using https for Jira):
    
        machine JIRALIB-HOST login USERNAME password XXXXX port 80
    
    If you do not set USERNAME here, then it will be prompted using the
    minibuffer; if you set the wrong username/password, then it won't work.



## External libraries

The [emacs-soap-client](http://code.google.com/p/emacs-soap-client/source/checkout) package is required, I used to bundle it
together with org-jira (because I had a patch for adding comments in
Chinese), but [tarsius](https://github.com/baohaojun/org-jira/issues/8) pointed out to me it is not a good idea. So
please install that package along with org-jira.

About the patch, I have submitted it [upstream](http://code.google.com/p/emacs-soap-client/issues/detail?id=16&colspec=ID Type Status Priority Owner Summary). You can download it
from the [attachment url](http://emacs-soap-client.googlecode.com/issues/attachment?aid=160000000&name=0001-Patch-for-soap-client.patch&token=0w4_XPg-Fe9sNqcTqgNP5RTXlXY:1359427017708) if it is not accepted yet.

Update: the issue has been fixed by the author, and I've learned from
him that emacs-soap-client is bundled with GNU Emacs itself now, and
he will push the fix into it. So we just need to wait a bit more.

## Commands supported

    org-jira-browse-issue         M-x ... RET
       Open the current issue in external browser.
    org-jira-copy-current-issue-key M-x ... RET
       Copy the current issue's key into clipboard
    org-jira-create-issue         M-x ... RET
       Create an issue
    org-jira-create-subtask       M-x ... RET
       Create an subtask issue
    org-jira-get-issues       M-x ... RET
       Get list of issues. Default is get unfinished issues assigned
    org-jira-get-issues-from-filter M-x ... RET
       Get issues from filter which are jql created and saved on the
    org-jira-get-issues-from-filter-headonly M-x ... RET
       Get issues *head only* from saved filter. See
       `org-jira-get-issues-from-filter'
    org-jira-get-issues-headonly  M-x ... RET
       Get list of issues assigned to you and unresolved, head
    org-jira-get-projects         M-x ... RET
       Get list of projects.
    org-jira-get-subtasks         M-x ... RET
       Get subtasks for the current issue
    org-jira-mode             M-x ... RET
       Toggle org-jira mode.
    org-jira-progress-issue       M-x ... RET
       Progress issue workflow
    org-jira-refresh-issue        M-x ... RET
       Refresh issue from jira to org
    org-jira-todo-to-jira         M-x ... RET
       Convert an ordinary todo item to a jira ticket
    org-jira-update-comment       M-x ... RET
       Update a comment for the current issue
    org-jira-update-issue         M-x ... RET
       Update an issue

## Acknowledgements

This plug-in is greatly inspired by [org2blog](https://github.com/punchagan/org2blog).

The [emacs-soap-client](http://code.google.com/p/emacs-soap-client/source/checkout) is the underlying library used to communicate
with Jira. This project provided a sample jira2.el and I started with
that file.

This readme is written using org-mode and exported to markdown mode
for github using `org-md-export-to-markdown`.
