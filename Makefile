# Really just so we can run 'make test' easily

all: test

test:
	@emacs -batch \
	-l ert \
	-l t/batch-runner/s.el \
	-l t/batch-runner/request.el \
	-l jiralib.el \
	-l org-jira.el \
	-l t/org-jira-t.el \
	-l t/jiralib-t.el \
	-f ert-run-tests-batch-and-exit

.PHONY: test
