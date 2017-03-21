;;; org-jira-t.el --- ERT tests

;; Copyright (C) 2017 Matthew Carter <m@ahungry.com>
;;
;; Authors:
;; Matthew Carter <m@ahungry.com>
;;
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/org-jira
;; Version: 2.6.2
;; Keywords: ahungry jira org bug tracker
;; Package-Requires: ((emacs "24.5") (cl-lib "0.5") (request "0.2.0"))

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

;;; Commentary:

;; This tests the extension to org-mode for syncing issues with JIRA
;; issue servers.

;;; News:

;;;; Changes since 0.0.0:
;; - Add some basic tests

;;; Code:

(require 'org-jira)

(ert-deftest org-jira-date-strip-letter-t-test ()
  (should
   (string= "2017-01-01 00:00:00.000-0500"
            (org-jira-date-strip-letter-t "2017-01-01T00:00:00.000-0500")))
  )

(ert-deftest org-jira-date-to-org-clock-test ()
  (should
   (string= "2017-01-01 Sun 00:00"
            (org-jira-date-to-org-clock "2017-01-01T00:00:00.000-0500")))
  (should
   (string= "2017-02-05 Sun 00:00"
            (org-jira-date-to-org-clock "2017-02-05T00:00:00.000-0500")))
  )

(ert-deftest org-jira-time-stamp-to-org-clock-test ()
  (should
   (string= "2016-12-31 Sat 19:05"
            (org-jira-time-stamp-to-org-clock '(22632 18348))))
  )

(provide 'org-jira-t)
;;; org-jira-t.el ends here
