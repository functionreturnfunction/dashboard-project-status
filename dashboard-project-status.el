;;; dashboard-project-status.el --- Display a git project status in a dashboard widget.     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0.1
;; URL: https://github.com/functionreturnfurnction/dashboard-project-status
;; Package-Requires: ((git "0.1.1") (dashboard "1.2.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display a git project status in a dashboard widget.

;;; Code:

(require 'git)
(require 'dashboard)

(defun git-local-is-behind? ()
  "Return non-nil if current `git-repo' is behind its remote."
  (numberp
   (string-match-p
    (regexp-quote "Your branch is behind")
    (git-run "status" "-uno"))))

(defun git-unstaged-files ()
  "Return list of unstaged files."
  (git--lines
   (git-run "diff" "--name-only")))

(defun dashboard-insert-project-status-heading ()
  "Insert a heading with project path and whether or not it is behind."
  (dashboard-insert-heading
   (concat "Project "
           git-repo
           (if (git-local-is-behind?)
               " is behind the remote. (use \"git-pull\" to update)"
             " is up-to-date.")
           hard-newline)))

(reverse
 (let ((git-repo user-emacs-directory)
       ret)
   (dolist (cur '("foo" "bar" "baz") ret)
     (setq ret (cons (expand-file-name
                      (concat (file-name-as-directory git-repo)
                              cur))
                     ret)))))

(defun dashboard-insert-project-status-body ()
  "Insert lists of untracked, unstaged, and staged files."
  (dolist (section `(("Untracked Files" . ,(git-untracked-files))
                     ("Unstaged Files"  . ,(git-unstaged-files))
                     ("Staged Files"    . ,(git-staged-files))))
    (dashboard-insert-recentf-list
     (car section)
     (reverse
      (let (ret)
        (dolist (cur (cdr section) ret)
          (setq ret (cons (expand-file-name
                           (concat (file-name-as-directory git-repo) cur))
                          ret))))))))

(defun dashboard-insert-project-status- (project-dir update)
  "Do the actual work for `dashboard-insert-project-status'."
  (when update (git-run "remote" "update"))
  (dashboard-insert-project-status-heading)
  (dashboard-insert-project-status-body))

(defun dashboard-insert-project-status (project-dir &optional update)
  "Return a function which will insert git status for PROJECT-DIR.
If UPDATE is non-nil, update the remote first with 'git remote update'."
  `(lambda (list-size)
     (let ((git-repo ,project-dir))
       (dashboard-insert-project-status- ,project-dir ,update))))

(provide 'dashboard-project-status)
;;; dashboard-project-status.el ends here
