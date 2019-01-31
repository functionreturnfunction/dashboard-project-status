;;; dashboard-project-status.el --- Display a git project status in a dashboard widget.     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0.1
;; URL: https://github.com/functionreturnfurnction/dashboard-project-status
;; Package-Requires: ((git "0.1.1"))

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

(defun git-local-is-behind? ()
  (numberp
   (string-match-p
    (regexp-quote "Your branch is behind")
    (git-run "status" "-uno"))))

(defun check-git-project-status (&optional project-dir)
  "Update the remote and check the status of PROJECT-DIR."
  (let* ((project-dir
          (fix-os-path
           (concat
            (expand-file-name
             (or project-dir user-emacs-directory))))))
    (with-temp-buffer
      (call-process "git" nil nil nil "-C" project-dir "remote" "update")
      (call-process "git" nil t   nil "-C" project-dir "status" "-uno")
      (buffer-string))))

(defun dashboard-insert-project-status-heading ()
  (insert
   (concat "Project "
           git-repo
           (if (git-local-is-behind?)
               " is behind the remote. (use \"git-pull\" to update)"
             "is up-to-date."))))

(defun dashboard-insert-project-status-body ()
  

(defun dashboard-insert-project-status (project-dir)
  (let ((git-repo project-dir))
    (lambda (list-size)
      (dashboard-insert-project-status-heading)
      (dashboard-insert-project-status-body))))

(provide 'dashboard-project-status)
;;; dashboard-project-status.el ends here
