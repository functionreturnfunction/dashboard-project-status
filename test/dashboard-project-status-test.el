;;; dashboard-project-status-test.el --- Tests for dashboard-project-status  -*- lexical-binding: t; -*-

;; Author:  Jason Duncan <jasond496@msn.com>

;;; Code:

(require 'cl)

(expectations
  (desc "dashboard-project-status without UPDATE value")
  (expect (not-called git-run)
    (stub dashboard-project-status-insert-heading)
    (stub dashboard-project-status-insert-body)
    (funcall (dashboard-project-status "foo") nil))

  (desc "dashboard-project-status with non-nil UPDATE value")
  (expect (mock (git-run "remote" "update") => nil)
    (stub dashboard-project-status-insert-heading)
    (stub dashboard-project-status-insert-body)
    (funcall (dashboard-project-status "foo" t) nil))

  (desc "dashboard-project-status inserts a heading")
  (expect (mock (dashboard-project-status-insert-heading) => nil)
    (stub dashboard-project-status-insert-body)
    (funcall (dashboard-project-status "foo") nil))

  (desc "dashboard-project-status inserts a body")
  (expect (mock (dashboard-project-status-insert-body 7) => nil)
    (stub dashboard-project-status-insert-heading)
    (funcall (dashboard-project-status "foo") 7))

  (desc "dashboard-project-status-insert-heading when magit is present inserts status text")
  ;; the blank in the middle is where the widget should go,
  ;; but I cannot as yet figure out a way to test that
  (expect "Project  is up-to-date."
    (stub dashboard-project-status-git-local-is-behind?)
    (stub widget-create)
    (with-temp-buffer
      (let ((git-repo "foo"))
        (mocklet (((functionp 'magit-status) => t))
          (dashboard-project-status-insert-heading)))
      (buffer-string)))

  (desc "dashboard-project-status-insert-heading when magit is not present only inserts text")
  (expect "Project foo is up-to-date."
    (stub dashboard-project-status-git-local-is-behind?)
    (with-temp-buffer
      (let ((git-repo "foo"))
        (mocklet (((functionp 'magit-status)))
          (dashboard-project-status-insert-heading)))
      (buffer-string)))

  (desc "dashboard-project-status-insert-body inserts nothing when no items")
  (expect ""
    (stub git-untracked-files)
    (stub dashboard-project-status-git-unstaged-files)
    (stub git-staged-files)
    (with-temp-buffer
      (dashboard-project-status-insert-body 5)
      (buffer-string)))

  (desc "dashboard-project-status-insert-body inserts untracked files")
  (expect (concat  "\nUntracked Files:"
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "foo")))
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "bar"))))
    (stub git-untracked-files => '("foo" "bar"))
    (stub dashboard-project-status-git-unstaged-files)
    (stub git-staged-files)
    (with-temp-buffer
      (let ((git-repo "foo"))
        (flet ((widget-create (&rest args)
                              (insert (car (last args)))))
          (dashboard-project-status-insert-body 5)))
      (buffer-string)))

  (desc "dashboard-project-status-insert-body inserts unstaged files")
  (expect (concat  "\nUnstaged Files:"
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "foo")))
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "bar"))))
    (stub git-untracked-files)
    (stub dashboard-project-status-git-unstaged-files => '("foo" "bar"))
    (stub git-staged-files)
    (with-temp-buffer
      (let ((git-repo "foo"))
        (flet ((widget-create (&rest args)
                              (insert (car (last args)))))
          (dashboard-project-status-insert-body 5)))
      (buffer-string)))

  (desc "dashboard-project-status-insert-body inserts staged files")
  (expect (concat  "\nStaged Files:"
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "foo")))
                   "\n    "
                   (abbreviate-file-name
                    (expand-file-name
                     (concat (file-name-as-directory "foo") "bar"))))
    (stub git-untracked-files)
    (stub dashboard-project-status-git-unstaged-files)
    (stub git-staged-files => '("foo" "bar"))
    (with-temp-buffer
      (let ((git-repo "foo"))
        (flet ((widget-create (&rest args)
                              (insert (car (last args)))))
          (dashboard-project-status-insert-body 5)))
      (buffer-string)))

  (desc "dashboard-project-status-insert-body observes limit")
  (expect (concat "\nUntracked Files:"
                  "\n    "
                  (abbreviate-file-name
                   (expand-file-name
                    (concat (file-name-as-directory "foo") "foo")))
                  "\n    "
                  (abbreviate-file-name
                   (expand-file-name
                    (concat (file-name-as-directory "foo") "bar")))
                  "\nUnstaged Files:"
                  "\n    "
                  (abbreviate-file-name
                   (expand-file-name
                    (concat (file-name-as-directory "foo") "foo")))
                  "\n    "
                  (abbreviate-file-name
                   (expand-file-name
                    (concat (file-name-as-directory "foo") "bar")))
                  "\nStaged Files:"
                  "\n    "
                  (abbreviate-file-name
                   (expand-file-name
                    (concat (file-name-as-directory "foo") "foo"))))
    (stub git-untracked-files => '("foo" "bar"))
    (stub dashboard-project-status-git-unstaged-files => '("foo" "bar"))
    (stub git-staged-files => '("foo" "bar"))
    (with-temp-buffer
      (let ((git-repo "foo"))
        (flet ((widget-create (&rest args)
                              (insert (car (last args)))))
          (dashboard-project-status-insert-body 5)))
      (buffer-string)))

  (desc "dashboard-project-status-git-local-is-behind? returns non nil when branch is behind")
  (expect t
    (mock (git-run "status" "-uno") => "Your branch is behind")
    (dashboard-project-status-git-local-is-behind?))

  (desc "dashboard-project-status-git-local-is-behind? returns nil when branch is not behind")
  (expect nil
    (mock (git-run "status" "-uno") => "")
    (dashboard-project-status-git-local-is-behind?))

  (desc "dashboard-project-status-git-unstaged-files returns a list of unstaged files")
  (expect '("foo" "bar")
    (mock (git-run "diff" "--name-only") => "foo\nbar")
    (dashboard-project-status-git-unstaged-files))
  )
