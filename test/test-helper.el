;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-tests.el")))

(require 'el-mock)
(require 'ert-expectations)
(require 'dashboard-project-status)
