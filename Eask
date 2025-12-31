;; -*- mode: eask; lexical-binding: t -*-

(package "parinfer-rust-mode"
         "0.9.3"
         "An interface for the parinfer-rust library")

(website-url "https://github.com/justinbarclay/parinfer-rust-mode")
(keywords "lisp" "tools")

(package-file "parinfer-rust-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(source "melpa")

(depends-on "emacs" "29.2")
(depends-on "track-changes")

(development
 (depends-on "f")
 (depends-on "ert-runner")
 (depends-on "ecukes")
 (depends-on "elisp-lint")
 (depends-on "el-mock")
 ;; Depends on these for testing
 (depends-on "paredit")
 (depends-on "clojure-mode")
 (load-paths "./test/"))