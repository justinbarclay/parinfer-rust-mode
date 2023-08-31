;;; indent-parinfer-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: nil; -*-
;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests user submitted cases

;;; Code:
(require 'paredit)
(require 'clojure-mode)
;; This covers issue #8
;; We expect delete-indentation to maintain hashmap-boundaries
(ert-deftest use-delete-indentation ()
  (let ((test
         '(:before
           "(def thing
  {:abc 1
   :xyz 2})"
           :after
           "(def thing {:abc 1
            :xyz 2})"
           :commands (((:lineNo 2 :column 2) delete-indentation)))))
    (should
     (string=
      (simulate-parinfer-in-another-buffer--with-commands (plist-get test :before)
                                                          "smart"
                                                          (plist-get test :commands))
      (plist-get test :after)))))

;; This covers issue #9
;; we expect fill paragraph to not cause the rest of the buffer to be realigned
;; This requires clojure-mode... need to think about _including_ this mode specific test
(ert-deftest user-fill-paragraph ()
  (let ((test
         '(:setup (clojure-mode)
                  :before
                  "(ns some-namespace.core
  (:import [java.util.concurrent Executors])
  (:require [clojure.string :as string]))

(defn foo
  \"pretty long docsting that will be affected by `fill-paragraph` function\"
  [x]
  x)"
                  :after
                  "(ns some-namespace.core
  (:import [java.util.concurrent Executors])
  (:require [clojure.string :as string]))

(defn foo
  \"pretty long docsting that will be affected by `fill-paragraph`
  function\"
  [x]
  x)"
                  :commands (((:lineNo 6 :column 4) fill-paragraph)))))
    (should
     (string=
      (simulate-parinfer-in-another-buffer--with-commands (plist-get test :before)
                                                          "smart"
                                                          (plist-get test :commands)
                                                          (plist-get test :setup))
      (plist-get test :after)))))

;; This covers issue #8
;; We expect delete-indentation to maintain hashmap-boundaries
(ert-deftest use-paredit-barf-sexp ()
  (let ((test
         '(:setup
           (clojure-mode paredit-mode
                         )
           :before
           "(foo- _foo [foo foo]
      (foo/foo foo {:foo foo-foo
                    :foo foo-foo
                    :foo-foo (foo [foo-foo]
                                  (foo/foo-foo! foo :foo-foo foo-foo)
                                  (foo-foo foo
                                           (foo
                                            foo
                                            {:foo_foo foo-foo
                                             :foo foo})
                                           foo-foo))}
               (foo2
                foo/Foo
                (foo-foo [_]
                         {:foo-foo \"Foo\"
                          :foo foo
                          :foo foo})
                foo/Foo
                (foo-foo [_]
                         (foo (foo-foo foo [:foo-foo :foo])
                              (foo/foo \"/foo/foo_foo\" #(foo/foo-foo! foo :foo-foo %)))))))"
           :after
           "(foo- _foo [foo foo]
      (foo/foo foo {:foo foo-foo
                    :foo foo-foo
                    :foo-foo (foo [foo-foo]
                                  (foo/foo-foo! foo :foo-foo foo-foo)
                                  (foo-foo foo
                                           (foo
                                            foo
                                            {:foo_foo foo-foo
                                             :foo foo})
                                           foo-foo))})
      (foo2
       foo/Foo
       (foo-foo [_]
                {:foo-foo \"Foo\"
                 :foo foo
                 :foo foo})
       foo/Foo
       (foo-foo [_]
                (foo (foo-foo foo [:foo-foo :foo])
                     (foo/foo \"/foo/foo_foo\" #(foo/foo-foo! foo :foo-foo %))))))"
           :commands (((:lineNo 11 :column 53) paredit-forward-barf-sexp)))))
    (should
     (string=
      (simulate-parinfer-in-another-buffer--with-commands (plist-get test :before)
                                                          "smart"
                                                          (plist-get test :commands)
                                                          (plist-get test :setup))
      (plist-get test :after)))))

(ert-deftest indent-buffer ()
  (defun indent-buffer ()
    (interactive)
    (setq-local this-command 'indent-buffer)
    (indent-region (point-min) (point-max)))
  (defun add-func-to-treat-command-as ()
    (add-to-list 'parinfer-rust-treat-command-as '(indent-buffer . "paren")))
  (let ((test
         '(:setup (clojure-mode add-func-to-treat-command-as)
                  :before
                  "               (defn vaiv []
        \"I am incorrect\"
   (let
 [a 1
         b 2]
                      (+ a b)))"
                  :after
                  "(defn vaiv []
  \"I am incorrect\"
  (let
      [a 1
       b 2]
    (+ a b)))"
                  :commands (((:lineNo 0 :column 0) indent-buffer)))))
    (should
     (string=
      (simulate-parinfer-in-another-buffer--with-commands (plist-get test :before)
                                                          "smart"
                                                          (plist-get test :commands)
                                                          (plist-get test :setup))
      (plist-get test :after)))))

(ert-deftest check-for-tabs--with-issues ()
  (should
   (let ((buffer-state "	(defun hello () \"world\")"))
     (with-temp-buffer
       (insert buffer-state)
       (parinfer-rust--check-for-tabs)))))

(ert-deftest check-for-tabs--without-issues ()
  (should-not
   (let ((buffer-state "(defun hello () \"world\")"))
     (with-temp-buffer
       (insert buffer-state)
       (parinfer-rust--check-for-tabs)))))

(ert-deftest check-for-indentation--with-issues ()
  (let ((buffer-state
         "(defun hello []
  (+ 1 2)
(- 3 4))"))
    (should
     (with-temp-buffer
       ;; Override prompt to simulate user saying no.
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (progn nil))))
         (insert buffer-state)
         (parinfer-rust--check-for-indentation))))))

(ert-deftest check-for-indentation--without-issues ()
  (let ((buffer-state
         "(defun hello []
  (+ 1 2)
  (- 3 4))"))
    (should-not
     (with-temp-buffer
       (insert buffer-state)
       (parinfer-rust--check-for-indentation)))))
