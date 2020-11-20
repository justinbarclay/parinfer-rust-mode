;;; smart-parinfer-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: t; -*-
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

;; Tests for smart-mode

;;; Code:
(ert-deftest parinfer-smart-0 ()
 (let ((before
"(let [a 1
      |])")
       (changes
nil)
       (after
"(let [a 1
      ])"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-1 ()
 (let ((before
"(let [a 1
      ]); <-- spaces")
       (changes
nil)
       (after
"(let [a 1])
      ; <-- spaces"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-2 ()
 (let ((before
"(let [a 1
      |] (+ a 2))")
       (changes
nil)
       (after
"(let [a 1
      ] (+ a 2))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-3 ()
 (let ((before
"(let [a 1
      ] (+ a 2))")
       (changes
nil)
       (after
"(let [a 1]
     (+ a 2))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-4 ()
 (let ((before
"(let [a 1
  |] (+ a 2))")
       (changes
nil)
       (after
"(let [a 1
      ] (+ a 2))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-5 ()
 (let ((before
"(let [a 1
      ]|)")
       (changes
nil)
       (after
"(let [a 1]
     )"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-6 ()
 (let ((before
"|)")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "(") (newText . ""))))
       (after
""))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-7 ()
 (let ((before
"(foo
  |))")
       (changes
'(((lineNo . 1) (x . 2) (oldText . "(bar") (newText . ""))))
       (after
"(foo
  )"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-8 ()
 (let ((before
"(foo
  }|)")
       (changes
nil)
       (after
"(foo
  )"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-9 ()
 (let ((before
"(foo
  ) foo} bar|")
       (changes
nil)
       (after
"(foo
  ) foo} bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-10 ()
 (let ((before
"(foo
  ) (bar|")
       (changes
nil)
       (after
"(foo
  ) (bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-11 ()
 (let ((before
"(foo (bar)
      baz)")
       (changes
'(((lineNo . 1) (x . 5) (oldText . "") (newText . " "))))
       (after
"(foo (bar
      baz))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-12 ()
 (let ((before
"(foo
{:a 1
   :b 2})")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "  ") (newText . ""))))
       (after
"(foo)
{:a 1
 :b 2}"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-13 ()
 (let ((before
"(foo)
  {:a 1
 :b 2}")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "") (newText . "  "))))
       (after
"(foo
  {:a 1
   :b 2})"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-14 ()
 (let ((before
"(foo
  {:a 1
:b 2})")
       (changes
'(((lineNo . 2) (x . 0) (oldText . "   ") (newText . ""))))
       (after
"(foo
  {:a 1})
:b 2"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-15 ()
 (let ((before
"(defn foo
[a b]
  bar)")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "  ") (newText . ""))))
       (after
"(defn foo)
[a b
  bar]"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-16 ()
 (let ((before
"(defn foo
    [a b]
    bar)")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "  ") (newText . ""))))
       (after
"(defn foo
  [a b]
  bar)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-17 ()
 (let ((before
"(defn foo
    [a b]
    ; comment 1
    bar)
    ; comment 2")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "  ") (newText . ""))))
       (after
"(defn foo
  [a b]
  ; comment 1
  bar)
  ; comment 2"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-18 ()
 (let ((before
"(defn foo
|[a b
   c d]
  bar
  baz)")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "  ") (newText . ""))))
       (after
"(defn foo)
[a b
 c d]
  bar
  baz"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-19 ()
 (let ((before
"(defn foo)
|[a b
 c d]
  bar
  baz")
       (changes
nil)
       (after
"(defn foo)
[a b
 c d]
  bar
  baz"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-20 ()
 (let ((before
"(foo (if some-condition
         println) foo {:foo 1
                          :bar 2})")
       (changes
'(((lineNo . 0) (x . 1) (oldText . "my-fn") (newText . "foo")) ((lineNo . 1) (x . 18) (oldText . "my-fun") (newText . "foo"))))
       (after
"(foo (if some-condition
       println) foo {:foo 1
                     :bar 2})"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-21 ()
 (let ((before
"(|(((1
        2
        3)))
    4)")
       (changes
'(((lineNo . 0) (x . 1) (oldText . "foo ") (newText . ""))))
       (after
"((((1
    2
    3)))
    4)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-22 ()
 (let ((before
"(@(((1
    2
    3)))
    4)")
       (changes
nil)
       (after
"((((1
    2
    3)))
 4)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-23 ()
 (let ((before
"(@(|((1
    2
    3)))
    4)")
       (changes
nil)
       (after
"((((1
    2
    3)))
 4)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-24 ()
 (let ((before
"  (foo
  (bar
    baz))")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "") (newText . "  "))))
       (after
"  (foo
    (bar
      baz))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-25 ()
 (let ((before
"  (foo
    (bar
    baz))")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "") (newText . "  ")) ((lineNo . 1) (x . 0) (oldText . "") (newText . "  "))))
       (after
"  (foo
    (bar
      baz))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-26 ()
 (let ((before
"  (foo
      (bar
        baz))")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "") (newText . "  ")) ((lineNo . 2) (x . 0) (oldText . "") (newText . "  "))))
       (after
"  (foo
      (bar
        baz))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-27 ()
 (let ((before
"  (foo
  bar
  baz)")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "") (newText . "  "))))
       (after
"  (foo
    bar
    baz)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-28 ()
 (let ((before
"  (foo
    bar
  baz)")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "") (newText . "  ")) ((lineNo . 1) (x . 0) (oldText . "") (newText . "  "))))
       (after
"  (foo
    bar
    baz)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-29 ()
 (let ((before
"(foo
    bar
    baz)")
       (changes
'(((lineNo . 1) (x . 0) (oldText . "") (newText . "  ")) ((lineNo . 2) (x . 0) (oldText . "") (newText . "  "))))
       (after
"(foo
    bar
    baz)"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-30 ()
 (let ((before
"((reduce-kv (fn [m k v]
            {}
            {})))")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "") (newText . "(")) ((lineNo . 1) (x . 11) (oldText . "") (newText . " ")) ((lineNo . 2) (x . 11) (oldText . "") (newText . " ")) ((lineNo . 2) (x . 16) (oldText . "") (newText . ")"))))
       (after
"((reduce-kv (fn [m k v])
            {}
            {}))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-31 ()
  (let ((before
"(let [a 1]
  (
    (foo)))")
       (changes
'(((lineNo . 1) (x . 2) (oldText . "") (newText . "(")) ((lineNo . 2) (x . 2) (oldText . "") (newText . "  ")) ((lineNo . 2) (x . 9) (oldText . "") (newText . ")"))))
       (after
"(let [a 1]
  (
    (foo)))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-32 ()
 (let ((before
"(let [a 1]
  (let [a 1]
    (foo))
  (foo))")
       (changes
'(((lineNo . 1) (x . 2) (oldText . "") (newText . "(let [a 1]
  (foo))")) ((lineNo . 2) (x . 2) (oldText . "") (newText . "  "))))
       (after
"(let [a 1]
  (let [a 1]
    (foo))
  (foo))"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
(ert-deftest parinfer-smart-33 ()
 (let ((before
"{:a {:b (Integer/valueOf (-> \"\"
                             (.length)))}}")
       (changes
'(((lineNo . 0) (x . 4) (oldText . "                ") (newText . "")) ((lineNo . 0) (x . 8) (oldText . "             ") (newText . "")) ((lineNo . 1) (x . 29) (oldText . "                             ") (newText . ""))))
       (after
"{:a {:b (Integer/valueOf (-> \"\"
                             (.length)))}}"))
    (should (equal (simulate-parinfer-in-another-buffer before "smart" changes)
                   after))))
