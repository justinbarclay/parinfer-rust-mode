;;; paren-parinfer-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: t; -*-
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

;; Tests for paren-mode

;;; Code:
(ert-deftest parinfer-paren-0 ()
 (let ((before
"(let [foo 1]
foo)")
       (changes
nil)
       (after
"(let [foo 1]
 foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-1 ()
 (let ((before
"(let [foo 1]
      foo)")
       (changes
nil)
       (after
"(let [foo 1]
     foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-2 ()
 (let ((before
"(let [foo {:a 1}]
           foo)")
       (changes
nil)
       (after
"(let [foo {:a 1}]
     foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-3 ()
 (let ((before
"(let [foo 1]
      foo)

(let [foo 1]
foo)")
       (changes
nil)
       (after
"(let [foo 1]
     foo)

(let [foo 1]
 foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-4 ()
 (let ((before
"(let [foo [1 2 3]]
      (-> foo
          (map inc)))")
       (changes
nil)
       (after
"(let [foo [1 2 3]]
     (-> foo
         (map inc)))"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-5 ()
 (let ((before
"(let [foo 1
      ]; <-- spaces
  foo)")
       (changes
nil)
       (after
"(let [foo 1]
      ; <-- spaces
  foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-6 ()
 (let ((before
"(let [foo 1
      bar 2

     ] (+ foo bar
  ); <-- spaces
)")
       (changes
nil)
       (after
"(let [foo 1
      bar 2]

     (+ foo bar))
  ; <-- spaces
"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-7 ()
 (let ((before
"(def x [1 2 3 4
         5 6 7 8])")
       (changes
nil)
       (after
"(def x [1 2 3 4
         5 6 7 8])"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-8 ()
 (let ((before
"  (assoc x
:foo 1
     :bar 2)")
       (changes
nil)
       (after
"  (assoc x
   :foo 1
     :bar 2)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-9 ()
 (let ((before
"(foo")
       (changes
nil)
       (after
"(foo"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-10 ()
 (let ((before
"(defn foo
[arg arg2
bar")
       (changes
nil)
       (after
"(defn foo
[arg arg2
bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-11 ()
 (let ((before
"(foo})")
       (changes
nil)
       (after
"(foo})"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-12 ()
 (let ((before
"(foo
  })")
       (changes
nil)
       (after
"(foo
  })"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-13 ()
 (let ((before
"(defn foo
  [arg
  bar)")
       (changes
nil)
       (after
"(defn foo
  [arg
  bar)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-14 ()
 (let ((before
"; hello \\n world")
       (changes
nil)
       (after
"; hello \\n world"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-15 ()
 (let ((before
"(def foo \\,)
(def bar \\ )")
       (changes
nil)
       (after
"(def foo \\,)
(def bar \\ )"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-16 ()
 (let ((before
"(foo [a b]\\
c)")
       (changes
nil)
       (after
"(foo [a b]\\
c)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-17 ()
 (let ((before
"(def foo
  \"hello
  bar)")
       (changes
nil)
       (after
"(def foo
  \"hello
  bar)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-18 ()
 (let ((before
"(def foo [a b]
  ; \"my string
ret)")
       (changes
nil)
       (after
"(def foo [a b]
  ; \"my string
ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-19 ()
 (let ((before
"(def foo [a b]
  ; \"my multiline
  ; docstring.\"
ret)")
       (changes
nil)
       (after
"(def foo [a b]
  ; \"my multiline
  ; docstring.\"
 ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-20 ()
 (let ((before
"( )\"
\"")
       (changes
nil)
       (after
"( )\"
\""))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-21 ()
 (let ((before
"(foo |)")
       (changes
nil)
       (after
"(foo )"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-22 ()
 (let ((before
"(foo [1 2 3 |] )")
       (changes
nil)
       (after
"(foo [1 2 3 ] )"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-23 ()
 (let ((before
"(foo )")
       (changes
nil)
       (after
"(foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-24 ()
 (let ((before
"(foo [1 2 3 ] )")
       (changes
nil)
       (after
"(foo [1 2 3])"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-25 ()
 (let ((before
"(foo [a b
|])")
       (changes
nil)
       (after
"(foo [a b
      ])"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-26 ()
 (let ((before
"(foo [1 2 3
 4 5 6
 7 8 9])|")
       (changes
nil)
       (after
"(foo [1 2 3
      4 5 6
      7 8 9])"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-27 ()
 (let ((before
"(let [foo 1
           ; comment 1
           bar 2
           baz 3])
           ; comment 2")
       (changes
'(((lineNo . 0) (x . 5) (oldText . "    ") (newText . ""))))
       (after
"(let [foo 1
       ; comment 1
       bar 2
       baz 3])
       ; comment 2"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-28 ()
 (let ((before
"(def foo
      ; comment 1
      bar)
      ; comment 2")
       (changes
'(((lineNo . 0) (x . 0) (oldText . "   ") (newText . ""))))
       (after
"(def foo
   ; comment 1
   bar)
   ; comment 2"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-29 ()
 (let ((before
"(def foo (bar
       4 5 6
       ; comment 1
       7 8 9))
       ; comment 2")
       (changes
'(((lineNo . 0) (x . 4) (oldText . "") (newText . " foo"))))
       (after
"(def foo (bar
           4 5 6
           ; comment 1
           7 8 9))
           ; comment 2"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-30 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-31 ()
 (let ((before
"(foo [bar baz]
       1 2 3
       4 5 6)")
       (changes
nil)
       (after
"(foo [bar baz]
     1 2 3
     4 5 6)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-32 ()
 (let ((before
"(foo [bar baz
         ]; <-- spaces
       1 2 3
       4 5 6)")
       (changes
nil)
       (after
"(foo [bar baz]
         ; <-- spaces
     1 2 3
     4 5 6)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-33 ()
 (let ((before
"(defn foo
  \"hello, this is a docstring\"
  [a b]
  (let [sum (+ a b)
        prod (* a b)]
     {:sum sum
      :prod prod}))")
       (changes
nil)
       (after
"(defn foo
  \"hello, this is a docstring\"
  [a b]
  (let [sum (+ a b)
        prod (* a b)]
     {:sum sum
      :prod prod}))"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-34 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-35 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-36 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-37 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-38 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-39 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-40 ()
 (let ((before
"(def x [1 2 3])
(def y 2)
|")
       (changes
nil)
       (after
"(def x [1 2 3])
(def y 2)
"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-41 ()
 (let ((before
"(foo bar
  (baz boo))
|")
       (changes
nil)
       (after
"(foo bar
  (baz boo))
"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-42 ()
 (let ((before
"(let [a {:foo 1}
      |
      bar [1 2 3]]
  bar)")
       (changes
nil)
       (after
"(let [a {:foo 1}
      
      bar [1 2 3]]
  bar)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
(ert-deftest parinfer-paren-43 ()
 (let ((before
"(let [a {:foo 1}
      bar (func 1 2 3)]
  |
  bar)")
       (changes
nil)
       (after
"(let [a {:foo 1}
      bar (func 1 2 3)]
  
  bar)"))
    (should (equal (simulate-parinfer-in-another-buffer before "paren" changes)
                   after))))
