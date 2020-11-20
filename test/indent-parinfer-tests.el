;;; indent-parinfer-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: t; -*-
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

;; Tests for indent-mode

;;; Code:

(ert-deftest parinfer-indent-0 ()
 (let ((before
"(defn foo
  [arg
  ret")
       (changes
nil)
       (after
"(defn foo
  [arg]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-1 ()
 (let ((before
"(defn foo
  [arg
   ret")
       (changes
nil)
       (after
"(defn foo
  [arg
   ret])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-2 ()
 (let ((before
"(defn foo
[arg
   ret")
       (changes
nil)
       (after
"(defn foo)
[arg
   ret]"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-3 ()
 (let ((before
"(defn foo
[arg
ret")
       (changes
nil)
       (after
"(defn foo)
[arg]
ret"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-4 ()
 (let ((before
"(defn foo
  [arg
  ret

(defn foo
  [arg
  ret")
       (changes
nil)
       (after
"(defn foo
  [arg]
  ret)

(defn foo
  [arg]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-5 ()
 (let ((before
"bar)")
       (changes
nil)
       (after
"bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-6 ()
 (let ((before
"(def foo [a b]]")
       (changes
nil)
       (after
"(def foo [a b])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-7 ()
 (let ((before
"(let [x {:foo 1 :bar 2]
  x)")
       (changes
nil)
       (after
"(let [x {:foo 1 :bar 2}]
  x)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-8 ()
 (let ((before
"(foo [a (|b] c)")
       (changes
nil)
       (after
"(foo [a (b] c)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-9 ()
 (let ((before
"(def foo \"as")
       (changes
nil)
       (after
"(def foo \"as"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-10 ()
 (let ((before
"(defn foo [a \"])")
       (changes
nil)
       (after
"(defn foo [a \"])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-11 ()
 (let ((before
"(defn foo
  \"This is docstring.
  Line 2 here.\"
  ret")
       (changes
nil)
       (after
"(defn foo
  \"This is docstring.
  Line 2 here.\"
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-12 ()
 (let ((before
"(let [a \"Hello
World\"
      b 2
  ret")
       (changes
nil)
       (after
"(let [a \"Hello
World\"
      b 2]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-13 ()
 (let ((before
"(let [a \"])\"
      b 2")
       (changes
nil)
       (after
"(let [a \"])\"
      b 2])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-14 ()
 (let ((before
"(def foo \"\\\"\"")
       (changes
nil)
       (after
"(def foo \"\\\"\")"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-15 ()
 (let ((before
"()\"
\"")
       (changes
nil)
       (after
"()\"
\""))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-16 ()
 (let ((before
"\"|\"foo\"")
       (changes
nil)
       (after
"\"\"foo\""))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-17 ()
 (let ((before
"(def foo
  \"|
  \"(a b)
      c\")")
       (changes
nil)
       (after
"(def foo
  \"
  \"(a b)
      c\")"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-18 ()
 (let ((before
"(for [col columns]
  \"|
  [:div.td {:style \"max-width: 500px;\"}])")
       (changes
nil)
       (after
"(for [col columns]
  \"
  [:div.td {:style \"max-width: 500px;\"}])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-19 ()
 (let ((before
"(def foo [a b]
  ; \"my multiline
  ; docstring.\"
ret)")
       (changes
nil)
       (after
"(def foo [a b])
  ; \"my multiline
  ; docstring.\"
ret"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-20 ()
 (let ((before
"(def foo [a b]
  ; \"\"\\\"
ret)")
       (changes
nil)
       (after
"(def foo [a b])
  ; \"\"\\\"
ret"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-21 ()
 (let ((before
"(defn foo [a b
  \\[
  ret")
       (changes
nil)
       (after
"(defn foo [a b]
  \\[
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-22 ()
 (let ((before
"(defn foo [a b]
  ret\\)")
       (changes
nil)
       (after
"(defn foo [a b]
  ret\\))"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-23 ()
 (let ((before
"{:tag-open \\[ :tag-close \\]}
{:tag-open \\[ :tag-close \\]}")
       (changes
nil)
       (after
"{:tag-open \\[ :tag-close \\]}
{:tag-open \\[ :tag-close \\]}"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-24 ()
 (let ((before
"(def foo \\;")
       (changes
nil)
       (after
"(def foo \\;)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-25 ()
 (let ((before
"(def foo \\,
(def bar \\ ; <-- space")
       (changes
nil)
       (after
"(def foo \\,)
(def bar \\ ); <-- space"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-26 ()
 (let ((before
"(foo [a b\\
  c)")
       (changes
nil)
       (after
"(foo [a b\\
  c)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-27 ()
 (let ((before
"(def foo ;)")
       (changes
nil)
       (after
"(def foo) ;)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-28 ()
 (let ((before
"(let [a 1
      b 2
      c {:foo 1
         ;; :bar 2}]
  ret)")
       (changes
nil)
       (after
"(let [a 1
      b 2
      c {:foo 1}]
         ;; :bar 2}]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-29 ()
 (let ((before
"(let [a 1 ;; a comment
  ret)")
       (changes
nil)
       (after
"(let [a 1] ;; a comment
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-30 ()
 (let ((before
"; hello \\n world")
       (changes
nil)
       (after
"; hello \\n world"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-31 ()
 (let ((before
"(def b |)")
       (changes
nil)
       (after
"(def b )"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-32 ()
 (let ((before
"(def b )")
       (changes
nil)
       (after
"(def b)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-33 ()
 (let ((before
"(def b [[c d] |])")
       (changes
nil)
       (after
"(def b [[c d] ])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-34 ()
 (let ((before
"(def b [[c d] ])")
       (changes
nil)
       (after
"(def b [[c d]])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-35 ()
 (let ((before
"(let [a 1])
  ret)")
       (changes
nil)
       (after
"(let [a 1]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-36 ()
 (let ((before
"(let [a 1])|
  ret)")
       (changes
nil)
       (after
"(let [a 1])
  ret"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-37 ()
 (let ((before
"(let [a 1]) 2
  ret")
       (changes
nil)
       (after
"(let [a 1]) 2
  ret"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-38 ()
 (let ((before
"(let [a 1]|)
  ret)")
       (changes
nil)
       (after
"(let [a 1]
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-39 ()
 (let ((before
"(let [a 1]) ;|
  ret")
       (changes
nil)
       (after
"(let [a 1] ;
  ret)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-40 ()
 (let ((before
"(foo)}}}}|")
       (changes
nil)
       (after
"(foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-41 ()
 (let ((before
"(foo}}}}|)")
       (changes
nil)
       (after
"(foo)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-42 ()
 (let ((before
"(foo
  ) bar")
       (changes
nil)
       (after
"(foo
  ) bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-43 ()
 (let ((before
"(foo
  ); comment")
       (changes
nil)
       (after
"(foo)
  ; comment"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-44 ()
 (let ((before
"[(foo
  )] bar")
       (changes
nil)
       (after
"[(foo
  )] bar"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-45 ()
 (let ((before
"(foo [bar (|...] baz)")
       (changes
nil)
       (after
"(foo [bar (...] baz)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-46 ()
 (let ((before
"(foo [bar (]| baz)])")
       (changes
nil)
       (after
"(foo [bar (] baz)])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-47 ()
 (let ((before
"[... (foo [bar ]| baz]  ...)]")
       (changes
nil)
       (after
"[... (foo [bar ] baz]  ...)]"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-48 ()
 (let ((before
"(let [{:keys |foo bar]} my-map])")
       (changes
nil)
       (after
"(let [{:keys foo bar]} my-map])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-49 ()
 (let ((before
"(a (b (c))| d) e)")
       (changes
nil)
       (after
"(a (b (c)) d) e"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-50 ()
 (let ((before
"(a (b (c(|) d) e)")
       (changes
nil)
       (after
"(a (b (c() d) e))"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-51 ()
 (let ((before
"(f [x (a (b c(|) d) y] g)")
       (changes
nil)
       (after
"(f [x (a (b c() d) y] g)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-52 ()
 (let ((before
"(foo
  bar)| baz) qux")
       (changes
nil)
       (after
"(foo
  bar) baz) qux"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-53 ()
 (let ((before
"(foo
  [bar
   bar)| baz
   bar])")
       (changes
nil)
       (after
"(foo
  [bar
   bar) baz
   bar])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-54 ()
 (let ((before
"(foo
  [bar]
|bar) baz")
       (changes
nil)
       (after
"(foo
  [bar]
bar) baz"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-55 ()
 (let ((before
"(foo
 [bar]
  |bar) baz")
       (changes
nil)
       (after
"(foo
 [bar]
  bar) baz"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-56 ()
 (let ((before
"(foo
 [bar
 bar]) baz")
       (changes
nil)
       (after
"(foo
 [bar
 bar]) baz"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-57 ()
 (let ((before
"(foo bar ;|)")
       (changes
nil)
       (after
"(foo bar) ;)"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-58 ()
 (let ((before
"(let [x 1
      y 2;|])")
       (changes
nil)
       (after
"(let [x 1
      y 2]);])"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-59 ()
 (let ((before
"(|")
       (changes
nil)
       (after
"()"))
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-60 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-61 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-62 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-63 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
(ert-deftest parinfer-indent-64 ()
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
    (should (equal (simulate-parinfer-in-another-buffer before "indent" changes)
                   after))))
