;; Issue #9
;; Steps to recreate
;; 1. move cursor to (5 . 3)
;; 2. Run fill-paragraph
;; Expected state:
;; (ns some-namespace.core
;;   (:import [java.util.concurrent Executors])
;;   (:require [clojure.string :as string])
;;
;; (defn foo
;;   "pretty long docsting that will be affected by `fill-paragraph`
;;   function"
;;   [x]
;;   x)

(ns some-namespace.core
  (:import [java.util.concurrent Executors])
  (:require [clojure.string :as string]))

(defn foo
  "pretty long docsting that will be affected by `fill-paragraph` function"
  [x]
  x)
