;; Issue #8
;; Steps to recreate
;; 1. move cursor to (1 . 2)
;; 2. Run delet-indentation
;; expected new states
;; (def thing {:abc 1
;;             :xyz 2})
(def thing
  {:abc 1
   :xyz 2})
