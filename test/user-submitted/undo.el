;; Issue #10
;; Steps to recreate:
;; 1. Move cursor to (0 . 0)
;; 2. Press space 5 times
;; 3. Run undo once
;; Expected state:
;;     (defvar a
;;       '(:a 1
;;         :c 2))
(defvar a
  '(:a 1
    :c 2))
