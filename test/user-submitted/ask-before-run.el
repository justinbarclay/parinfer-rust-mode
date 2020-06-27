;; Issue #11
;; Steps to recreate:
;; 1. Open buffer
;; 2. enable parinfer-rust-mode
;; 3. It should prompt the user to see if it should be enabled, because it would change the indentation of the buffer
;; Expected state:
;;     (defvar a
;;       '(:a 1
;;       :c 2))

(defvar a
  '(:a 1
  :c 2))
