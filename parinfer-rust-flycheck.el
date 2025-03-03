;;; parinfer-rust-flycheck.el --- Flycheck integration for parinfer-rust-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;; An assortment of helper functions and ports of functions from Emacs

;;; Code:
(eval-when-compile
  (declare-function flycheck-error-new-at "flycheck")
  (declare-function flycheck-define-generic-checker "flycheck")
  (defvar parinfer-rust--error nil))

(require 'flycheck nil t)

(defvar parinfer-rust--error nil)

(defun parinfer-rust--flycheck-start (checker callback)
  (funcall callback 'finished
           (when-let ((error parinfer-rust--error))
             (list
              (flycheck-error-new-at
               (+ 1 (plist-get error :line_no))
               (+ 1 (plist-get error :x))
               'error
               (plist-get error :message)
               :id (plist-get error :name)
               :checker checker)))))

(flycheck-define-generic-checker 'parinfer-rust
  "A checker for parinfer-rust."
  :start 'parinfer-rust--flycheck-start
  :verify (lambda (&rest args)
            (list (flycheck-verification-result-new
                   :label "parinfer-rust-mode"
                   :message (if parinfer-rust-mode
                                "enabled"
                              "not enabled"))))
  :modes '(clojure-mode
           clojurec-mode
           clojurescript-mode
           clojure-ts-mode
           clojure-ts-clojurescript-mode
           janet-mode
           common-lisp-mode
           lisp-mode
           racket-mode
           scheme-mode
           lisp-interaction-mode
           emacs-lisp-mode))

(flycheck-add-next-checker 'emacs-lisp 'parinfer-rust)

(add-to-list 'flycheck-checkers 'parinfer-rust t)

(provide 'parinfer-rust-flycheck)
;;; parinfer-rust-flycheck.el ends here
