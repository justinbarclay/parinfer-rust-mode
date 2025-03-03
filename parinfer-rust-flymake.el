;;; parinfer-rust-flymake.el --- Flymake integration for parinfer-rust-mode   -*- lexical-binding: t; -*-

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
;;

;;; Code:

(require 'flymake)

(eval-when-compile
  (defvar parinfer-rust--error nil))

(defvar parinfer-rust--error nil)

(defun parinfer-rust-flymake (report-fn &rest _args)
  (funcall report-fn
           (when-let ((error parinfer-rust--error))
             (list
              (let ((loc (flymake-diag-region
                          (current-buffer)
                          (+ 1 (plist-get error :line_no))
                          (+ 1 (plist-get error :x)))))
                (flymake-make-diagnostic
                 (current-buffer)
                 (car loc)
                 (cdr loc)
                 :error
                 (plist-get error :message)))))))

(defun parinfer-rust-setup-flymake ()
  (add-hook 'flymake-diagnostic-functions 'parinfer-rust-flymake nil t))

(add-hook 'parinfer-rust-mode-hook 'parinfer-rust-setup-flymake)

(provide 'parinfer-rust-flymake)
;;; parinfer-rust-flymake.el ends here
