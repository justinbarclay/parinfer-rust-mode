;;; parinfer-helper.el --- parinfer-helper   -*- lexical-binding: t; -*-

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

;;; Commentary: A file for functions not strictly related to making parinfer work

;; A helper library to download a precompiled library for you

;;; Code:
(require 'url)

(defconst ask-to-download "Could not find the parinfer-rust library, would you like to automatically download it from github?")
(defconst outdated-version "You are using a parinfer-rust library that is not compatible with this file, would you like to download the appropriate file from github?")
(defvar parinfer-rust--auto-download-p nil "Automatically download the latest version of parinfer-rust from github")
(defvar parinfer-rust--test-p nil "Boolean value for running tests in the current buffer")

(defun parinfer-rust--check-for-library (supported-version library-location lib-name)
  "Checks for the existence of the parinfer-rust library and if it can't be found it offers to download it for the user"
  (when (and (not (file-exists-p library-location)) ;; Using when here instead of unless so we can early exit this if file does exist
             (or
              parinfer-rust--auto-download-p
              (and (boundp parinfer-rust--test-p)
                   parinfer-rust--test-p)
              (yes-or-no-p ask-to-download)))
    (parinfer-rust--download-from-github supported-version library-location lib-name)))

;; This function has a problem: Emacs can't reload dynamic libraries. This means that if we download a new library the user has to restart Emacs.
(defun parinfer-rust--check-version (supported-version current-version library-location lib-name)
  "Checks to see if parinfer-rust version library currently installed is compatible with parinfer-rust-helper.
   If it is not compatible, offer to download the file for the user"
  (when (and current-version
             (not (string=
                   current-version
                   supported-version))
             (and
              (not (bound-and-true-p parinfer-rust--test-p))
              (yes-or-no-p outdated-version)))
    (parinfer-rust--download-from-github supported-version library-location lib-name)
    (message "A new version has been downloaded, you will need to reload Emacs for the changes to take effect.")))


(defun parinfer-rust--download-from-github (parinfer-rust-version library-location lib-name)
  (if (executable-find "curl")
      (progn
        (unless (file-directory-p (file-name-directory library-location)) (make-directory (file-name-directory library-location)))
        (shell-command (format "curl -L %s -o %s"
                               (format "https://github.com/eraserhd/parinfer-rust/releases/download/v%s/%s" parinfer-rust-version lib-name)
                               library-location))
        (message "Installing %s v%s to %s" lib-name parinfer-rust-version library-location))
    (message "Unable to download parinfer-rust library because curl is not on $PATH")))

(defun parinfer-rust--is-active-minor-mode (minor-mode-maybe)
  "Returns true if MINOR-MODE-MAYBE is active in the current buffer."
  (cl-reduce (lambda (acc mode)
               (or
                (and (boundp mode)
                     (symbol-value mode)
                     (eq mode minor-mode-maybe))
                acc))
             minor-mode-list
             :initial-value nil))

(defun parinfer-rust--detect-troublesome-modes ()
  "Checks to see if a list of troublesome modes are enabled in the same buffer and offers to disables them for the user.
  If the user does not disable these modes then it may cause bugs or crashes"
  (let ((warning-list))
      (dolist (mode '(electric-pair-mode hungry-delete-mode global-hungry-delete-mode))
        (when (parinfer-rust--is-active-minor-mode mode)
          (push mode warning-list)))
      (if (and
           warning-list
           (yes-or-no-p
                (format "The following modes may cause issues with parinfer-rust, do you want to disable them? Mode(s): %s"
                        (mapconcat (lambda (sym) (symbol-name sym)) warning-list ", "))))
          (dolist (mode warning-list)
            (apply mode '(-1))))))

(provide 'parinfer-helper)
;;; parinfer-helper.el ends here
