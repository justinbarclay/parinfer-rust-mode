;;; parinfer-rust-helper.el --- Helper functions for parinfer-rust-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; Version: 0
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Package-Requires: ((emacs "25.1"))

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


;;; Commentary:
;; An assortment of helper functions and ports of functions from Emacs
;; 27+ to support older versions of Emacs

;;; Code:
(require 'url)
(require 'cl-lib)

(defconst ask-to-download "Could not find the parinfer-rust library, would you like to automatically download it from github?")
(defconst outdated-version "You are using a parinfer-rust library that is not compatible with this file, would you like to download the appropriate file from github?")
(defcustom parinfer-rust-auto-download-p nil "Automatically download the latest version of parinfer-rust from github."
  :type 'boolean
  :group 'parinfer-rust-mode)

(defvar parinfer-rust--test-p nil "Boolean value for running tests in the current buffer.")

(defun parinfer-rust--check-for-library (supported-version library-location lib-name)
  "Check for the existence of the parinfer-rust library. If it
can't be found it offers to download it for the user."
  (when (and (not (file-exists-p library-location)) ;; Using when here instead of unless so we can early exit this if file does exist
             (or
              parinfer-rust-auto-download-p
              (and (boundp parinfer-rust--test-p)
                   parinfer-rust--test-p)
              (yes-or-no-p ask-to-download)))
    (parinfer-rust--download-from-github supported-version library-location lib-name)))

;; This function has a problem: Emacs can't reload dynamic libraries. This means that if we download a new library the user has to restart Emacs.
(defun parinfer-rust--check-version (supported-version current-version library-location lib-name)
  "Check compatability between parinfer-rust-mode and the parinfer-rust library.
If it is not compatible, offer to download the file for the user."
  (when (and current-version
             (not (string=
                   current-version
                   supported-version))
             (and
              (not (bound-and-true-p parinfer-rust--test-p))
              (yes-or-no-p outdated-version)))
    (parinfer-rust--download-from-github supported-version library-location lib-name)
    (message "A new version has been downloaded, you will need to reload Emacs for the changes to take effect.")))

(defun parinfer-rust--download-from-github (parinfer-rust-version
                                            library-location
                                            lib-name)
  "Downloads parinfer-rust to LIBRARY-LOCATION and gives it the name LIB-NAME.
Uses PARINFER-RUST-VERSION to download a compatible version of the library."
  (if (executable-find "curl")
      (progn
        (unless (file-directory-p (file-name-directory library-location)) (make-directory (file-name-directory library-location)))
        (shell-command (format "curl -L %s -o %s"
                               (format "https://github.com/justinbarclay/parinfer-rust/releases/download/v%s/%s" parinfer-rust-version lib-name)
                               library-location))
        (message "Installing %s v%s to %s" lib-name parinfer-rust-version library-location))
    (message "Unable to download parinfer-rust library because curl is not on $PATH")))

(defun parinfer-rust--is-active-minor-mode (minor-mode-maybe)
  "Return non-nil if MINOR-MODE-MAYBE is active in the current buffer."
  (cl-reduce (lambda (acc mode)
               (or
                (and (boundp mode)
                     (symbol-value mode)
                     (eq mode minor-mode-maybe))
                acc))
             minor-mode-list
             :initial-value nil))

(defun parinfer-rust--detect-troublesome-modes ()
  "Check to see if a list of troublesome modes are enabled in `current-buffer'.
If the user does not disable these modes then it may cause bugs
or crashes"
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

;; Helper functions for dealing with parinfer and emacs
(defmacro local-bound-and-true (var)
  "Return non-nil if VAR is locally bound and true."
  `(and (local-variable-if-set-p (quote ,var)) ,var))

(defun parinfer-rust--get-cursor-x ()
  "Return the current x coordinate of the cursor."
  (- (point) (point-at-bol)))

(defun parinfer-rust--get-cursor-line ()
  "Return the parinfer compatible line number of the cursor."
  (- (line-number-at-pos) 1))

(defun parinfer-rust--bound-number (text num)
  "Bounds NUM to be within range of TEXT."
  (let ((max (length text)))
    (cond ((< num 0) 0)
          ((> num max) max)
          ('t num))))

(provide 'parinfer-rust-helper)
;;; parinfer-rust-helper.el ends here
