;;; parinfer-smart-mode.el --- parinfer-smart-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-smart-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Keywords: lisps

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


(defvar ask-to-download "Could not find the parinfer-rust library, would you like to automatically download it from github?")
(defvar outdated-version "You are using a parinfer-rust library that is not compatible with this file, would you like to download the appropriate file from github?")

(defun check-for-library (library-location)
  "Checks for the existence of the parinfer-rust library and if it can't be found it offers to download it for the user"
  (unless (and (file-exists-p library-location)
               (yes-or-no-p ask-to-download))
    (download-from-github library-location)))

(defun check-parinfer-rust-version ()
  "Checks to see if parinfer-rust version library currently installed is compatible with parinfer-smart-helper.
   If it is not compatible, offer to download the file for the user"
  (when (and (parinfer-rust-version)
             (not (equalp "0"
                          supported-parinfer-rust-version))
             (yes-or-no-p outdated-version))
    (download-from-github)))

(defun download-from-github (library-location)
  (url-copy-file (concat "https://github.com/justinbarclay/parinfer-smart-mode/raw/master/" parinfer-smart--lib-name)
                 library-location))

(defun check-for-version)
(provide 'parinfer-helper)
