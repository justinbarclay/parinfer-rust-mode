;;; parinfer-rust-helper.el --- Helper functions for parinfer-rust-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024  Justin Barclay

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
;; 27+ to support older versions of Emacs

;;; Code:
(eval-when-compile
  (declare-function parinfer-rust-mode-enable "parinfer-rust-mode")
  (defvar parinfer-rust--mode)
  (defvar parinfer-rust-dim-parens)
  (defvar parinfer-rust-buffer-replace-strategy)
  (defvar parinfer-rust-mode))
(require 'url)

(defcustom parinfer-rust-troublesome-modes
  '(electric-pair-mode hungry-delete-mode global-hungry-delete-mode indent-tabs-mode)
  "Modes that may conflict when run alongside `parinfer-rust-mode'.

To disable checking for troublesome modes set this to an empty
list."
  :type '(repeat symbol)
  :group 'parinfer-rust-mode)

(defcustom  parinfer-rust-disable-troublesome-modes nil
  "Disables troublesome modes without prompting the user.

Troublesome modes are listed in `parinfer-rust-troublesome-modes'.
Set this to non-nil to disable troublesome modes without prompting."
  :type 'boolean
  :group 'parinfer-rust-mode)

(defconst parinfer-rust--ask-to-download "Could not find the parinfer-rust library, would you like to automatically download it from github?")
(defconst parinfer-rust--outdated-version "You are using a parinfer-rust library that is not compatible with this file, would you like to download the appropriate file from github?")

(defvar  parinfer-rust--download-url "https://github.com/justinbarclay/parinfer-rust-emacs/releases/download/v%s/%s"
  "The url to download the parinfer-rust library from.

This should be a format string that takes two arguments, the
first is the version of the library and the second is the name of
the library.")

(defun parinfer-rust--check-for-library (supported-versions
                                         library-location
                                         lib-name
                                         auto-download)
  "Check for the existence of the parinfer-rust library.

If SUPPORTED-VERSIONS can't be found in LIBRARY-LOCATION offers to
download LIB-NAME for the user. Automatically downloads if
AUTO-DOWNLOAD is supplied or parinfer-rust runs in test mode,
otherwise will promt user. Return non-nil if the parinfer-rust
library was downloaded."
  (if (file-exists-p library-location)
      t
    (when (or
           auto-download
           (parinfer-rust--test-p)
           (yes-or-no-p parinfer-rust--ask-to-download))
      (parinfer-rust--download-from-github (car supported-versions)
                                           ;; This is a hold over because I am lazy. I've stuctured
                                           ;; this data such that my version is the first one in the
                                           ;; list
                                           library-location lib-name)
      t)))
;; This function has a problem: Emacs can't reload dynamic libraries. This means that if we download
;; a new library the user has to restart Emacs.
(defun parinfer-rust--check-version (supported-versions current-version library-location lib-name)
  "Check compatibility between `parinfer-rust-mode' and parinfer-rust library.

If SUPPORTED-VERSIONS is not compatible with CURRENT-VERSION,
offer to download the LIB-NAME to LIBRARY-LOCATION."
  (cond
   ((and current-version
           (not (member-ignore-case
                 current-version
                 supported-versions))
           (and
            (not (parinfer-rust--test-p))
            (yes-or-no-p parinfer-rust--outdated-version)))
    (progn
     (parinfer-rust--download-from-github (car supported-versions) library-location lib-name)
     (message "A new version has been downloaded, you will need to reload Emacs for the changes to take effect.")))
   ((string= "0.4.3" current-version)
    (display-warning '(parinfer-rust-mode)
                     "parinfer-rust-mode now relies on a fork of parinfer-rust and has dropped support for v0.4.3.\nPlease go to https://github.com/justinbarclay/parinfer-rust/discussions/9 to find out more."))
   (t 't)))

(defun parinfer-rust--download-from-github (parinfer-rust-version
                                            library-location
                                            lib-name)
  "Downloads parinfer-rust to LIBRARY-LOCATION and gives it the name LIB-NAME.

Uses PARINFER-RUST-VERSION to download a compatible version of the library."
  (if (executable-find "curl")
      (progn
        (unless (file-directory-p (file-name-directory library-location))
          (make-directory (file-name-directory library-location) t))
        (shell-command
         (format "curl -L %s -o %s"
                 (format parinfer-rust--download-url
                         parinfer-rust-version
                         lib-name)
                 (expand-file-name library-location)))
        (message "Installing %s v%s to %s" lib-name parinfer-rust-version library-location))
    (warn "Unable to download parinfer-rust library because curl is not on $PATH")))

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
  "Check to see if a list of troublesome modes are enabled in `current-buffer`.

If the user does not disable these modes then it may cause bugs or crashes"
  (let ((warning-list))
    (dolist (mode (if (eq parinfer-rust-buffer-replace-strategy
                          'fast)
                      (cons 'flyspell-mode parinfer-rust-troublesome-modes)
                    parinfer-rust-troublesome-modes))
      (when (parinfer-rust--is-active-minor-mode mode)
        (push mode warning-list)))
    (if (and
         warning-list
         (or parinfer-rust-disable-troublesome-modes
          (yes-or-no-p
           (format "The following modes may cause issues with parinfer-rust, do you want to disable them? Mode(s): %s?"
                   (mapconcat (lambda (sym) (symbol-name sym)) warning-list ", ")))))
        (dolist (mode warning-list)
          (apply mode '(-1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paren Dimming
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parinfer can make it apparent which parens are going to be inferred
;; by dimming parens
(defface parinfer-rust-dim-parens
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Parinfer dim paren face."
  :group 'parinfer-rust-mode)

(defun parinfer-rust--dim-parens-fontify-search (limit)
  "Search for closing parens at the end of lines.

This search is bound to occur before LIMIT."
  (let ((result nil)
        (finish nil)
        (bound (+ (point) limit)))
    (while (not finish)
      (if (re-search-forward "\\s)" bound t)
          (when (and (= 0 (string-match-p
                           "\\s)*$"
                           (buffer-substring-no-properties (point) (line-end-position))))
                     (not (eq (char-before (1- (point))) 92)))
            (setq result (match-data)
                  finish t))
        (setq finish t)))
    result))

(defun parinfer-rust--dim-parens-refresh ()
  "If font-lock is available rerun to cover any change."
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defun parinfer-rust--dim-parens ()
  "Apply paren dimming if appropriate."
  (if (and parinfer-rust-mode
           (not (string-equal parinfer-rust--mode "paren"))
           parinfer-rust-dim-parens)
      (font-lock-add-keywords
       nil '((parinfer-rust--dim-parens-fontify-search . 'parinfer-rust-dim-parens)))
    (font-lock-remove-keywords
     nil '((parinfer-rust--dim-parens-fontify-search . 'parinfer-rust-dim-parens))))
  (parinfer-rust--dim-parens-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to make working with Emacs and parinfer nicer

(defun parinfer-rust--test-p ()
  "Return non-nil if running in a test environment.

Parinfer needs to tweak some behavior of parinfer based on test
mode to better emulate users."
  (and (getenv "PARINFER_RUST_TEST")
       (string= (downcase (getenv "PARINFER_RUST_TEST"))
                "true")))

(defmacro parinfer-rust--local-bound-and-true (var)
  "Return non-nil if VAR is locally bound and true."
  `(and (local-variable-if-set-p (quote ,var)) ,var))

(defun parinfer-rust--get-cursor-x ()
  "Return the current x coordinate of the cursor."
  (- (point) (line-beginning-position)))

(defun parinfer-rust--get-cursor-line ()
  "Return the parinfer compatible line number of the cursor."
  (- (line-number-at-pos) 1))

(defun parinfer-rust--reposition-cursor (point-x line-number)
  "Move the cursor to the new LINE-NUMBER and POINT-X column."
  (let* ((new-line (- line-number (parinfer-rust--get-cursor-line)))
         (new-x (- point-x (parinfer-rust--get-cursor-x))))
    (unless (= new-line 0)
      (forward-line new-line))
    (unless (= new-x 0)
      (forward-char new-x))))

(defun parinfer-rust--bound-number (text num)
  "Bounds NUM to be within range of TEXT."
  (let ((max (length text)))
    (cond ((< num 0) 0)
          ((> num max) max)
          (t num))))

(defun parinfer-rust--defer-loading (&rest _)
  "Defer loading of `parinfer-rust-mode' until the buffer is in focus."
  ;; This is a parinfer enabled buffer that started in the background and has now been moved to the foreground
  (when (and parinfer-rust-mode
             (eq (current-buffer)
                 (window-buffer (selected-window))))
    (remove-hook 'window-selection-change-functions #'parinfer-rust--defer-loading t)
    (parinfer-rust-mode-enable)))

;; Disable fill column warning only for this buffer to enable long strings of text without
;; having to do a weird mapconcat.
;; Local Variables:
;; elisp-lint-ignored-validators: ("fill-column" "checkdoc")
;; package-lint-main-file: "parinfer-rust-mode.el"
;; End:

(provide 'parinfer-rust-helper)
;;; parinfer-rust-helper.el ends here
