;;; test-helper.el --- Provides a test harness that simulates a user for parinfer -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; Package-Requires: ((emacs "25"))
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

;;; Commentary:

;; Helper functions for running ERT for parinfer-rust-mode

;;; Code:
;; This is copy and pasted because we need this information before parinfer-rust-mode runs

(require 'parinfer-rust-mode)

(defvar-local parinfer-rust--test-no-cursor nil "A global variable for indicating that the current test doesn't have a cursor in it. Used in conjunction with parinfer-rust--capture-changes")
(defvar-local parinfer-rust--test-has-no-prev-cursor nil "A global variable for indicating that the current test doesn't have a cursor in it. Used in conjunction with parinfer-rust--capture-changes")
(defvar-local parinfer-rust--test-line-no nil "The line change that the current change/replace is happening on")
(defvar-local paringer-rust--in-debug t "Tell parinfer-rust-mode to print out it's debug information to a file")
(defvar-local remove-first-line nil "A flag to let our test harness to remove the first line in a file, because we inserted one")
(defvar parinfer-result-string nil "Result of running a test on parinfer")

;; Don't prompt for permission to modify buffer
;; We're running tests, prompting will break things
(setq parinfer-rust-check-before-enable nil)


;; (when (not (fboundp 'replace-region-contents))) ;; This function does not exist in Emacs <27

(defun replace-region-contents (beg end replace-fn
                                    &optional max-secs max-costs)
  "Replace the region between BEG and END using REPLACE-FN.
REPLACE-FN runs on the current buffer narrowed to the region.  It
should return either a string or a buffer replacing the region.

The replacement is performed using `replace-buffer-contents'
which also describes the MAX-SECS and MAX-COSTS arguments and the
return value.

Note: If the replacement is a string, it'll be placed in a
temporary buffer so that `replace-buffer-contents' can operate on
it.  Therefore, if you already have the replacement in a buffer,
it makes no sense to convert it to a string using
`buffer-substring' or similar."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((repl (funcall replace-fn)))
        (if (bufferp repl)
            (replace-buffer-contents repl max-secs max-costs)
          (let ((source-buffer (current-buffer)))
            (with-temp-buffer
              (insert repl)
              (let ((tmp-buffer (current-buffer)))
                (set-buffer source-buffer)
                (replace-buffer-contents tmp-buffer)))))))))

(defun move-cursor-to-previous-position ()
  (setq-local inhibit-modification-hooks t) ;; we don't need to track this change
  (if (search-forward "@" nil t) ;; If we find a cursor replace our cursor with that one
      (delete-char -1)
    (setq-local parinfer-rust--test-has-no-prev-cursor t))
  (setq-local inhibit-modification-hooks nil))

(defun move-cursor-to-current-position ()
  (setq-local inhibit-modification-hooks t) ;; we don't need to track this change
  (if (search-forward "|" nil t) ;; If we find a cursor replace our cursor with that one
      (delete-char -1)
    (setq-local parinfer-rust--test-no-cursor t))
  (setq-local inhibit-modification-hooks nil))

(defun reverse-changes-in-buffer (change)
  "Revert a list of CHANGE to the specified area of the buffer."
  (with-no-warnings
    (goto-line (+ 1 (cdr (assoc 'lineNo change))))) ;; This is normally bad, but because we're just doing this in a test
  (forward-char (cdr (assoc 'x change)))            ;; and we need to go to the line specified by the current change
  (replace-region-contents (point)
                           (+ (point) (length (cdr (assoc 'newText change))))
                           (lambda (&rest _args) (cdr (assoc 'oldText change)))))

(defun apply-changes-in-buffer (change)
  "Given a list of CHANGE apply to the specified area of the buffer."
  (with-no-warnings
    (goto-line (+ 1 (cdr (assoc 'lineNo change)))))
  (forward-char (cdr (assoc 'x change)))
  (setq-local parinfer-rust--test-line-no (line-number-at-pos))
  (replace-region-contents (point)
                           (+ (point) (length (cdr (assoc 'oldText change))))
                           (lambda (&rest _args) (cdr (assoc 'newText change))))
  (setq-local parinfer-rust--test-line-no nil))

;; Shadow capture-changes for a test friendly version
(defun parinfer-rust--generate-options (old-options changes)
  "Capture the current buffer state and it's associated meta information needed to execute parinfer"
  (if (not (and (parinfer-rust--test-p) ;; If we're in test mode and no cursor is present don't
                parinfer-rust--test-no-cursor ;; capture this information because it causes tests to fail
                parinfer-rust--test-has-no-prev-cursor))
      (let* ((cursor-x (when (not parinfer-rust--test-no-cursor)
                         (parinfer-rust--get-cursor-x)))
             (cursor-line (when (not parinfer-rust--test-no-cursor)
                            (parinfer-rust--get-cursor-line)))
             (options (parinfer-rust-new-options
                       cursor-x
                       cursor-line
                       nil
                       old-options
                       changes)))
        (setq-local parinfer-rust--changes nil)
        options)
    (parinfer-rust-new-options
     nil
     nil
     nil
     old-options
     changes)))
;; Shadow function form parinfer-rust-mode because it executes buffer before everything is set-up in some test cases
(define-minor-mode parinfer-rust-mode
  "A simpler way to write lisps"
  :lighter " parinfer"
  :init-value nil
  :keymap parinfer-rust-mode-map
  (if parinfer-rust-enabled
      (parinfer-rust-mode-disable)
    (progn
      (parinfer-rust--check-version parinfer-rust-supported-versions
                                    (parinfer-rust-version)
                                    parinfer-rust-library
                                    parinfer-rust--lib-name)
      (parinfer-rust-mode-enable)
      ;; Disable checks for deferral and do not run --execute on initialization
      ;; this breaks a lot of test because they expect the buffer to be in a specific state
      )))

(defun simulate-parinfer-in-another-buffer--without-changes (test-string mode)
  "Run parinfer on buffer using text and cursor position
extracted from the json-alist."
  (when (get-buffer "*parinfer-tests*") (kill-buffer "*parinfer-tests*"))
  (save-mark-and-excursion ;; This way we automatically get our point saved
    (let ((current (current-buffer))
          (new-buf (get-buffer-create "*parinfer-tests*"))) ;; We need this
      (switch-to-buffer new-buf)
      (setq-local parinfer-rust--test-no-cursor nil)
      (setq-local paringer-rust--in-debug t)
      (setq-local remove-first-line nil)
      (insert test-string)
      (goto-char 0)
      (setq-local parinfer-rust--mode mode)
      (move-cursor-to-previous-position)
      (when (not parinfer-rust--test-has-no-prev-cursor)
        (setq parinfer-rust--previous-options (parinfer-rust--generate-options (parinfer-rust-make-option)
                                                                               (parinfer-rust-make-changes))))
      (move-cursor-to-current-position)
      (parinfer-rust--execute)
      (when remove-first-line ;; if we created a new line in move-cursor-current-position
        (progn                 ;; remove it
          (goto-char 0)
          (kill-line)
          (setq remove-first-line nil)))
      (setq parinfer-result-string (buffer-string)) ;; Save the string before we kill our current buffer
      (switch-to-buffer current)
      (kill-buffer new-buf)))
  parinfer-result-string)

(defun simulate-parinfer-in-another-buffer--with-changes (test-string mode &optional changes)
  "Run parinfer on buffer using text and cursor position
extracted from the json-alist."
  (when (get-buffer "*parinfer-tests*") (kill-buffer "*parinfer-tests*"))
  (save-mark-and-excursion ;; This way we automatically get our point saved
    (let ((current (current-buffer))
          (new-buf (get-buffer-create "*parinfer-tests*"))) ;; We need this
      (switch-to-buffer new-buf)
      (setq-local parinfer-rust--test-no-cursor nil)
      (setq-local paringer-rust--in-debug t)
      (setq-local remove-first-line nil)
      (insert test-string)
      (when changes
        (progn
          (mapc
           'reverse-changes-in-buffer
           (reverse changes))))
      (goto-char 0)
      (move-cursor-to-previous-position)
      (parinfer-rust-mode)
      (setq-local parinfer-rust--mode mode)
      (when changes
        (mapc
         'apply-changes-in-buffer
         changes))
      (move-cursor-to-current-position)
      (parinfer-rust--execute)   ;; Parinfer execute doesn't run after apply-changes so we have to call in manually
      (parinfer-rust-mode)
      (when remove-first-line
        (progn
          (setq-local inhibit-modification-hooks t) ;; we don't need to track this change
          (goto-char 0)
          (kill-line)
          (setq remove-first-line nil)
          (setq-local inhibit-modification-hooks nil)))
      (setq parinfer-result-string (buffer-string)) ;; Save the string before we kill our current buffer
      (switch-to-buffer current)
      (kill-buffer new-buf)))
  parinfer-result-string)

(defun simulate-parinfer-in-another-buffer--with-commands (test-string mode commands &optional setup)
  (when (get-buffer "*parinfer-tests*") (kill-buffer "*parinfer-tests*"))
  (let ((current (current-buffer))
        (new-buf (get-buffer-create "*parinfer-tests*")))
    (switch-to-buffer new-buf)
    (when setup
      (mapc (lambda (command)
              (apply command nil))
            setup))
    (insert test-string)
    (setq parinfer-rust--mode mode)
    (parinfer-rust-mode)
    (cl-loop for command-set in commands do
             (let ((lineNo (plist-get (car command-set) :lineNo))
                   (column (plist-get (car command-set) :column))
                   (command (cadr command-set)))
               (goto-char (point-min))
               (forward-line (1- lineNo))
               (forward-char column)
               (apply command nil)
               (parinfer-rust--execute)))
    (setq parinfer-result-string (buffer-substring-no-properties (point-min) (point-max))) ;; Save the string before we kill our current buffer
    (switch-to-buffer current)
    (kill-buffer new-buf))
  parinfer-result-string)

(defun simulate-parinfer-in-another-buffer (test-string mode &optional changes)
  "Run parinfer on buffer using text and cursor position
extracted from the json-alist."
  (if changes
      (simulate-parinfer-in-another-buffer--with-changes test-string mode changes)
    (simulate-parinfer-in-another-buffer--without-changes test-string mode)))
;;; test-helper.el ends here
