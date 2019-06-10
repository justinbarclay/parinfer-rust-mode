;;; test-helper.el --- Provides a test harness that simulates a user for parinfer -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Helper functions for running ERT for parinfer-smart-mode

;;; Code:

(require 'parinfer-smart-mode)

(defvar-local parinfer-smart--test-no-cursor nil "A global variable for indicating that the current test doesn't have a cursor in it. Used in conjunction with parinfer-smart--capture-changes")
(defvar-local parinfer-smart--test-has-no-prev-cursor nil "A global variable for indicating that the current test doesn't have a cursor in it. Used in conjunction with parinfer-smart--capture-changes")
(defvar-local parinfer-smart--test-p nil "An indicator that parinfer-smart is being tested.")
(defvar-local parinfer-smart--test-line-no nil "The line change that the current change/replace is happening on")
(defvar-local parinfer-smart--debug-p 't "Tell parinfer-smart-mode to print out it's debug information to a file")
(defvar-local remove-first-line-p nil "A flag to let our test harness to remove the first line in a file, because we inserted one")
(defvar parinfer-result-string nil "Result of running a test on parinfer")



(when (not (fboundp 'replace-region-contents)) ;; This function does not exist in Emacs <27
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
		  (replace-buffer-contents tmp-buffer))))))))))

(defun move-cursor-to-previous-position ()
  (setq-local inhibit-modification-hooks 't) ;; we don't need to track this change
  (if (search-forward "@" nil t) ;; If we find a cursor replace our cursor with that one
      (delete-char -1)
    (setq-local parinfer-smart--test-has-no-prev-cursor 't))
  (setq-local inhibit-modification-hooks nil))

(defun move-cursor-to-current-position ()
  (setq-local inhibit-modification-hooks 't) ;; we don't need to track this change
  (if (search-forward "|" nil t) ;; If we find a cursor replace our cursor with that one
      (delete-char -1)
    (setq-local parinfer-smart--test-no-cursor 't))
  (setq-local inhibit-modification-hooks nil))

(defun reverse-changes-in-buffer (change)
  "Given a list of change it applies the before to the specified area of the buffer"
  (with-no-warnings
    (goto-line (+ 1 (cdr (assoc 'lineNo change))))) ;; This is normally bad, but because we're just doing this in a test
  (forward-char (cdr (assoc 'x change)))           ;; and we need to go to the line specified by the current change
  (replace-region-contents (point)
                           (+ (point) (length (cdr (assoc 'newText change))))
                           (lambda (&rest _args) (cdr (assoc 'oldText change)))))

(defun apply-changes-in-buffer (change)
  "Given a list of change it applies the before to the specified area of the buffer"
  (with-no-warnings
    (goto-line (+ 1 (cdr (assoc 'lineNo change)))))
  (forward-char (cdr (assoc 'x change)))
  (setq-local parinfer-smart--test-line-no (line-number-at-pos))
  (replace-region-contents (point)
                           (+ (point) (length (cdr (assoc 'oldText change))))
                           (lambda (&rest _args) (cdr (assoc 'newText change))))
  (setq-local parinfer-smart--test-line-no nil))


;; Shadow capture-changes for a test friendly version
(defun parinfer-smart--capture-changes (&optional old-options changes)
  (let* ((table (make-hash-table))
         (options-table (make-hash-table)))
    (puthash 'text
             (buffer-substring-no-properties (point-min) (point-max)) ;; Needs to be substring to remove all the properties from the document
             table)
    (puthash 'options
             (progn
               (when (not (and parinfer-smart--test-p ;; If we're in test mode and no cursor is present don't
                               parinfer-smart--test-no-cursor ;; capture this information because it causes tests to fail
                               parinfer-smart--test-has-no-prev-cursor))
                 (puthash 'cursorX (when (not parinfer-smart--test-no-cursor)
                                     (parinfer-smart--get-cursor-x))
                          options-table)
                 (puthash 'cursorLine (when (not parinfer-smart--test-no-cursor)
                                        (parinfer-smart--get-cursor-line))
                          options-table)
                 (puthash 'prevCursorX (when old-options
                                         (gethash 'cursorX old-options))
                          options-table)
                 (puthash 'prevCursorLine (when old-options
                                            (gethash 'cursorLine old-options))
                          options-table)
                 (puthash 'selectionStartLine nil
                          options-table))
               (when changes (puthash 'changes changes
                                      options-table))
               options-table)
             table)
    (puthash 'mode ;; Test only on smart mode
             parinfer-smart--mode
             table)
    (if parinfer-smart--test-no-cursor ;; Turn off so we don't get a false positive next run
        (setq-local parinfer-smart--test-no-cursor nil))
    (setq-local parinfer-smart--previous-change table)
    table))

(defun simulate-parinfer-in-another-buffer--without-changes (test-string mode)
  "Runs parinfer on buffer using text and cursor position extracted from the json-alist"
  (when (get-buffer "*parinfer-tests*") (kill-buffer "*parinfer-tests*"))
  (save-mark-and-excursion ;; This way we automatically get our point saved
    (let ((current (current-buffer))
          (new-buf (get-buffer-create "*parinfer-tests*"))) ;; We need this
      (switch-to-buffer new-buf)
      (setq-local parinfer-smart--test-no-cursor nil)
      (setq-local parinfer-smart--test-p 't)
      (setq-local remove-first-line-p nil)
      (insert test-string)
      (goto-char 0)
      (setq-local parinfer-smart--mode mode)
      (move-cursor-to-previous-position)
      (when (not parinfer-smart--test-has-no-prev-cursor)
        (parinfer-smart--capture-changes))
      (move-cursor-to-current-position)
      (parinfer-smart--execute)
      (when remove-first-line-p ;; if we created a new line in move-cursor-current-position
        (goto-char 0)
        (kill-line)
        (setq remove-first-line-p nil))
      (setq parinfer-result-string (buffer-string)) ;; Save the string before we kill our current buffer
      (switch-to-buffer current)
      (kill-buffer new-buf)))
  parinfer-result-string)

(defun simulate-parinfer-in-another-buffer--with-changes (test-string mode &optional changes)
  "Runs parinfer on buffer using text and cursor position extracted from the json-alist"
  (when (get-buffer "*parinfer-tests*") (kill-buffer "*parinfer-tests*"))
  (save-mark-and-excursion ;; This way we automatically get our point saved
    (let ((current (current-buffer))
          (new-buf (get-buffer-create "*parinfer-tests*"))) ;; We need this
      (switch-to-buffer new-buf)
      (setq-local parinfer-smart--test-no-cursor nil)
      (setq-local parinfer-smart--test-p 't)
      (setq-local remove-first-line-p nil)
      (insert test-string)
      (when changes
        (mapc
         'reverse-changes-in-buffer
         (reverse changes)))
      (goto-char 0)
      (move-cursor-to-previous-position)
      (parinfer-smart-mode)
      (setq-local parinfer-smart--mode mode)
      (when changes
        (mapc
         'apply-changes-in-buffer
         changes))
      (move-cursor-to-current-position)
      (parinfer-smart--execute)   ;; Parinfer execute doesn't run after apply-changes so we have to call in manually
      (parinfer-smart-mode)
      (when remove-first-line-p
        (setq-local inhibit-modification-hooks 't) ;; we don't need to track this change
        (goto-char 0)
        (kill-line)
        (setq remove-first-line-p nil)
        (setq-local inhibit-modification-hooks nil))
      (setq parinfer-result-string (buffer-string)) ;; Save the string before we kill our current buffer
      (switch-to-buffer current)
      (kill-buffer new-buf)))
  parinfer-result-string)

(defun simulate-parinfer-in-another-buffer (test-string mode &optional changes)
  "Runs parinfer on buffer using text and cursor position extracted from the json-alist"
  (if changes
      (simulate-parinfer-in-another-buffer--with-changes test-string mode changes)
    (simulate-parinfer-in-another-buffer--without-changes test-string mode)))


(provide 'test-helper)
;;; test-helper.el ends here
