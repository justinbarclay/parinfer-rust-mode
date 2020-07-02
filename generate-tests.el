;;; generate-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: t; -*-
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

;; Generates test files for each "mode" in Parinfer based on the JSON
;; files supplied be the reference implementation of Parinfer.  Which can
;; be found at:
;; https://github.com/shaunlebron/parinfer/tree/master/lib/test/cases

;;; Code:
(defvar-local json-tests (list (cons "indent" "./test/cases/indent-mode.json")
                               (cons "paren" "./test/cases/paren-mode.json")
                               (cons "smart" "./test/cases/smart-mode.json")))

(defun insert-cursor-position (json-alist)
  "Insert a | to represent the position of the cursor in the test text."
  (let* ((options (cdr (assoc 'options json-alist)))
         (cursorLine (cdr (assoc 'cursorLine options)))
         (cursorX (cdr (assoc 'cursorX options)))
         (prevCursorLine (cdr (assoc 'prevCursorLine options)))
         (prevCursorX (cdr (assoc 'prevCursorX options))))
    (with-temp-buffer
      (insert (cdr (assoc 'text json-alist)))
      (when (and cursorLine cursorX)
        (progn
          (goto-line (+ 1 cursorLine))
          (forward-char cursorX)
          (insert "|")))
      (when (and prevCursorLine prevCursorX)
        (progn
          (goto-line (+ 1 prevCursorLine))
          (forward-char prevCursorX)
          (insert "@")))
      (buffer-string))))

(defun generate-parinfer-test (mode index before after &optional changes)
  "Generates an ERT style test."
  (with-temp-buffer
    (insert (format
             "(ert-deftest parinfer-%s-%s ()
 (let ((before
%s)
       (changes
%s)
       (after
%s))
    (should (equal (simulate-parinfer-in-another-buffer before \"%s\" changes)
                   after))))\n"
             mode
             index
             (prin1-to-string before)
             (when changes (concat "'" (prin1-to-string changes)))
             (prin1-to-string after)
             mode))
    (buffer-string)))

(defun generate-parinfer-tests (mode test-file)
  "Generates tests for a specified mode"
  (let* ((json-array-type 'list)
         (index 0)
         (filename (format "%s-parinfer-tests.el" mode))
         (tests (json-read-from-string
                 (with-temp-buffer
                   (insert-file-contents test-file)
                   (buffer-string)))))
    (append-string-to-file "(load-file \"./test-helpers.el\")\n" filename)
    (mapcar
     (lambda (test)
       (append-string-to-file
        (generate-parinfer-test mode
                                index
                                (insert-cursor-position test)
                                (cdr (assoc 'text
                                            (cdr (assoc 'result test))))
                                (cdr (assoc 'changes
                                            (cdr (assoc 'options test)))))
        filename)
       (incf index))
     tests)
    t))

(defun generate-all-tests ()
  "Generate all mode based tests for parinfer-smart-mode."
  (mapcar ;; Clean up old files
   (lambda (file) (delete-file file))
   '("./paren-parinfer-tests.el" "./smart-parinfer-tests.el" "./indent-parinfer-tests.el"))
  (mapcar
   (lambda (mode-and-file)
     (generate-parinfer-tests (car mode-and-file)
                              (cdr mode-and-file)))
   json-tests))

;; (generate-all-tests)
