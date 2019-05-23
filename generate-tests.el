;;; generate-tests.el --- Auto generates tests based on a json file  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;;; Commentary:
;;; Code:
(defvar-local json-tests (list (cons "indent" "./test/cases/indent-mode.json")
                               (cons "paren" "./test/cases/paren-mode.json")
                               (cons "smart" "./test/cases/smart-mode.json")))

(defun insert-cursor-position (json-alist)
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
    't))

(defun generate-all-tests ()
  (mapcar ;; Clean up old files
   (lambda (file) (delete-file file))
   '("./paren-parinfer-tests.el" "./smart-parinfer-tests.el" "./indent-parinfer-tests.el"))
  (mapcar
   (lambda (mode-and-file)
     (generate-parinfer-tests (car mode-and-file)
                              (cdr mode-and-file)))
   json-tests))

;; (generate-all-tests)
