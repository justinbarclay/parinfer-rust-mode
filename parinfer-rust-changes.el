;;; parinfer-rust-changes.el --- parinfer-rust-changes   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary: A small library for converting Emacs changes to parinfer changes

;; The idea with merging changes is that if two changes occur in the
;; same line, start-region and temporarily are next to each other that
;; they can be merged into one change.
;;

;;; Commentary:
;; Isolating change tracking to it's own file because it's still a volatile idea with room to grow
;; and change.

;; Change tracking works thusly:
;; 1. parinfer-rust-mode tracks all changes reported by the after after-change-hook.

;; 2. During each change it will capture the state of the current and previous regions of text,
;; along with some meta state we use to determine if two changes should be merged.

;; 3. After it has finished recording state it update the parinfer-rust--previous-buffer-text local
;; variable to ensure the next time a change event is fired the state of previous text is always
;; correct.

;; 4. When parinfer-rust--execute is called it will merge change events that occur sequentially in
;; time and at the same starting coordinates. This merges delete events together and helps create a
;; more minimized change list that parinfer-rust can understand.

;; 5. Once changes have been merged, they get transformed into a parinfer-rust change struct.

;; 6. Finally they get passed into parinfer-rust and may god have mercy on their souls.
;;; Code:

(eval-when-compile
  (declare-function parinfer-rust-new-change "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-make-changes "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-add-change "ext:parinfer-rust" t t)
  (defvar parinfer-rust-library
    (concat user-emacs-directory "parinfer-rust/"
            (cond
             ((eq system-type 'darwin) "parinfer-rust-darwin.so")
             ((eq system-type 'gnu/linux) "parinfer-rust-linux.so"
              "parinfer-rust-linux.so"))))
  (defvar parinfer-rust--previous-buffer-text)
  (defvar parinfer-rust--disable))

(require 'parinfer-rust parinfer-rust-library t)

(require 'parinfer-rust-helper)
(require 'subr-x)

(defvar-local parinfer-rust--changes '()
  "The current set of unprocessed changes.")

(defun parinfer-rust--merge-changes (change-a change-b)
  "Return change list from CHANGE-A and CHANGE-B.

Return the set of changes that covers the greatest region, the
lowest start value, highest end value, and merge the before and
after text for two changes."
  (let ((start (if (< (plist-get change-a 'start)
                      (plist-get change-b 'start))
                   (plist-get change-a 'start)
                 (plist-get change-b 'start)))
        (end (if (> (plist-get change-a 'end)
                    (plist-get change-b 'end))
                 (plist-get change-a 'end)
               (plist-get change-b 'end)))
        (length (+ (plist-get change-a 'length)
                   (plist-get change-b 'length))))
    (list
     'lineNo (plist-get change-a 'lineNo)
     'x (plist-get change-a 'x)
     'start start
     'end end
     'length length
     'before-text (string-join (list (plist-get change-a 'before-text)
                                     (plist-get change-b 'before-text)))
     'after-text (string-join (list (plist-get change-a 'after-text)
                                    (plist-get change-b 'after-text)))
     'group t)))

(defun parinfer-rust--combine-changes (change-list)
  "Iterate over CHANGE-LIST and look for change.

Changes that operate beside each other sequentially in time and
on similar regions of texts."
  (let ((sorted-changes (reverse change-list))
        (consolidated-changes '())
        (previous-line nil)
        (previous-start nil))
    (dolist (change sorted-changes consolidated-changes)
      ;; Look for text
      (if (and (equal previous-line
                      (plist-get change 'lineNo))
               (equal previous-start
                      (plist-get change 'start)))
          (setq consolidated-changes
                (cons
                 (parinfer-rust--merge-changes (car consolidated-changes) change)
                 (cdr consolidated-changes)))
        (setq consolidated-changes (cons change consolidated-changes)))
      (setq previous-start (plist-get change 'start))
      (setq previous-line (plist-get change 'lineNo)))))

;; Good for future tests
;; (setq some-changes
;;       '((lineNo 7 x 10 start 170 end 171 length 0 before-text "" after-text " " group nil)
;;         (lineNo 7 x 10 start 170 end 170 length 2 before-text "  " after-text "" group nil)
;;         (lineNo 7 x 10 start 170 end 170 length 1 before-text "\n" after-text ""  group nil)))

;; (assert
;;  (equal
;;   '(lineNo 7 x 10 start 170 end 170 before-text "\n  " after-text "" length 3 group t)
;;   (parinfer-rust--merge-changes
;;    '(lineNo 7 x 10 start 170 end 170 length 1 before-text "\n" after-text ""  group nil)
;;    '(lineNo 7 x 10 start 170 end 170 length 2 before-text "  " after-text "" group nil))))

;; (assert
;;  (equal
;;   (parinfer-rust--combine-changes some-changes)
;;   '((lineNo 7 x 10 start 170 end 171 before-text "\n  " after-text " " length 3 group t))))

(defun parinfer-rust--get-before-and-after-text (start end length)
  "Builds before and after change text using START, END, and LENGTH.

Uses on `parinfer-rust--previous-buffer-text' and
`current-buffer' text to generate info."
  (let* ((previous-text parinfer-rust--previous-buffer-text)
         (old-region-end (parinfer-rust--bound-number previous-text (+ start length -1)))
         (old-region-start (parinfer-rust--bound-number previous-text (- start 1))))
    (list
     (if previous-text
         (substring-no-properties previous-text
                                  old-region-start
                                  old-region-end)
       "")
     (buffer-substring-no-properties start end))))

(defun parinfer-rust--build-changes (change-list)
  "Convert CHANGE-LIST to a list of change structs for parinfer-rust."
  (let ((changes (parinfer-rust-make-changes)))
    (cl-loop for change in change-list do
             (let* ((current-change (parinfer-rust-new-change (plist-get change 'lineNo)
                                                              (plist-get change 'x)
                                                              (plist-get change 'before-text)
                                                              (plist-get change 'after-text))))
               (parinfer-rust-add-change
                changes
                current-change)))
    (setq-local parinfer-rust--changes '())
    changes))

(defun parinfer-rust--track-changes (start end length)
  "Track  change in buffer using START, END, and LENGTH.

Uses START, END, and Length to capture the state from the
previous buffer and current buffer."
  (if parinfer-rust--disable
      nil
    ;; If we're in test-mode we want the absolute position otherwise relative is fine
    (let ((lineNo (- (line-number-at-pos start t)
                     1))
          (x (save-excursion
               (save-restriction
                 (widen)
                 (goto-char start)
                 (parinfer-rust--get-cursor-x))))
          (changes (parinfer-rust--get-before-and-after-text start end length)))
      (push (list 'lineNo lineNo
                  'x x
                  'start start
                  'end end
                  'length length
                  'before-text (car changes)
                  'after-text (cadr changes)
                  'group nil)
            parinfer-rust--changes))
    (setq parinfer-rust--previous-buffer-text
          (save-restriction
            (widen)
            (buffer-substring-no-properties (point-min) (point-max))))))

;; Local Variables:
;; package-lint-main-file: "parinfer-rust-mode.el"
;; End:
(provide 'parinfer-rust-changes)
;;; parinfer-rust-changes.el ends here
