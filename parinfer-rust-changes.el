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
  (declare-function parinfer-rust--execute "parinfer-rust-mode" t t)
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
(require 'track-changes)
(require 'subr-x)

(defvar-local parinfer-rust--changes '()
  "The current set of unprocessed changes.")

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

(defun parinfer-rust--fetch-changes (id)
  "Fetch change for current buffer using signal ID."
  (track-changes-fetch id
                       (lambda (start end before)
                         (if parinfer-rust--disable
                             nil
                           ;; If we're in test-mode we want the absolute position otherwise
                           ;; relative is fine
                           (let ((lineNo (- (line-number-at-pos start t) 1))
                                 (x (save-excursion
                                      (save-restriction
                                        (widen)
                                        (goto-char start)
                                        (parinfer-rust--get-cursor-x)))))
                             (push (list 'lineNo lineNo
                                         'x x
                                         'length (length before)
                                         'before-text before
                                         'after-text (buffer-substring-no-properties start end))
                                   parinfer-rust--changes))))))

(defun parinfer-rust--changes-signal (id &optional distance)
  "Signal change for ID with optional DISTANCE."
  (parinfer-rust--fetch-changes id)
  (if distance
      ;; We're still in the middle of changes, but they're "far",
      ;; so record the past changes and keep going.
      nil
    (parinfer-rust--execute)))

;; Local Variables:
;; package-lint-main-file: "parinfer-rust-mode.el"
;; End:
(provide 'parinfer-rust-changes)
;;; parinfer-rust-changes.el ends here
