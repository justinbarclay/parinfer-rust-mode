;;; parinfer-rust-mode.el --- parinfer-rust-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Version: 0.3.0
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

;; Manage your parenthesis automatically based on whitespace.

;;; Code:

;; Need to define these two before parinfer-rust and parinfer-helper are called
(defconst parinfer-rust--lib-name (cond
                                    ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                    ((eq system-type 'gnu/linux) "parinfer-rust-linux.so"))
  "System dependent library name for parinfer-rust-mode")
(defconst parinfer-rust-version "123" "The version of the parinfer-rust library that parinfer-rust-mode was tested against")
(defconst parinfer-rust--mode-types (list "indent" "smart" "paren") "The different modes that parinfer can operate on")

(defvar-local parinfer-rust--test-p (bound-and-true-p parinfer-test-helper) "Predicate to determine if we're in test mode or not. We need to tweak some behavior of parinfer based on test mode to better emulate users.")

(defcustom parinfer-rust-library (locate-user-emacs-file parinfer-rust--lib-name)
  "The location to store or to find the parinfer-rust library."
  :type 'file
  :group 'parinfer-rust-mode)

(require 'parinfer-helper)

(parinfer-rust--check-for-library parinfer-rust-version
                                  parinfer-rust-library
                                  parinfer-rust--lib-name) ;; Check for library and download if necessary
(require 'parinfer-rust parinfer-rust-library)
(require 'subr-x)
(require 'cl)

(parinfer-rust--check-version parinfer-rust-version
                              parinfer-rust-library
                              parinfer-rust--lib-name) ;; Check version and prompt to download latest version if out of date

(defcustom parinfer-rust-preferred-mode "smart"
  "The location to store or to find the parinfer-rust library."
  :type '(radio (const :tag "indent" "indent")
                (const :tag "smart" "smart")
                (const :tag "paren" "paren"))
  :group 'parinfer-rust-mode)


;; Local Vars
(defvar-local parinfer-enabled-p nil "Tracks if parinfer has been enabled")
(defvar-local parinfer-rust--debug-p nil "When enabled, outputs the response input and output of the parinfer response to a file") ;; TODO: Set a specific file in emacs home directory
(defvar-local parinfer-rust--mode "paren" "The current mode that parinfer running under to managing your paranthesis. Either 'paren', 'indent', or 'smart'")
(defvar-local parinfer-rust--previous-options nil "The last set of record of changes and meta information of changes in the buffer")
(defvar-local parinfer-rust--current-changes nil "The set of currently tracked changes since parinfer-rust--execute was ran")
(defvar-local parinfer-rust--disable nil "Temporarily disable parinfer")
(defvar-local parinfer-rust--undo-p nil "Tracks if the user has recently run the undo command")

;; Helper functions
(defun parinfer-rust--get-cursor-x ()
  (- (point) (point-at-bol)))

(defun parinfer-rust--get-cursor-line ()
  (- (line-number-at-pos) 1))

(defun parinfer-rust--reposition-cursor (point-x line-number)
  "Moves the cursor to the new line and column"
  (let* ((new-line (- line-number (parinfer-rust--get-cursor-line)))
         (new-x (- point-x (parinfer-rust--get-cursor-x))))
    (when (not (= new-line 0))
      (forward-line new-line))
    (when (not (= new-x 0))
      (forward-char new-x))))

(defun parinfer-rust--bound-number (text num)
  "Bounds number to be within range of string "
  (let ((max (length text)))
    (cond ((< num 0) 0)
          ((> num max) max)
          ('t num))))

(defmacro local-bound-and-true (var)
  "Helper macro for determining if a variable is locally set and if it's assigned a value "
  `(and (local-variable-if-set-p (quote ,var)) ,var))

(defun parinfer-rust--make-change (region-start region-end length old-buffer-text)
  (let* ((lineNo (- (line-number-at-pos region-start parinfer-rust--test-p)
                    1)) ;; If we're in test-mode we want the absolute position otherwise relative is fine
         (x (save-excursion
              (save-restriction
                (widen)
                (goto-char region-start)
                (parinfer-rust--get-cursor-x)))) ;; We don't use region-end because region-end represents the end of change of the new text
         (old-region-end (parinfer-rust--bound-number old-buffer-text (+ region-start length)))
         (old-region-start (parinfer-rust--bound-number old-buffer-text region-start)))
    (parinfer-rust-new-change lineNo
                              x
                              (if old-buffer-text
                                  (substring-no-properties old-buffer-text
                                                           old-region-start
                                                           old-region-end)
                                "")
                              (buffer-substring-no-properties region-start region-end))))

;; TODO catch args out of range error
(defun parinfer-rust--track-changes (region-start region-end length)
  "Add the current change into a list of changes from when `parinfer-rust--execute` was last run."
  (if parinfer-rust--disable
      nil
    (let* ((old-buffer-text (when (local-variable-if-set-p 'parinfer-rust--previous-buffer-text)
                              parinfer-rust--previous-buffer-text))
           (current-change (parinfer-rust--make-change region-start region-end length old-buffer-text)))
      (if parinfer-rust--current-changes
          (parinfer-rust-add-change
           parinfer-rust--current-changes
           current-change)
        (progn
          (setq-local parinfer-rust--current-changes (parinfer-rust-make-changes))
          (parinfer-rust-add-change
           parinfer-rust--current-changes
           current-change))))))

(defun parinfer-rust--generate-options (old-options changes)
  "Capture the current buffer state and it's associated meta information needed to execute parinfer"
  (let ((options (parinfer-rust-new-options
                  (parinfer-rust--get-cursor-x)
                  (parinfer-rust--get-cursor-line)
                  nil
                  old-options
                  changes)))
    (setq-local parinfer-rust--current-changes nil)
    options))

(defun parinfer-rust--execute (&rest _args)
  "Run parinfer in the current buffer"
  (interactive)
  (if (or parinfer-rust--disable ;; Don't run if disabled by user or right after an undo
          parinfer-rust--undo-p)
      (setq-local parinfer-rust--undo-p nil)
    (progn
      (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties (point-min) (point-max)))
      (let* ((old-options (or (local-bound-and-true parinfer-rust--previous-options)
                              (parinfer-rust-make-option)))
             (changes (or (local-bound-and-true parinfer-rust--current-changes)
                          (parinfer-rust-make-changes)))
             (options (parinfer-rust--generate-options old-options
                                                        changes))
             (request (parinfer-rust-make-request parinfer-rust--mode
                                                  (buffer-substring-no-properties (point-min) (point-max))
                                                  options))
             (answer (parinfer-rust-execute request))
             (replacement-string (parinfer-rust-get-in-answer answer "text"))
             (error-p (parinfer-rust-get-in-answer answer "error")))
        (setq-local inhibit-modification-hooks 't) ;; We don't want other hooks to run while we're modifying the buffer
                                                   ;; that could lead to weird and unwanted behavior
        (when (and (local-variable-if-set-p 'parinfer-rust--debug-p)
                   parinfer-rust--debug-p)
          (parinfer-rust-debug "./parinfer-rust-debug.txt" options answer))
        (if error-p
            (message (format "%s" (parinfer-rust-get-in-error error-p "message")))
          (if (not (string-equal parinfer-rust--previous-buffer-text replacement-string)) ;; This stops Emacs from flickering when scrolling
              (progn
                (save-mark-and-excursion ;; This way we automatically get our point saved
                  (let ((current (current-buffer))
                        (new-buf (get-buffer-create "*parinfer*")))
                    (switch-to-buffer new-buf)
                    (insert replacement-string)
                    (switch-to-buffer current)
                    (replace-buffer-contents new-buf)
                    (kill-buffer new-buf))))))
        (when-let ((new-x (parinfer-rust-get-in-answer answer "cursor_x")) ;; TODO handle errors
                   (new-line (parinfer-rust-get-in-answer answer "cursor_line")))
          (parinfer-rust--reposition-cursor new-x new-line))
        (when parinfer-rust--undo-p (setq-local parinfer-rust--undo-p nil))
        (setq parinfer-rust--previous-options options)
        (with-no-warnings ;; TODO: Fix this issue
          (setq-local inhibit-modification-hooks nil))))))

(defun parinfer-rust-switch-mode ()
  "Switch to a different Parinfer mode. Either: indent, smart, or paren"
  (interactive)
  (setq-local parinfer-rust--mode
              (completing-read "Choose parinfer mode:"
                               (remove parinfer-rust--mode
                                       parinfer-rust--mode-types)
                               nil
                               t)))

(defun parinfer-rust--track-undo (&rest _)
  "Used to track when an undo action is performed, so we can temporarily disable parinfer"
  (setq-local parinfer-rust--undo-p 't))

(defun parinfer-rust-toggle-debug ()
  (if parinfer-enabled-p
      (setq parinfer-rust--debug-p nil)
    (setq parinfer-rust--debug-p t)))

(defun parinfer-rust-mode-enable ()
  "Enable Parinfer"
  (parinfer-rust--detetect-troublesome-modes)
  (setq-local parinfer-rust--previous-options (parinfer-rust--generate-options
                                               (parinfer-rust-make-option)
                                               (parinfer-rust-make-changes)))
  (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties (point-min) (point-max))) ; We need to store thiqs separately because it's not being tracked with options anymore
  (setq-local parinfer-enabled-p 't)
  (setq-local parinfer-rust--current-changes nil)
  (setq-local parinfer-rust--mode "paren")
  (parinfer-rust--execute)
  (setq-local parinfer-rust--mode parinfer-rust-preferred-mode)
  (advice-add 'undo :before 'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :before 'parinfer-rust--track-undo))
  (add-hook 'after-change-functions 'parinfer-rust--track-changes t t)
  (add-hook 'post-command-hook 'parinfer-rust--execute t t))

(defun parinfer-rust-mode-disable ()
  "Disable Parinfer"
  (advice-remove 'undo 'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :before 'parinfer-rust--track-undo))
  (remove-hook 'after-change-functions 'parinfer-rust--track-changes t)
  (remove-hook 'post-command-hook 'parinfer-rust--execute t)
  (setq-local parinfer-enabled-p nil))

(defun parinfer-rust-toggle-disable ()
  "Temporarily, stop parinfer from tracking what you're doing or from executing parinfer-rust--execute"
  (interactive)
  (if parinfer-rust--disable
      (setq-local parinfer-rust--disable nil)
    (setq-local parinfer-rust--disable 't)))

;;;###autoload

(defvar parinfer-rust-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c s") 'parinfer-rust-switch-mode)
    (define-key m (kbd "C-c d") 'parinfer-rust-toggle-disable)
    m)
  "Keymap for `parinfer-rust-mode'.")

(define-minor-mode parinfer-rust-mode
  "A simpler way to write lisps"
  :lighter " parinfer"
  :init-value nil
  :keymap parinfer-rust-mode-map
  (if parinfer-enabled-p
      (parinfer-rust-mode-disable)
    (parinfer-rust-mode-enable)))

(provide 'parinfer-rust-mode)
;;; parinfer-rust-mode.el ends here
