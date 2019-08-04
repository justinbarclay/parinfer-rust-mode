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

;;; Commentary:

;; Manage your parenthesis automatically based on whitespace.

;;; Code:

(defvar parinfer-smart--lib-name nil "System dependent library name for parinfer-smart-mode")
(cond
 ((eq system-type 'darwin) (setq parinfer-smart--lib-name "parinfer-rust-mac.so"))
 ((eq system-type 'gnu/linux) (setq parinfer-smart--lib-name "parinfer-rust-linux.so")))

(defcustom parinfer-smart-library (locate-user-emacs-file parinfer-smart--lib-name)
  "The location to store the parinfer-rust library."
  :type 'file
  :group 'parinfer-smart-mode)

(unless (file-exists-p parinfer-smart-library)
  (url-copy-file (concat "https://github.com/justinbarclay/parinfer-smart-mode/raw/master/" parinfer-smart--lib-name)
                 parinfer-smart-library))

(require 'parinfer-rust parinfer-smart-library)
(require 'subr-x)
(require 'cl-lib)
(require 'url)

;; Local Vars
(defvar-local parinfer-enabled-p nil "Tracks if parinfer has been enabled")
(defvar-local parinfer-smart--debug-p nil "When enabled, outputs the response input and output of the parinfer response to a file") ;; TODO: Set a specific file in emacs home directory
(defvar-local parinfer-smart--mode "paren" "The current mode that parinfer running under to managing your paranthesis. Either 'paren', 'indent', or 'smart'")
(defvar-local parinfer-smart--previous-options nil "The last set of record of changes and meta information of changes in the buffer")
(defvar-local parinfer-smart--current-changes nil "The set of currently tracked changes since parinfer-rust--execute was ran")
(defvar-local parinfer-smart--test-p nil "Predicate to determine if we're in test mode or not. We need to tweak some behavior of parinfer based on test mode to better emulate users.")
(defvar-local parinfer-smart--disable nil "Temporarily disable parinfer")
(defvar-local parinfer-smart--undo-p nil "Tracks if the user has recently run the undo command")

;; Helper functions
(defun parinfer-smart--get-cursor-x ()
  (- (point) (point-at-bol)))

(defun parinfer-smart--get-cursor-line ()
  (- (line-number-at-pos) 1))

(defun parinfer-smart--reposition-cursor (point-x line-number)
  (let* ((new-line (- line-number (parinfer-smart--get-cursor-line)))
         (new-x (- point-x (parinfer-smart--get-cursor-x))))
    (when (not (= new-line 0))
      (forward-line new-line))
    (when (not (= new-x 0))
      (forward-char new-x))))

(defmacro local-bound-and-true (var)
  "Helper macro for determining if a variable is locally set and if it's assigned a value "
  `(and (local-variable-if-set-p (quote ,var)) ,var))

(defun parinfer-smart--make-change (region-start region-end length old-buffer-text)
  (let ((lineNo (- (line-number-at-pos region-start parinfer-smart--test-p)
                   1)) ;; If we're in test-mode we want the absolute position otherwise relative is fine
        (x (save-excursion
             (save-restriction
               (widen)
               (goto-char region-start)
               (parinfer-smart--get-cursor-x))))
        (region-start (- region-start 0)))
    (parinfer-rust-new-change lineNo
                              x
                              (if old-buffer-text
                                  (substring-no-properties old-buffer-text
                                                           region-start
                                                           (+ region-start length)) ;; We don't use region-end because region-end represents the end of change of the new text
                                "")
                              (buffer-substring-no-properties region-start region-end))))

(defun parinfer-smart--track-changes (region-start region-end length)
  "Add the current change into a list of changes from when `parinfer-rust--execute` was last run."
  (if parinfer-smart--disable
      nil
    (let* ((old-buffer-text (when (local-variable-if-set-p 'parinfer-smart--previous-buffer-text)
                              parinfer-smart--previous-buffer-text))
           (current-change (parinfer-smart--make-change region-start region-end length old-buffer-text)))
      (if parinfer-smart--current-changes
          (parinfer-rust-add-change
           parinfer-smart--current-changes
           current-change)
        (progn
          (setq-local parinfer-smart--current-changes (parinfer-rust-make-changes))
          (parinfer-rust-add-change
           parinfer-smart--current-changes
           current-change))))))

(defun parinfer-smart--generate-options (old-options changes)
  "Capture the current buffer state and it's associated meta information needed to execute parinfer"
  (let ((options (parinfer-rust-new-options
                  (parinfer-smart--get-cursor-x)
                  (parinfer-smart--get-cursor-line)
                  nil
                  old-options
                  changes)))
    (setq-local parinfer-smart--current-changes nil)
    options))

(defun parinfer-smart--execute (&rest _args)
  "Run parinfer in the current buffer"
  (interactive)
  (if (or parinfer-smart--disable ;; Don't run if disabled by user or right after an undo
          parinfer-smart--undo-p)
      (setq-local parinfer-smart--undo-p nil)
    (progn
      (setq-local parinfer-smart--previous-buffer-text (buffer-substring-no-properties (point-min) (point-max)))
      (let* ((old-options (or (local-bound-and-true parinfer-smart--previous-options)
                              (parinfer-rust-make-option)))
             (changes (or (local-bound-and-true parinfer-smart--current-changes)
                          (parinfer-rust-make-changes)))
             (options (parinfer-smart--generate-options old-options
                                                        changes))
             (request (parinfer-rust-make-request parinfer-smart--mode
                                                  (buffer-substring-no-properties (point-min) (point-max))
                                                  options))
             (answer (parinfer-rust-execute request))
             (replacement-string (parinfer-rust-get-answer answer "text" nil))
             (error-p (parinfer-rust-get-answer answer "error" "message")))
        ;; (cdr (assoc 'error response))))) ;; Disabled until I add a hashmap like function for destructuring errors
        (setq-local inhibit-modification-hooks 't) ;; We don't want other hooks to run while we're modifying the buffer
                                                   ;; that could lead to weird and unwanted behavior
        (when (and (local-variable-if-set-p 'parinfer-smart--debug-p)
                   parinfer-smart--debug-p)
          (parinfer-rust-debug "./parinfer-rust-debug.txt" options answer))
        (if error-p
            (message (format "%s" error-p))
          (if (not (string-equal parinfer-smart--previous-buffer-text replacement-string)) ;; This stops Emacs from flickering when scrolling
              (progn
                (save-mark-and-excursion ;; This way we automatically get our point saved
                  (let ((current (current-buffer))
                        (new-buf (get-buffer-create "*parinfer*")))
                    (switch-to-buffer new-buf)
                    (insert replacement-string)
                    (switch-to-buffer current)
                    (replace-buffer-contents new-buf)
                    (kill-buffer new-buf))))))
                ;; (when-let ((new-x (cdr (assoc 'cursorX response)))
                ;;            (new-line (cdr (assoc 'cursorLine response))))
                ;;   (parinfer-smart--reposition-cursor new-x new-line)))))
        (when parinfer-smart--undo-p (setq-local parinfer-smart--undo-p nil))
        (with-no-warnings ;; TODO: Fix this issue
          (setq-local inhibit-modification-hooks nil))))))

(defun parinfer-smart-switch-mode ()
  "Switch to a different Parinfer mode. Either: indent, smart, or paren"
  (interactive)
  (setq-local parinfer-smart--mode
              (completing-read "Choose parinfer mode:"
                               (remove parinfer-smart--mode
                                       (list "indent" "smart" "paren"))
                               nil
                               t)))

(defun parinfer-smart--track-undo (&rest _)
  "Used to track when an undo action is performed, so we can temporarily disable parinfer"
  (message "undoing")
  (setq-local parinfer-smart--undo-p 't))

(defun parinfer-smart-toggle-debug ()
  (if parinfer-enabled-p
      (setq parinfer-smart--debug-p nil)
    (setq parinfer-smart--debug-p t)))

(defun parinfer-smart-mode-enable ()
  "Enable Parinfer"
  (setq-local parinfer-smart--previous-options (parinfer-smart--generate-options
                                                (parinfer-rust-make-option)
                                                (parinfer-rust-make-changes)))
  (setq-local parinfer-smart--previous-buffer-text (buffer-substring-no-properties (point-min) (point-max))) ; We need to store thiqs separately because it's not being tracked with options anymore
  (setq-local parinfer-enabled-p 't)
  (setq-local parinfer-smart--current-changes nil)
  (setq-local parinfer-smart--mode "indent")

  (advice-add 'undo :before 'parinfer-smart--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :before 'parinfer-smart--track-undo))
  (add-hook 'after-change-functions 'parinfer-smart--track-changes t t)
  (add-hook 'post-command-hook 'parinfer-smart--execute t t))

(defun parinfer-smart-mode-disable ()
  "Disable Parinfer"
  (advice-remove 'undo 'parinfer-smart--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :before 'parinfer-smart--track-undo))
  (remove-hook 'after-change-functions 'parinfer-smart--track-changes t)
  (remove-hook 'post-command-hook 'parinfer-smart--execute t)
  (setq-local parinfer-enabled-p nil))

(defun parinfer-smart-toggle-disable ()
  "Temporarily, stop parinfer from tracking what you're doing or from executing parinfer-smart--execute"
  (interactive)
  (if parinfer-smart--disable
      (setq-local parinfer-smart--disable nil)
    (setq-local parinfer-smart--disable 't)))

;;;###autoload

(defvar parinfer-smart-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c s") 'parinfer-smart-switch-mode)
    (define-key m (kbd "C-c d") 'parinfer-smart-toggle-disable)
    m)
  "Keymap for `parinfer-smart-mode'.")

(define-minor-mode parinfer-smart-mode
  "A simpler way to write lisps"
  :lighter " parinfer"
  :init-value nil
  :keymap parinfer-smart-mode-map
  (if parinfer-enabled-p
      (parinfer-smart-mode-disable)
    (parinfer-smart-mode-enable)))

(provide 'parinfer-smart-mode)
;;; parinfer-smart-mode.el ends here
