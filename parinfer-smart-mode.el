;;; parinfer-smart-mode.el --- parinfer-smart-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; Keywords: extensions
;; Version: 0.1.0
(defvar parinfer-smart--lib-name nil "System dependent library name")
(cond
 ((eq system-type 'darwin) (setq parinfer-smart--lib-name "parinfer-rust-mac.so"))
 ((eq system-type 'gnu/linux) (setq parinfer-smart--lib-name "parinfer-rust-linux.so")))

(require 'parinfer-rust parinfer-smart--lib-name)
(require 'subr-x)
(require 'cl-lib)
(require 'json)

;; Local Vars
(defvar-local parinfer-enabled-p nil "Tracks whether parinfer has been enabled")
(defvar-local parinfer-smart--debug-p nil "Whether the request response to the rust plugin or not")
(defvar-local parinfer-smart--mode "paren" "Set the mode that parinfer is runs when managing your paranthesis. Either 'paren', 'indent', or 'smart'")
(defvar-local parinfer-smart--previous-change nil "The last set of changes recorded in the buffer")
(defvar-local parinfer-smart--current-changes nil "A list of changes made in between running of parinfer-smart--execute")
(defvar-local parinfer-smart--test-p nil "Are we in test mode?")
(defvar-local parinfer-smart--disable nil "Temporarily disable parinfer")
(defvar-local parinfer-smart--undo-p nil "Tracks if the user has recently run the undo command")

;; Helper functions
(defun parinfer-smart--get-cursor-x ()
  (- (point) (point-at-bol)))

(defun parinfer-smart--set-previous-change (change)
  (setq-local parinfer-smart--previous-change change))

(defun parinfer-smart--get-cursor-line ()
  (- (line-number-at-pos) 1))

(defun parinfer-smart--get-line-changes (region-start region-end length old-buffer-text)
  "Get associated with a current line including, line number, cursor position on line, old text, and new text"
  (let ((lineNo (line-number-at-pos region-start parinfer-smart--test-p)) ;; If we're in test-mode we want the absolute position otherwise relative is fine
        (x (save-excursion
             (save-restriction
               (widen)
               (goto-char region-start)
               (parinfer-smart--get-cursor-x))))
        (region-start (- region-start 0)))
    (list
     (cons "lineNo" (- lineNo 1))
     (cons "x" x)
     (cons "oldText" (if old-buffer-text
                         (substring-no-properties old-buffer-text
                                                  region-start
                                                  (+ region-start length)) ;; We don't use region-end because region-end represents the end of change of the new text
                       ""))                                            ;; so instead we calculate from the start of both strings to the length of changes made
     (cons "newText" (buffer-substring-no-properties region-start region-end)))))

(defun parinfer-smart--reposition-cursor (point-x line-number)
  (let* ((new-line (- line-number (parinfer-smart--get-cursor-line)))
         (new-x (- point-x (parinfer-smart--get-cursor-x))))
    (when (not (= new-line 0))
      (forward-line new-line))
    (when (not (= new-x 0))
      (forward-char new-x))))

(defun parinfer-smart--track-changes (region-start region-end length)
  "Track changes to the buffer."
  (if parinfer-smart--disable
      nil
    (let* ((old-buffer-text (when (local-variable-if-set-p 'parinfer-smart--previous-change)
                              (gethash 'text parinfer-smart--previous-change)))
           (current-change (parinfer-smart--get-line-changes region-start region-end length old-buffer-text)))
      (if parinfer-smart--current-changes
          (setq-local parinfer-smart--current-changes (cons current-change parinfer-smart--current-changes))
        (setq-local parinfer-smart--current-changes (list current-change))))))

(defun parinfer-smart--capture-changes (&optional old-options changes)
  "Capture the current change information needed by Parinfer."
  (let ((table (make-hash-table))
        (options-table (make-hash-table)))
    (puthash 'text
             (buffer-substring-no-properties (point-min) (point-max)) ;; Needs to be substring to remove all the properties from the document
             table)
    (puthash 'options
             (progn
               (puthash 'cursorX (parinfer-smart--get-cursor-x)
                        options-table)
               (puthash 'cursorLine (parinfer-smart--get-cursor-line)
                        options-table)
               (puthash 'prevCursorX (when old-options
                                       (gethash 'cursorX old-options))
                        options-table)
               (puthash 'prevCursorLine (when old-options
                                          (gethash 'cursorLine old-options))
                        options-table)
               (puthash 'selectionStartLine nil
                        options-table)
               (when changes (puthash 'changes changes
                                      options-table))
               options-table)
             table)
    (puthash 'mode ;; Test only on smart mode
             parinfer-smart--mode
             table)
    (setq-local parinfer-smart--current-changes nil)
    (setq-local parinfer-smart--previous-change table)
    table))

(defun parinfer-smart-switch-mode ()
  "Switch to a different Parinfer mode. Either: indent, smart, or paren"
  (interactive)
  (setq-local parinfer-smart--mode
              (completing-read "Choose parinfer mode:"
                               (remove parinfer-smart--mode
                                       (list "indent" "smart" "paren"))
                               nil
                               t)))

(defun append-string-to-file (s filename)
  "Helper function to write to a file"
  (with-temp-buffer
    (insert s)
    (write-region (point-min) (point-max) filename t)))

(defun parinfer-smart--debug-to-file (current-change response)
  "Print parinfer debug information to a file"
  (append-string-to-file "Request:\n" "parinfer.txt")
  (append-string-to-file (json-encode current-change) "parinfer.txt")
  (append-string-to-file "\nResponse:\n" "parinfer.txt")
  (append-string-to-file (json-encode response) "parinfer.txt")
  (append-string-to-file "\n" "parinfer.txt"))

(defun parinfer-smart--execute (&rest _args)
  "Run parinfer in the current buffer"
  (interactive)
  (if (or parinfer-smart--disable ;; Don't run Execute if disabled by user or right after an undo
          parinfer-smart--undo-p)
      (setq-local parinfer-smart--undo-p nil)
    (let* ((change-list (if parinfer-smart--current-changes
                            (puthash
                             'changes
                             parinfer-smart--current-changes
                             (make-hash-table))
                          ()))
           (old-options (when (and (local-variable-if-set-p 'parinfer-smart--previous-change)
                                   parinfer-smart--previous-change)
                          (gethash 'options parinfer-smart--previous-change)))
           (current-change (progn
                             (parinfer-smart--capture-changes old-options
                                                              change-list)))
           (response (json-read-from-string
                      (parinfer-rust-execute
                       (json-encode-hash-table
                        current-change))))
           (replacement-string (cdr (assoc 'text response)))
           (error-p (cdr (assoc 'error response))))
      (setq-local inhibit-modification-hooks 't)
      (when (and (local-variable-if-set-p 'parinfer-smart--debug-p)
                 parinfer-smart--debug-p)
        (parinfer-smart--debug-to-file current-change response))
      (if error-p
          (message (format "%s"
                           (cdr (assoc 'message error-p))))
        (if (not (string-equal (gethash 'text current-change) replacement-string)) ;; This stops Emacs from flickering when scrolling
            (progn
              (save-mark-and-excursion ;; This way we automatically get our point saved
                (let ((current (current-buffer))
                      (new-buf (get-buffer-create "*parinfer*")))
                  (switch-to-buffer new-buf)
                  (insert replacement-string)
                  (switch-to-buffer current)
                  (replace-buffer-contents new-buf)
                  (kill-buffer new-buf)))
              (when-let ((new-x (cdr (assoc 'cursorX response)))
                         (new-line (cdr (assoc 'cursorLine response))))
                (parinfer-smart--reposition-cursor new-x new-line)))))
      (if parinfer-smart--undo-p (setq-local parinfer-smart--undo-p nil))
      (with-no-warnings ;; TODO: Fix this issue
        (setq-local inhibit-modification-hooks nil)))))

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
  (setq-local parinfer-smart--previous-change (parinfer-smart--capture-changes nil nil))
  (setq-local parinfer-enabled-p 't)
  (setq-local parinfer-smart--current-changes nil)
  (setq-local parinfer-smart--mode "paren")
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
