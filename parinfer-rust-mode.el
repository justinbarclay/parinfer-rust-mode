;;; parinfer-rust-mode.el --- Infer your parens away -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Version: 0.7.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp tools

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

;; `parinfer-rust-mode` provides an interface between the `parinfer-rust` library
;; and Emacs.  As such it's primary role is to capture meta information about the
;; buffer and transmit it to the parinfer-rust API.
;;
;; In broad strokes we must:
;; 1. Monitor and record all changes and meta information about changes in the buffer
;; 2. Keep a record of the buffer state before the last time parinfer-rust was run
;; 3. Run parinfer-rust and update the state of the buffer accordingly
;;
;; For a complete list of state that needs to be tracked read:
;; https://github.com/shaunlebron/parinfer/tree/master/lib#api
;; https://github.com/shaunlebron/parinfer/blob/master/lib/doc/integrating.md

;;; Code:

;; Need to define these before parinfer-rust and parinfer-helper are loaded
(defconst parinfer-rust--lib-name (cond
                                   ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                   ((eq system-type 'gnu/linux) "parinfer-rust-linux.so"
                                    "parinfer-rust-linux.so"))
  "System dependent library name for parinfer-rust-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizations
;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom parinfer-rust-auto-download nil
  "Automatically download the latest version of parinfer-rust from github."
  :type 'boolean
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-library (locate-user-emacs-file (concat "parinfer-rust/"
                                                                 parinfer-rust--lib-name))
  "The location to store or to find the parinfer-rust library."
  :type 'file
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-preferred-mode "smart"
  "What mode you want parinfer-rust to start in."
  :type '(radio (const :tag "indent" "indent")
                (const :tag "smart" "smart")
                (const :tag "paren" "paren"))
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-check-before-enable t
  "Have parinfer-rust ask the user if it wants to be enable
`parinfer-rust-mode' if it detects it needs to change the
indentation in the buffer to run."
  :type 'boolean
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-dim-parens t
  "Whether to dim inferred parens in Indent and Smart modes."
  :type 'boolean
  :group 'parinfer-rust-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst parinfer-rust-supported-version "0.4.4-beta" "The version of the parinfer-rust library
that `parinfer-rust-mode' was tested against.")
(defconst parinfer-rust--mode-types (list "indent" "smart" "paren") "The different modes that
parinfer can operate on.")

;; Require helper so we can check for library
(require 'parinfer-rust-helper)

;; Make sure the library is installed at the appropriate location or offer to download it
(parinfer-rust--check-for-library parinfer-rust-supported-version
                                  parinfer-rust-library
                                  parinfer-rust--lib-name
                                  parinfer-rust-auto-download)

(require 'subr-x)
(require 'font-lock)
(require 'parinfer-rust parinfer-rust-library)
(require 'parinfer-rust-changes)

;; Check version and prompt to download latest version if out of date
;; Problem: Emacs can't reload dynamic libraries, which means that if we
;; download a new library the user has to restart Emacs for changes to take effect.
(parinfer-rust--check-version parinfer-rust-supported-version
                              (parinfer-rust-version)
                              parinfer-rust-library
                              parinfer-rust--lib-name)

;; This is a hack around Emacs complex command and scripting system. In some cases
;; parinfer-rust-mode sucks at picking up the correct changes in the buffer, so the
;; `treat-command-as` system is a means to work around `parinfer-rust-mode`'s or Emacs limitations
;; and give hints to `parinfer-rust-mode` for how you want to treat specific commands in
;; `smart-mode`.

(defvar parinfer-rust-treat-command-as
  '((paredit-forward-barf-sexp . "paren")
    (paredit-forward-slurp-sexp . "paren")
    (yank . "paren")
    (counsel-yank-pop . "paren"))
  "A curated list of pairs consisting of a command and the mode the command should be run in.
Ex: '((yank . \"paren\"))")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local State
;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local parinfer-rust-enabled nil "Tracks if parinfer has been enabled")
(defvar-local parinfer-rust--in-debug nil
  "When enabled, outputs the response input and output of the parinfer response to a file")
(defvar-local parinfer-rust--mode "paren"
  "The current mode that parinfer running under to managing your
  paranthesis. Either 'paren', 'indent', or 'smart'")
(defvar-local parinfer-rust--previous-options nil
  "The last set of record of changes and meta information of changes in the buffer")
;; TODO this might be not needed anymore
(defvar-local parinfer-rust--current-changes nil
  "The set of currently tracked changes since parinfer-rust--execute was ran")
(defvar-local parinfer-rust--disable nil "Temporarily disable parinfer")
(defvar-local parinfer-rust--in-undo nil "Tracks if parinfer-rust-mode is within an undo command")
(defvar-local parinfer-rust--previous-buffer-text ""
  "The text in the buffer previous to when parinfer-rust ran last")
(defvar-local parinfer-rust--ignore-post-command-hook nil
  "A hack to not run parinfer-execute after an undo has finished processing")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paren Dimming
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parinfer can make it apparent which parens are going to be inferred
;; by dimming parens
(defface parinfer-rust-dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Parinfer dim paren face."
  :group 'parinfer-rust-mode)

(defun parinfer-rust--dim-parens-fontify-search (limit)
  "Search for closing parens at the end of lines. This search is
bound to occur before LIMIT."
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
  (if (and parinfer-rust-enabled
           (not (string-equal parinfer-rust--mode "paren"))
           parinfer-rust-dim-parens)
      (font-lock-add-keywords
       nil '((parinfer-rust--dim-parens-fontify-search . 'parinfer-rust-dim-parens-face)))
    (font-lock-remove-keywords
     nil '((parinfer-rust--dim-parens-fontify-search . 'parinfer-rust-dim-parens-face))))
  (parinfer-rust--dim-parens-refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parinfer-rust--set-default-state ()
  "Set up parinfer for execution in a default context. Good for
switching modes, after an undo, or when first starting parinfer."
  (setq-local parinfer-rust--previous-options (parinfer-rust--generate-options
                                               (parinfer-rust-make-option)
                                               (parinfer-rust-make-changes)))
  (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties
                                                   (point-min)
                                                   (point-max)))
  (setq-local parinfer-rust--current-changes nil))


;; The idea for this function: 1. is to never run during an undo operation 2. Set a flag to ignore
;; the first post command execution after an undo operation 2 is important because if we undo our
;; last key press and that causes parinfer to modify the buffer we get stuck in a loop of trying to
;; undo things and parinfer redoing them

(defun parinfer-rust--track-undo (orig-func &rest args)
  "Wraps ORIG-FUNC and ARGS in some state tracking for `parinfer-rust-mode'."
  (setq-local parinfer-rust--in-undo t)
  (condition-case-unless-debug err
      (apply orig-func args)
    ;; We want to "Ignore" errors here otherwise the function exits
    ;; and causes the following commands, where we set flags, to be
    ;; ignored
    (error
     (message "%s" (cadr err))
     nil))
  (setq-local parinfer-rust--in-undo nil)
  ;; Always ignore the first post-command-hook run of parinfer after an undo
  (setq-local parinfer-rust--ignore-post-command-hook t)
  (parinfer-rust--set-default-state))

(defun parinfer-rust--execute-change-buffer-p (mode)
  "Return non-nil if running `parinfer-rust--execute' with MODE would change the current buffer."
  (let ((parinfer-rust--mode mode)
        (old-buffer (current-buffer))
        (current-text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert-buffer-substring old-buffer)
      (parinfer-rust--execute)
      (not
       (string= (buffer-substring-no-properties (point-min) (point-max))
                current-text)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfaces for parinfer-rust
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parinfer-rust--generate-options (old-options changes)
  "Capture the current buffer state and it's associated meta information needed to execute parinfer.
Builds a parinfer-rust OPTION struct based on OLD-OPTIONS and CHANGES."
  (let ((options (parinfer-rust-new-options
                  (parinfer-rust--get-cursor-x)
                  (parinfer-rust--get-cursor-line)
                  nil
                  old-options
                  changes)))
    (setq-local parinfer-rust--current-changes nil)
    options))

(defun parinfer-rust--execute (&rest _args)
  "Run parinfer in the current buffer."
  (if (or parinfer-rust--disable ;; Don't run if disabled by user or right after an undo
          parinfer-rust--in-undo
          parinfer-rust--ignore-post-command-hook
          undo-in-progress)
      ;; Do nothing and disable flags
      (when parinfer-rust--ignore-post-command-hook
        (setq-local parinfer-rust--ignore-post-command-hook nil))
    (progn
      (when (not (= 0 (length parinfer-rust--changes)))
        (parinfer-rust--build-changes (parinfer-rust--combine-changes parinfer-rust--changes))
        (setq-local parinfer-rust--changes '()))
      (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties (point-min)
                                                                                      (point-max)))
      (let* ((parinfer-rust--mode
              (if-let ((mode
                        (and (string= "smart" parinfer-rust--mode)
                             (alist-get this-command parinfer-rust-treat-command-as))))
                  (progn
                    ;; By saying a command should run under another mode, we're
                    ;; going to simplify parinfer-rust's behavior and clear all
                    ;; tracked changes so that it can more closely behave to
                    ;; _my_ expectations. Such expectations are that if I'm
                    ;; running most commands in "paren" mode, I want it to
                    ;; behave as if I am running it from a clean slate and not
                    ;; knowing all the changes I just made. That's because by
                    ;; knowing all the state changes it made it might make the
                    ;; wrong choices for similar reasons it would under smart
                    ;; mode, the changes Emacs reports may be different than
                    ;; those parinfer expects.
                    (setq parinfer-rust--current-changes nil)
                    mode)
                parinfer-rust--mode))
             (old-options (or (parinfer-rust--local-bound-and-true parinfer-rust--previous-options)
                              (parinfer-rust-make-option)))
             (changes (or (parinfer-rust--local-bound-and-true parinfer-rust--current-changes)
                          (parinfer-rust-make-changes)))
             (options (parinfer-rust--generate-options old-options
                                                       changes))
             (request (parinfer-rust-make-request parinfer-rust--mode
                                                  (buffer-substring-no-properties (point-min)
                                                                                  (point-max))
                                                  options))
             (answer (parinfer-rust-execute request))
             (replacement-string (parinfer-rust-get-in-answer answer "text"))
             (error-p (parinfer-rust-get-in-answer answer "error")))
        ;; We don't want other hooks to run while we're modifying the buffer
        ;; that could lead to weird and unwanted behavior
        (setq-local inhibit-modification-hooks t)
        (when (and (local-variable-if-set-p 'parinfer-rust--in-debug)
                   parinfer-rust--in-debug)
          (parinfer-rust-debug "./parinfer-rust-debug.txt" options answer))
        (if error-p
            (message (format "%s" (parinfer-rust-print-error error-p))) ;; TODO handle errors
          ;; This stops Emacs from flickering when scrolling
          (if (not (string-equal parinfer-rust--previous-buffer-text replacement-string))
              (progn
                (save-mark-and-excursion ;; This way we automatically get our point saved
                  (let ((current (current-buffer))
                        (new-buf (get-buffer-create "*parinfer*")))
                    (switch-to-buffer new-buf)
                    (insert replacement-string)
                    (switch-to-buffer current)
                    (replace-buffer-contents new-buf)
                    (kill-buffer new-buf))))))
        (when-let ((new-x (parinfer-rust-get-in-answer answer "cursor_x"))
                   (new-line (parinfer-rust-get-in-answer answer "cursor_line")))
          (parinfer-rust--reposition-cursor new-x new-line))
        (setq parinfer-rust--previous-options options)
        (with-no-warnings ;; TODO: Should not need with-no-warnings function
          (setq-local inhibit-modification-hooks nil))))))

;; Interactive or functions meant to be called by user
(defun parinfer-rust-toggle-debug ()
  "Turn on debug for parinfer. This will create a text file in the current directory."
  (interactive)
  (if parinfer-rust--in-debug
      (setq parinfer-rust--in-debug nil)
    (setq parinfer-rust--in-debug t)))

(defun parinfer-rust-mode-enable ()
  "Enable Parinfer."
  (setq-local parinfer-rust-enabled t)
  (parinfer-rust--detect-troublesome-modes)
  (parinfer-rust--set-default-state)
  ;; As per spec, always run paren on a buffer before entering any mode
  ;; this ensure that all functions are aligned to a point parinfer won't
  ;; change the meaning of code
  (setq-local parinfer-rust--mode "paren")
  (parinfer-rust--execute)
  (setq-local parinfer-rust--mode parinfer-rust-preferred-mode)
  (advice-add 'undo :around 'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :around 'parinfer-rust--track-undo))
  (add-hook 'after-change-functions 'parinfer-rust--track-changes t t)
  (add-hook 'post-command-hook 'parinfer-rust--execute t t)
  (parinfer-rust--dim-parens))

(defun parinfer-rust-mode-disable ()
  "Disable Parinfer."
  (advice-remove 'undo 'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-remove 'undo-tree-undo 'parinfer-rust--track-undo))
  (remove-hook 'after-change-functions 'parinfer-rust--track-changes t)
  (remove-hook 'post-command-hook 'parinfer-rust--execute t)
  (setq-local parinfer-rust-enabled nil)
  (parinfer-rust--dim-parens))

(defun parinfer-rust-toggle-disable ()
  "Temporarily disable parinfer-rust.

This includes stopping tracking of all changes."
  (interactive)
  (if parinfer-rust--disable
      (setq-local parinfer-rust--disable nil)
    (setq-local parinfer-rust--disable t)))

;;;###autoload
(defun parinfer-rust-switch-mode ()
  "Switch to a different Parinfer mode. Either: indent, smart, or paren."
  (interactive)
  (setq-local parinfer-rust--mode
              (completing-read "Choose parinfer mode:"
                               (remove parinfer-rust--mode
                                       parinfer-rust--mode-types)
                               nil
                               t))
  (parinfer-rust--set-default-state)
  (parinfer-rust--dim-parens))

;;;###autoload
(defvar parinfer-rust-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-p s") 'parinfer-rust-switch-mode)
    (define-key m (kbd "C-c C-p d") 'parinfer-rust-toggle-disable)
    m)
  "Keymap for `parinfer-rust-mode'.")
;;;###autoload
(define-minor-mode parinfer-rust-mode
  "A simpler way to write lisps"
  :lighter " parinfer"
  :init-value nil
  :keymap parinfer-rust-mode-map
  (if parinfer-rust-enabled
      (parinfer-rust-mode-disable)
    (let ((changes-buffer-p (parinfer-rust--execute-change-buffer-p "paren")))
      (cond
       ;; We don't care about changing indentation
       ((not parinfer-rust-check-before-enable)
        (parinfer-rust-mode-enable))
       ;; We care about parinfer changing indentation
       ;; and it does change indentation
       ((and parinfer-rust-check-before-enable
             changes-buffer-p
             (y-or-n-p "Parinfer needs to modify indentation in this buffer to work. Continue? "))
        (parinfer-rust-mode-enable))
       ;; Do we care about parinfer changing indentation
       ;; and does not change the current buffer
       ((and parinfer-rust-check-before-enable
             (not changes-buffer-p))
        (parinfer-rust-mode-enable))

       (t (progn
            ;; This needs to be on so that we can turn off the
            ;; emacs' tracking of this mode
            (setq parinfer-rust-enabled t)
            (parinfer-rust-mode -1)))))))

(provide 'parinfer-rust-mode)
;;; parinfer-rust-mode.el ends here
