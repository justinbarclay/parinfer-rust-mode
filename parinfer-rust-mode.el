;;; parinfer-rust-mode.el --- An interface for the parinfer-rust library -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Version: 0.8.3
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An intuitive editor mode to make paren management fun and easy without sacrificing power.

;; How it works:

;; Parinfer users the state of the buffer combined with the current mode (paren, indent, or smart)
;; to either balance an s-expression's indentation, paren, or to do both.

;; Let's go over some light examples to get a sense of how parinfer works. I am going to use `|` to
;; indicate the cursor and `_` to indicate the a space that's been added either by the user or
;; parinfer.

;; Paren Mode gives you full control of parens, while Parinfer corrects indentation.

;; For example, if we start off with the cursor before `(foo`

;; ```
;; |(foo
;;    bar)
;; ```

;; and the user inserts a space
;; ```
;; _|(foo
;;    bar)
;; ```

;; then parinfer will maintain, infer, that a space is needed in front of `bar)` to maintain
;; indentation

;; ```
;;  |(foo
;;    _bar)
;; ```

;; Indent Mode gives you full control of indentation, while Parinfer corrects or inserts
;; close-parens where appropriate.

;;Now the cursor is before `4`

;; ```
;; (foo [1 2 3]
;;     |4 5 6)
;; ```

;; and the user inserts a space
;; ```
;; (foo [1 2 3]
;;     _|4 5 6)
;; ```

;; then parinfer will adjust the `]` and move it down to the follow line to enclose the 4 5 6

;; ```
;; (foo [1 2 3
;;      |4 5 6])
;; ```

;; Smart Mode is like Indent Mode, but it tries to preserve the structure too. This roughly
;; translates to it treating everything before the cursor as indent-mode and every after the cursor
;; as paren-mode.

;; The cursor is before `(+`

;; ```
;; (let [x (fn [])]
;; |(+ 1
;;     2)
;;  x)
;; ```

;; and the user add several spaces to the sexp
;; ```
;; (let [x (fn [])]
;;  ________|(+ 1
;;     2)
;; x)
;; ```

;; Smart-Mode will move a ) and a ] to enclose the addition function call and move the 2 to maintain
;; structure

;; ```
;; (let [x (fn []
;;         |(+ 1
;;    _________2))]
;; x)
;; ```


;; To find out more about how parinfer works go to: https://shaunlebron.github.io/parinfer/
;;
;; `parinfer-rust-mode` provides an interface between the `parinfer-rust` library
;; and Emacs.  As such it's primary role is to capture meta information about the
;; buffer and transfer it to the parinfer-rust API.
;;
;; As such parinfer-rust-mode requires that your version of Emacs supports modules.

;;; Code:

;; For a complete list of state that needs to be tracked read:
;; https://github.com/shaunlebron/parinfer/tree/master/lib#api
;; https://github.com/shaunlebron/parinfer/blob/master/lib/doc/integrating.md

;; In broad strokes we must:

;; 1. Monitor and record all changes and meta information about changes in the buffer. More info can
;; be found in parinfer-rust-changes.el

;; 2. Keep a record of the buffer state before the last time parinfer-rust was run

;; 3. Run parinfer-rust and update the state of the buffer accordingly

(defconst parinfer-rust-supported-versions '("0.4.4-beta" "0.4.3")
  "The Supported versions of the parinfer-rust library.

Versions of the library that `parinfer-rust-mode' was tested
against and is known to be api compatible.")

;; Require helper so we can check for library
(require 'parinfer-rust-helper)

(eval-when-compile
  (declare-function parinfer-rust-make-option "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-make-changes "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-new-options "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-make-request "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-execute "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-get-in-answer "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-debug "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-print-error "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-version "ext:parinfer-rust" t t)
  (defvar parinfer-rust--lib-name (cond
                                   ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                   ((eq system-type 'gnu/linux) "parinfer-rust-linux.so")
                                   ((eq system-type 'windows-nt) "parinfer-rust-windows.dll")))
  (defvar parinfer-rust-library-dir
    (concat user-emacs-directory "parinfer-rust/"))
  (defvar parinfer-rust-library
    (concat parinfer-rust-library-dir parinfer-rust--lib-name))
  (unless (bound-and-true-p module-file-suffix)
    (error "Emacs was not compiled with the '--with-modules'. Unable to load parinfer-rust-mode")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customization options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom parinfer-rust-auto-download nil
  "Automatically download the latest version of parinfer-rust from GitHub."
  :type 'boolean
  :group 'parinfer-rust-mode)

;; We need to define parinfer-rust--lib-name and parinfer-rust-library early so we know where to
;; find the parinfer-rust module.

(defconst parinfer-rust--lib-name (cond
                                   ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                   ((eq system-type 'gnu/linux) "parinfer-rust-linux.so")
                                   ((eq system-type 'windows-nt) "parinfer-rust-windows.dll"))
  "System dependent library name for `parinfer-rust-mode'.")

(defcustom parinfer-rust-library-directory (locate-user-emacs-file (concat user-emacs-directory
                                                                           "parinfer-rust/"))
  "The directory to store or to find the parinfer-rust library in."
  :type 'directory
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-library (concat parinfer-rust-library-directory
                                         parinfer-rust--lib-name)
  "The location to store or to find the parinfer-rust library."
  :type 'file
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-preferred-mode "smart"
  "Preferred mode for parinfer-rust."
  :type '(radio (const :tag "indent" "indent")
                (const :tag "smart" "smart")
                (const :tag "paren" "paren"))
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-check-before-enable 'defer
  "Perform check on indentation before enabling `parinfer-rust-mode'.

  If Parinfer detects that it needs to change the indentation in
  the before first running, it will prompt the user whether it is
  OK to adjust the indentation. If the user disagrees Parinfer
  will disable itself. The user may choose to get the prompt
  immediately whenever `parinfer-rust-mode' is enabled, defer it
  until the first change in the buffer, or disable it and never
  receive a prompt. When disabled, `parinfer-rust-mode' will run
  automatically balance the indentation for the user."
  :type '(radio (const :tag "Immediate" immediate)
                (const :tag "Defer" defer)
                (const :tag "Disabled" nil))
  :group 'parinfer-rust-mode)

(defcustom parinfer-rust-dim-parens t
  "Whether to dim inferred parens in Indent and Smart modes."
  :type 'boolean
  :group 'parinfer-rust-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst parinfer-rust--mode-types '("indent" "smart" "paren")
  "The different modes that parinfer can operate on.")

(require 'parinfer-rust parinfer-rust-library t)
(require 'parinfer-rust-changes)

(require 'subr-x)
(require 'font-lock)

;; Check version and prompt to download latest version if out of date
;; Problem: Emacs can't reload dynamic libraries, which means that if we
;; download a new library the user has to restart Emacs for changes to take effect.
;; (parinfer-rust--check-version parinfer-rust-supported-version
;;                               (parinfer-rust-version)
;;                               parinfer-rust-library
;;                               parinfer-rust--lib-name)

;; This is a hack around Emacs complex command and scripting system. In some cases
;; parinfer-rust-mode sucks at picking up the correct changes in the buffer, so the
;; `treat-command-as` system is a means to work around `parinfer-rust-mode`'s or Emacs limitations
;; and give hints to `parinfer-rust-mode` for how you want to treat specific commands in
;; `smart-mode`.

(defvar parinfer-rust-treat-command-as
  '((paredit-forward-barf-sexp . "paren")
    (paredit-forward-slurp-sexp . "paren")
    (yank . "paren")
    (counsel-yank-pop . "paren")
    (evil-open-above . "paren")
    (evil-change-whole-line . "paren"))
  "Commands to run with certain Parinfer mode.

A curated list of pairs consisting of a command and the mode the
command should be run in.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local State
;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local parinfer-rust-enabled nil "Tracks if parinfer has been enabled.")
(defvar-local parinfer-rust--in-debug nil
  "When enabled, outputs the response input and output of the parinfer response to a file.")
(defvar-local parinfer-rust--mode "paren"
  "The current mode that parinfer running under to managing your parenthesis.

Either `paren', `indent', or `smart'.")
(defvar-local parinfer-rust--previous-options nil
  "The last set of record of changes and meta information of changes in the buffer.")
;; TODO this might be not needed anymore
(defvar-local parinfer-rust--disable nil "Temporarily disable parinfer.")
(defvar-local parinfer-rust--previous-buffer-text ""
  "The text in the buffer previous to when parinfer-rust ran last.")
(defvar-local parinfer-rust--ignore-post-command-hook nil
  "A hack to not run parinfer-execute after an undo has finished processing.")

(defvar parinfer-rust--last-mode nil
  "Last active Parinfer mode.
Used for toggling between paren mode and last active mode.")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parinfer-rust--set-default-state ()
  "Set up parinfer for execution in a default context.

Good for switching modes, after an undo, or when first starting
parinfer."
  (setq-local parinfer-rust--previous-options (parinfer-rust--generate-options
                                               (parinfer-rust-make-option)
                                               (parinfer-rust-make-changes)))
  (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties
                                                   (point-min)
                                                   (point-max)))
  (setq-local parinfer-rust--changes nil)
  (setq-local parinfer-rust--disable nil))


;; The idea for this function: 1. is to never run during an undo operation 2. Set a flag to ignore
;; the first post command execution after an undo operation 2 is important because if we undo our
;; last key press and that causes parinfer to modify the buffer we get stuck in a loop of trying to
;; undo things and parinfer redoing them

(defun parinfer-rust--track-undo (orig-func &rest args)
  "Wraps ORIG-FUNC and ARGS in some state tracking for `parinfer-rust-mode'."
  (condition-case-unless-debug err
      (apply orig-func args)
    ;; We want to "Ignore" errors here otherwise the function exits
    ;; and causes the following commands, where we set flags, to be
    ;; ignored
    (error
     (message "%s" (cadr err))
     nil))
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
;; The change interface and associated functions for change tracking
;; can be found in parinfer-rust-changes.el
(defun parinfer-rust--generate-options (old-options changes)
  "Capture the buffer state and associated metadata needed to execute parinfer.

Builds a parinfer-rust OPTION struct based on OLD-OPTIONS and
CHANGES."
  (let ((options (parinfer-rust-new-options
                  (parinfer-rust--get-cursor-x)
                  (parinfer-rust--get-cursor-line)
                  nil
                  old-options
                  changes)))
    (setq-local parinfer-rust--changes nil)
    options))

(defun parinfer-rust--execute (&rest _args)
  "Run parinfer in the current buffer."
  (if (or parinfer-rust--disable
          undo-in-progress
          parinfer-rust--ignore-post-command-hook)
      ;; Do nothing and disable flags
      (when parinfer-rust--ignore-post-command-hook
        (setq-local parinfer-rust--changes nil)
        (setq-local parinfer-rust--ignore-post-command-hook nil))
    (progn
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
                    ;; mode; the changes Emacs reports may be different than
                    ;; those parinfer expects.
                    (setq parinfer-rust--changes nil)
                    mode)
                parinfer-rust--mode))
             (old-options (or (parinfer-rust--local-bound-and-true parinfer-rust--previous-options)
                              (parinfer-rust-make-option)))
             (changes (if (> (length parinfer-rust--changes) 0)
                          (parinfer-rust--build-changes
                           (parinfer-rust--combine-changes
                            parinfer-rust--changes))
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
            (message "%s" (parinfer-rust-print-error error-p)) ;; TODO handle errors
          ;; This stops Emacs from flickering when scrolling
          (if (not (string-equal parinfer-rust--previous-buffer-text replacement-string))
              (save-mark-and-excursion
                (let ((change-group (prepare-change-group))
                      (current (current-buffer))
                      (new-buf (get-buffer-create "*parinfer*")))
                  (switch-to-buffer new-buf)
                  (insert replacement-string)
                  (switch-to-buffer current)
                  (replace-buffer-contents new-buf)
                  (kill-buffer new-buf)
                  (undo-amalgamate-change-group change-group)))))
        (when-let ((new-x (parinfer-rust-get-in-answer answer "cursor_x"))
                   (new-line (parinfer-rust-get-in-answer answer "cursor_line")))
          (parinfer-rust--reposition-cursor new-x new-line))
        (setq parinfer-rust--previous-options options)
        (with-no-warnings ;; TODO: Should not need with-no-warnings function
          (setq-local inhibit-modification-hooks nil))))))

;; Interactive or functions meant to be called by user
(defun parinfer-rust-toggle-debug ()
  "Turn on debug for parinfer.

This will create a text file in the current directory."
  (interactive)
  (if parinfer-rust--in-debug
      (setq parinfer-rust--in-debug nil)
    (setq parinfer-rust--in-debug t)))

(defun parinfer-rust--check-for-tabs ()
  "Return t current buffer has a `\t'."
  (when (> (how-many "\t" (point-min) (point-max))
           0)
    (message (concat "Disabling parinfer-rust-mode because parinfer-rust "
                     "does not work on files with the tab character."))
    t))

(defun parinfer-rust--check-for-indentation (&rest _)
  "Check to see if running in paren mode will cause a change in the buffer.

If a change is detected in the buffer, prompt the user to see if they still want
`parinfer-rust-mode' enabled."
  (when (parinfer-rust--execute-change-buffer-p "paren")
    (if (y-or-n-p
         "Parinfer needs to modify indentation in this buffer to work.  Continue? ")
        (let ((parinfer-rust--mode "paren"))
          (parinfer-rust--execute))
      t)))

(defun parinfer-rust--check-for-issues (&rest _)
  "Check for issues that can cause unwanted behaviors.

Disable `parinfer-rust-mode' if the user does not want to have
parinfer autofix them, or if there is no reasonable way for
`parinfer-rust-mode' to automatically fix them."
  (setq-local parinfer-rust--disable nil)
  (if-let (issue (or (parinfer-rust--check-for-tabs)
                     (parinfer-rust--check-for-indentation)))
      (parinfer-rust-mode -1))
  (remove-hook 'before-change-functions #'parinfer-rust--check-for-issues t))

(defun parinfer-rust--switch-mode (&optional mode)
  "Switch to a different Parinfer MODE.

Checks if MODE is a valid Parinfer mode, and uses
`parinfer-rust-preferred-mode' otherwise. Sets
`parinfer-rust--last-mode' variable to current MODE."
  (setq-local parinfer-rust--mode
              (if (member mode parinfer-rust--mode-types)
                  mode
                parinfer-rust-preferred-mode))
  (unless (string= parinfer-rust--mode "paren")
    (setq-local parinfer-rust--last-mode parinfer-rust--mode))
  (parinfer-rust--set-default-state)
  (parinfer-rust--dim-parens))

(defun parinfer-rust-mode-enable ()
  "Enable Parinfer."
  (setq-local parinfer-rust-enabled t)
  (parinfer-rust--detect-troublesome-modes)
  (parinfer-rust--set-default-state)
  (setq-local parinfer-rust--mode parinfer-rust-preferred-mode)
  (advice-add 'undo :around #'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-add 'undo-tree-undo :around #'parinfer-rust--track-undo))
  (add-hook 'after-change-functions #'parinfer-rust--track-changes t t)
  (add-hook 'post-command-hook #'parinfer-rust--execute t t)
  (parinfer-rust--dim-parens))

(defun parinfer-rust-mode-disable ()
  "Disable Parinfer."
  (advice-remove 'undo #'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-remove 'undo-tree-undo #'parinfer-rust--track-undo))
  (remove-hook 'after-change-functions #'parinfer-rust--track-changes t)
  (remove-hook 'post-command-hook #'parinfer-rust--execute t)
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
  "Switch to a different Parinfer mode.

Either: indent, smart, or paren."
  (interactive)
  (parinfer-rust--switch-mode
   (completing-read "Choose parinfer mode:"
                    (remove parinfer-rust--mode
                            parinfer-rust--mode-types)
                    nil
                    t)))

;;;###autoload
(defun parinfer-rust-toggle-paren-mode ()
  "Switch to paren mode if current mode is either smart or indent.
Switch back to previous mode if current mode is paren mode. Uses
`parinfer-rust-preferred-mode' as a fallback if previous mode is
not available."
  (interactive)
  (if (string= parinfer-rust--mode "paren")
      (parinfer-rust--switch-mode parinfer-rust--last-mode)
    (parinfer-rust--switch-mode "paren")))

;;;###autoload
(defvar parinfer-rust-mode-map (make-sparse-keymap)
  "Keymap for `parinfer-rust-mode'.")

;;;###autoload
(define-minor-mode parinfer-rust-mode
  "A simpler way to write lisps."
  :lighter (:eval (concat " parinfer:" parinfer-rust--mode))
  :init-value nil
  :keymap parinfer-rust-mode-map
  (if parinfer-rust-enabled
      (parinfer-rust-mode-disable)
    (progn
      ;; Make sure the library is installed at the appropriate location or offer to download it
      (when (parinfer-rust--check-for-library parinfer-rust-supported-versions
                                              parinfer-rust-library
                                              parinfer-rust--lib-name
                                              parinfer-rust-auto-download)
        (require 'parinfer-rust parinfer-rust-library t))
      ;; Check version and prompt to download latest version if out of date Problem: Emacs can't
      ;; reload dynamic libraries, which means that if we download a new library the user has to
      ;; restart Emacs for changes to take effect.
      (parinfer-rust--check-version parinfer-rust-supported-versions
                                    (parinfer-rust-version)
                                    parinfer-rust-library
                                    parinfer-rust--lib-name)
      (parinfer-rust-mode-enable)
      (cond ((or (eq 'defer parinfer-rust-check-before-enable)
                 buffer-read-only)
             ;; Defer checking for changes until a user changes the buffer
             (setq-local parinfer-rust--disable t)
             (add-hook 'before-change-functions #'parinfer-rust--check-for-issues t t))

            ((eq 'immediate parinfer-rust-check-before-enable)
             (setq-local parinfer-rust--disable t)
             (parinfer-rust--check-for-issues))

            (t (let ((parinfer-rust--mode "paren"))
                 (parinfer-rust--execute)))))))

(provide 'parinfer-rust-mode)
;;; parinfer-rust-mode.el ends here
