;;; parinfer-rust-mode.el --- An interface for the parinfer-rust library -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024  Justin Barclay

;; Author: Justin Barclay <justinbarclay@gmail.com>
;; URL: https://github.com/justinbarclay/parinfer-rust-mode
;; Version: 0.9.3
;; Package-Requires: ((emacs "26.1") (track-changes "1.1"))
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

;; Installation:
;;
;; If you're on Windows, Linux, or Arm Mac parinfer-rust will attempt to install the parinfer-rust
;; library for you. However, if you want you can compile the library from source at:
;; https://github.com/justinbarclay/parinfer-rust.

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
;; https://github.com/parinfer/parinfer.js/tree/master#api
;; https://github.com/parinfer/parinfer.js/blob/master/doc/integrating.md

;; In broad strokes we must:

;; 1. Monitor and record all changes and meta information about changes in the buffer. More info can
;; be found in parinfer-rust-changes.el

;; 2. Keep a record of the buffer state before the last time parinfer-rust was run

;; 3. Run parinfer-rust and update the state of the buffer accordingly

(defconst parinfer-rust-supported-versions '("0.4.7")
  "The Supported versions of the parinfer-rust library.

Versions of the library that `parinfer-rust-mode' was tested
against and is known to be api compatible.")

;; Require helper so we can check for library
(require 'parinfer-rust-helper)
(require 'track-changes)

(eval-when-compile
  (declare-function parinfer-rust-make-option "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-make-changes "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-new-options "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-make-request "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-execute "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-get-answer "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-debug "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-version "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-set-option "ext:parinfer-rust" t t)
  (declare-function parinfer-rust-get-option "ext:parinfer-rust" t t)
  (defvar parinfer-rust--lib-name (cond
                                   ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                   ((or (eq system-type 'gnu/berkeley)
                                        (eq system-type 'berkeley-unix))
                                    "parinfer-rust-freebsd.so")
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
(defgroup parinfer-rust-mode nil
  "Smart paren management for lisps using the parinfer-rust library."
  :group 'lisp
  :prefix "parinfer-rust-")

(defgroup parinfer-rust-options nil
  "Language specific options for parinfer-rust."
  :group 'parinfer-rust-mode
  :prefix "parinfer-rust-")

(defcustom parinfer-rust-auto-download nil
  "Automatically download the latest version of parinfer-rust from GitHub."
  :type 'boolean
  :group 'parinfer-rust-mode)

;; We need to define parinfer-rust--lib-name and parinfer-rust-library early so we know where to
;; find the parinfer-rust module.

(defconst parinfer-rust--lib-name (cond
                                   ((eq system-type 'darwin) "parinfer-rust-darwin.so")
                                   ((or
                                     (eq system-type 'gnu/berkeley)
                                     (eq system-type 'berkeley-unix))
                                    "parinfer-rust-freebsd.so")
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

(define-obsolete-variable-alias 'parinfer-rust--buffer-replace-strategy 'parinfer-rust-buffer-replace-strategy "0.8.7")

(defcustom parinfer-rust-buffer-replace-strategy 'safe
  "The strategy to use when replacing the buffer's text.

When set to `safe' the buffer is replaced using the slower but more
fastiduous `replace-buffer-contents'.

When set to `fast' the buffer is replaced using `delete-region'.

For more info on why the default is `replace-buffer-contents', see Info
node `(elisp)Replacing'"
  :type '(radio (const :tag "Safe" safe)
                (const :tag "Fast" fast))
  :group 'parinfer-rust-mode)

(defvar parinfer-rust--option-type '(plist
                                     :key-type symbol
                                     :options ((:force-balance boolean)
                                               (:return-parens boolean)
                                               (:partial-result boolean)
                                               (:lisp-vline-symbols boolean)
                                               (:lisp-block-comments boolean)
                                               (:guile-block-comments boolean)
                                               (:scheme-sexp-comments boolean)
                                               (:janet-long-strings boolean)
                                               (:comment-char string)
                                               (:string-delimiters (repeat string)))
                                     :value-type (choice boolean string (repeat string)))
  "The available options to pass to the parinfer-rust library.

These options are used to configure the behavior of the
parinfer-rust library to handle special cases in different lisps
and schemes.

The available options are:

  - `:force-balance': employ the aggressive paren-balancing rules
    from v1.
    - Type: boolean
    - Default: nil

  - `:return-parens': return the parens in the result text.
    - Type: boolean
    - Default: nil

  - `:partial-result': return partially processed text/cursor if
    an error occurs

    - Type: boolean
    - Default: nil

  - `:lisp-vline-symbols': recognize |lisp-style vline symbol|s.
    - Type: boolean
    - Default: nil

  - `:lisp-block-comments': recognize #|lisp-style block comments|#.
    - Type: boolean
    - Default: nil

  - `:guile-block-comments': recognize #!/guile/block/comments \\n!#
    - Type: boolean
    - Default: nil

  - `:scheme-sexp-comments': recognize #;( scheme sexp comments )
    - Type: boolean
    - Default: nil

  - `:janet-long-strings': recognize ``` janet-style long strings ```
    - Type: boolean
    - Default: nil

  - `:comment-char': a character (ie: string of length 1) that
    should be considered comments in the code
    - Type: string
    - Default: \";\"

  - `:string-delimiters' - the delimiters used for strings.
    - Type: (repeat string)
    - Default: (\"\\\"\")")


(defvar parinfer-rust--default-options '(:force-balance nil
                                         :return-parens nil
                                         :partial-result nil
                                         :lisp-vline-symbols nil
                                         :lisp-block-comments nil
                                         :guile-block-comments nil
                                         :scheme-sexp-comments nil
                                         :janet-long-strings nil
                                         :comment-char ";"
                                         :string-delimiters ("\""))
   "The set of options parinfer-rust considers default.

This is here mainly as reference for what is available to pass to
the library and what needs to be changed for major mode specific
implementations.")

(defcustom parinfer-rust-clojure-options '()
  "Options to configure parinfer-rust for clojure mode.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

(defcustom parinfer-rust-janet-options '(:comment-char "#"
                                         :janet-long-strings t)
  "Options to configure parinfer-rust for janet.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

(defcustom parinfer-rust-lisp-options '(:lisp-vline-symbols t
                                        :lisp-block-comments t)
  "Options to configure parinfer-rust for LISP.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

(defcustom parinfer-rust-racket-options '(:lisp-vline-symbols t
                                          :lisp-block-comments t
                                          :scheme-sexp-comments t)
  "Options to configure parinfer-rust for racket.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

(defcustom parinfer-rust-guile-options '(:lisp-vline-symbols t
                                         :lisp-block-comments t
                                         :guile-block-comments t
                                         :scheme-sexp-comments t)
  "Options to configure parinfer-rust for guile.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

(defcustom parinfer-rust-scheme-options '(:lisp-vline-symbols t
                                          :lisp-block-comments t
                                          :scheme-sexp-comments t)
  "Options to configure parinfer-rust for scheme.

See `parinfer-rust--option-type' for a more complete explanation of the options."
  :type parinfer-rust--option-type
  :group 'parinfer-rust-options)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'parinfer-rust parinfer-rust-library t)
(require 'parinfer-rust-changes)

(require 'subr-x)
(require 'font-lock)

(defconst parinfer-rust--mode-types '("indent" "smart" "paren")
  "The different modes that parinfer can operate on.")

(defvar parinfer-rust-major-mode-options
  (list 'clojure-mode parinfer-rust-clojure-options
        'clojurec-mode parinfer-rust-clojure-options
        'clojurescript-mode parinfer-rust-clojure-options
        'clojure-ts-mode parinfer-rust-clojure-options
        'clojure-ts-clojurescript-mode parinfer-rust-clojure-options
        'janet-mode parinfer-rust-janet-options
        'common-lisp-mode parinfer-rust-lisp-options
        'lisp-mode parinfer-rust-lisp-options
        'racket-mode parinfer-rust-racket-options
        'scheme-mode parinfer-rust-scheme-options)
  ;; Rewrite this string to be more readable
  "A plist that controls how parinfer-rust behaves for a given major mode.

For more information see `parinfer-rust--option-type'")

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
    (evil-change-whole-line . "paren")
    (quoted-insert . "paren"))
  "Commands to run with certain Parinfer mode.

A curated list of pairs consisting of a command and the mode the
command should be run in.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local State
;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defvar-local parinfer-rust--change-tracker nil)

(defvar-local parinfer-rust--last-mode nil
  "Last active Parinfer mode.
Used for toggling between paren mode and last active mode.")
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parinfer-rust--set-default-state ()
  "Set up parinfer for execution in a default context.

Good for switching modes, after an undo, or when first starting
parinfer."
  (let ((major-mode-defaults (or (plist-get parinfer-rust-major-mode-options major-mode)
                                 parinfer-rust--default-options)))
    (setq-local parinfer-rust--previous-options (parinfer-rust--generate-options
                                                 (parinfer-rust--set-options
                                                   (parinfer-rust-make-option)
                                                   major-mode-defaults)
                                                 (parinfer-rust-make-changes)))
    (setq-local parinfer-rust--previous-buffer-text (buffer-substring-no-properties
                                                     (point-min)
                                                     (point-max)))
    (setq-local parinfer-rust--changes nil)
    (setq-local parinfer-rust--disable nil)))

;; The idea for this function:
;;
;; 1. is to never run during an undo operation
;;
;; 2. Set a flag to ignore the first post command execution after an undo operation
;;
;; 2 is important because if we undo our last key press and that causes parinfer to modify the
;; buffer we get stuck in a loop of trying to undo things and parinfer redoing them
(defun parinfer-rust--track-undo (orig-func &rest args)
  "Wraps ORIG-FUNC and ARGS in some state tracking for `parinfer-rust-mode'."
  (unwind-protect
    (apply orig-func args)
    ;; We want to "Ignore" errors here otherwise the function exits
    ;; and causes the following commands, where we set flags, to be
    ;; ignored
    (progn
      ;; Always ignore the first post-command-hook run of parinfer after an undo
      (setq-local parinfer-rust--ignore-post-command-hook t)
      (parinfer-rust--set-default-state))))

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
(defun parinfer-rust--set-options (options new-options)
  "Update the `OPTIONS' with values in the plist `NEW-OPTIONS'.

This mutates the current reference to `OPTIONS'
Ex:
  (parinfer-rust--set-options parinfer-rust--previous-options ;; \='((:cursor-x . 1) (:cursor-line . 1))
                               \='(:cursor-x 2 :cursor-line 2))
;;=> \='((:cursor-x . 2) (:cursor-line . 2))"
  (mapc (lambda (option)
          ;; Note to self set-option might need to clone in order to keep old option immutable
          (parinfer-rust-set-option options
                                    (car option)
                                    (cadr option)))
        ;; partition plist into key-value pairs
        (seq-partition new-options 2))
  options)

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
                ;; Through impropriety or mistakes parinfer-rust--mode _could_ be set to nil.
                ;; So let's make sure a sensible default is set just in case.
                (or parinfer-rust--mode
                    "paren")))
             (old-options (or (parinfer-rust--local-bound-and-true parinfer-rust--previous-options)
                              (parinfer-rust-make-option)))
             (changes (if (> (length parinfer-rust--changes) 0)
                          (parinfer-rust--build-changes parinfer-rust--changes)
                        (parinfer-rust-make-changes)))
             (options (parinfer-rust--generate-options old-options
                                                       changes))
             (request (parinfer-rust-make-request parinfer-rust--mode
                                                  (buffer-substring-no-properties (point-min)
                                                                                  (point-max))
                                                  options))
             (answer (parinfer-rust-execute request))
             (replacement-string (parinfer-rust-get-answer answer :text))
             (parinfer-error (parinfer-rust-get-answer answer :error)))
        (when (and (local-variable-if-set-p 'parinfer-rust--in-debug)
                   parinfer-rust--in-debug)
          (parinfer-rust-debug "./parinfer-rust-debug.txt" options answer))
        (if parinfer-error
            (message "Problem on line %s: %s"
             (plist-get parinfer-error :line_no)
             (plist-get parinfer-error :message)) ;; TODO - Handler errors
          ;; This stops Emacs from flickering when scrolling
          (if (not (string-equal parinfer-rust--previous-buffer-text replacement-string))
              (save-mark-and-excursion
                (let ((change-group (prepare-change-group))
                      (current (current-buffer))
                      (new-buf (get-buffer-create "*parinfer*")))
                  (switch-to-buffer new-buf)
                  (insert replacement-string)
                  (switch-to-buffer current)
                  (let ((window-start-pos (window-start))
                        (was-replaced-safely nil))
                    (if (eq parinfer-rust-buffer-replace-strategy 'fast)
                        (progn
                          (delete-region (point-min) (point-max))
                          (insert-buffer-substring new-buf))
                      (setq was-replaced-safely-safely (replace-buffer-contents new-buf 1)))
                    (when (and (not was-replaced-safely)
                               (not (= window-start-pos (window-start))))
                      ;; If the buffer is not pixel aligned, this will cause a slight jump. But if
                      ;; we want speed and not to jump around too much, this is the best we can do
                      ;; for now.
                      (set-window-start (selected-window) window-start-pos)))
                  (kill-buffer new-buf)
                  (undo-amalgamate-change-group change-group)))))
        (when-let ((new-x (parinfer-rust-get-answer answer :cursor-x))
                   (new-line (parinfer-rust-get-answer answer :cursor-line)))
          (parinfer-rust--reposition-cursor new-x new-line))
        (setq parinfer-rust--previous-options options)
        (when parinfer-rust--change-tracker
          ;; Ignore our own changes.
          (track-changes-fetch parinfer-rust--change-tracker #'ignore))))))

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
         (format "Parinfer needs to modify indentation in the buffer %s to work.  Continue? " (current-buffer)))
        (let ((parinfer-rust--mode "paren"))
          (parinfer-rust--execute))
      t)))


(defun parinfer-rust--check-for-issues (&rest _)
  "Check for issues that can cause unwanted behaviors.

Disable `parinfer-rust-mode' if the user does not want to have
parinfer autofix them, or if there is no reasonable way for
`parinfer-rust-mode' to automatically fix them."
  ;; TODO: this should only run the first time the buffer is actually changed.
  ;; buffer searching or navigating should not trigger this.
  (setq-local parinfer-rust--disable nil)
  ;; Disable change tracker for now because we are about to make changes in an change hook.
  (track-changes-unregister parinfer-rust--change-tracker)
  (setq-local parinfer-rust--change-tracker nil)
  (if-let (issue (or (parinfer-rust--check-for-tabs)
                     (parinfer-rust--check-for-indentation)))
      (parinfer-rust-mode -1)
    ;; Re-enable change trackers now that we've succeeded in our tasks
    (setq-local parinfer-rust--change-tracker
                (track-changes-register #'parinfer-rust--changes-signal
                                        :disjoint t)))
  (remove-hook 'pre-command-hook #'parinfer-rust--check-for-issues t))

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

(defun parinfer-rust-mode-setup ()
  "Enable Parinfer."
  (parinfer-rust--detect-troublesome-modes)
  (parinfer-rust--set-default-state)
  (setq-local parinfer-rust--mode (if (stringp parinfer-rust-preferred-mode)
                                      parinfer-rust-preferred-mode
                                    (and (symbolp parinfer-rust-preferred-mode)
                                     (symbol-name parinfer-rust-preferred-mode))))

  (advice-add 'undo :around #'parinfer-rust--track-undo)
  (advice-add 'undo-tree-undo :around #'parinfer-rust--track-undo)
  (if (fboundp 'track-changes-register)
      (progn
        (when parinfer-rust--change-tracker
          (track-changes-unregister parinfer-rust--change-tracker)
          (setq-local parinfer-rust--change-tracker nil))
        (setq-local parinfer-rust--change-tracker
              (track-changes-register #'parinfer-rust--changes-signal
                                      :disjoint t))))
  (parinfer-rust--dim-parens))

(defun parinfer-rust-mode-disable ()
  "Disable Parinfer."
  (advice-remove 'undo #'parinfer-rust--track-undo)
  (when (fboundp 'undo-tree-undo)
    (advice-remove 'undo-tree-undo #'parinfer-rust--track-undo))
  (when parinfer-rust--change-tracker
    (track-changes-unregister parinfer-rust--change-tracker)
    (setq-local parinfer-rust--change-tracker nil))
  (remove-hook 'pre-command-hook #'parinfer-rust--check-for-issues t)
  (setq-local parinfer-rust--disable nil)
  (parinfer-rust--dim-parens))

(defun parinfer-rust-toggle-disable ()
  "Temporarily disable parinfer-rust.

This includes stopping tracking of all changes."
  (interactive)
  (if parinfer-rust--disable
      (setq-local parinfer-rust--disable nil)
    (setq-local parinfer-rust--disable t)))

(defun parinfer-rust--auto-apply-fast-mode ()
  "Switch to the fast buffer replace strategy when the buffer is over 1000 lines."
    (when (< 1000 (count-lines (point-min)
                             (point-max)))
      (setq-local parinfer-rust-buffer-replace-strategy 'fast)))

(defun parinfer-rust-mode-enable ()
  "Enable Parinfer."
  ;; Make sure the library is installed at the appropriate location or offer to download it
  (if (and (parinfer-rust--check-for-library parinfer-rust-supported-versions
                                        parinfer-rust-library
                                        parinfer-rust--lib-name
                                        parinfer-rust-auto-download)
           (require 'parinfer-rust parinfer-rust-library t)
           (parinfer-rust--check-version parinfer-rust-supported-versions
                                      (parinfer-rust-version)
                                      parinfer-rust-library
                                      parinfer-rust--lib-name))
      (progn
        ;; Check version and prompt to download latest version if out of date Problem: Emacs can't
        ;; reload dynamic libraries, which means that if we download a new library the user has to
        ;; restart Emacs for changes to take effect.
        (parinfer-rust-mode-setup)
        (cond ((or (eq 'defer parinfer-rust-check-before-enable)
                   buffer-read-only)
               ;; Defer checking for changes until a user changes the buffer
               (setq-local parinfer-rust--disable t)
               (add-hook 'pre-command-hook #'parinfer-rust--check-for-issues nil t))
              ((eq 'immediate parinfer-rust-check-before-enable)
               (setq-local parinfer-rust--disable t)
               (parinfer-rust--check-for-issues))
              (t (let ((parinfer-rust--mode "paren"))
                   (parinfer-rust--execute)))))
    (progn
      (message "Unable to load library parinfer-rust disabling parinfer-rust-mode")
      (parinfer-rust-mode -1))))

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
  (cond
   ((not parinfer-rust-mode)
    (parinfer-rust-mode-disable))
   ;; Don't do anything if the buffer is not selected
   ;; TODO: Come up with a better way to defer and disable loading
   ;; Defer waits for window selection change and disabled waits for a change event
   ;; there is also the idea of deferring the running of parinfer vs deferring the loading
   ((not (eq (current-buffer)
             (window-buffer (selected-window))))
    (add-hook 'window-selection-change-functions #'parinfer-rust--defer-loading nil t))
   (t
    (parinfer-rust-mode-enable))))


(provide 'parinfer-rust-mode)

;;; parinfer-rust-mode.el ends here
