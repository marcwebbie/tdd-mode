;;; tdd-mode.el --- Modern TDD Mode for Python -*- lexical-binding: t; -*-

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; Maintainer: Marcwebbie <marcwebbie@gmail.com>
;; Created: 2024-11-12
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/marcwebbie/tdd-mode
;; Keywords: tools, convenience, testing, python, tdd

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

;;; Commentary:

;; tdd-mode provides a modern, intuitive, and responsive minor mode for
;; test-driven development in Python projects using Emacs.
;;
;; It supports various test runners (e.g. pytest, nosetests, Django),
;; provides interactive commands to run tests at point, rerun last tests,
;; display output in a dedicated buffer, and highlight the Emacs mode-line
;; based on test results using dynamic fading effects.
;;
;; Features:
;; - Run tests for current function, class, file, or project
;; - Configurable test runners: pytest, Django, nosetests
;; - Automatic test reruns on file save
;; - Alerts for pass/fail (with optional integration with "alert")
;; - Mode-line blinking with color fade feedback
;; - Interactive command map for test operations
;; - Copy test command or output to clipboard
;; - Intelligent test discovery using Python syntax
;; - Rich customization options via Emacs Customize
;;
;; Usage:
;; Enable tdd-mode in a Python buffer or globally with:
;;
;;    (add-hook 'python-mode-hook #'tdd-mode)
;;
;; Bind "tdd-mode-command-map" to a convenient keybinding:
;;    (global-set-key (kbd "C-c C-t") tdd-mode-command-map)
;;
;; Note: Removed all backticks to resolve melpazoid warnings.
;;
;; See the GitHub repository for documentation, issues, and contributions.

;;; Code:
(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'python)
(require 'compile)

(defgroup tdd nil
  "Test-Driven Development for Python projects in Emacs."
  :group 'tools
  :prefix "tdd-mode-")

(defcustom tdd-mode-test-runner 'pytest
  "Test runner to use for TDD mode."
  :type '(choice (const :tag "Pytest" pytest)
                 (const :tag "Nosetests" nosetests)
                 (const :tag "Django" django))
  :group 'tdd)

(defcustom tdd-mode-notify-on-pass t
  "Notify when a test passes."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-notify-on-fail t
  "Notify when a test fails."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-auto-run-on-save t
  "Automatically rerun the last test command on save."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-buffer-popup t
  "If non-nil, displays the *tdd-output* buffer after each test run.
If set to nil, keeps the buffer in the background."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-verbose nil
  "Toggle verbose debug output for TDD Mode."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-blink-enabled t
  "If non-nil, enables mode-line blinking on test failures and success."
  :type 'boolean
  :group 'tdd)

(defcustom tdd-mode-blink-fail-color "#F44336"
  "Color for the mode-line when a test fails."
  :type 'string
  :group 'tdd)

(defcustom tdd-mode-blink-pass-color "#4CAF50"
  "Color for the mode-line when a test passes."
  :type 'string
  :group 'tdd)

(defcustom tdd-mode-blink-steps 20
  "Number of steps for the mode-line fade effect."
  :type 'integer
  :group 'tdd)

(defcustom tdd-mode-blink-interval 0.2
  "Interval in seconds between each fade step."
  :type 'number
  :group 'tdd)

(defcustom tdd-mode-scroll-output t
  "If non-nil, the compilation buffer will automatically scroll to follow output."
  :type 'boolean
  :group 'tdd)

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "Stores the last test command used in tdd-mode.")

(defvar tdd-mode-last-test-exit-code nil
  "Stores the last exit code to show status across buffers.")

(defvar tdd-mode-original-mode-line-bg (face-background 'mode-line)
  "Original mode-line background color.")

(defvar tdd-mode-fade-timer nil
  "Timer to control mode-line fading.")

(defvar tdd-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'tdd-mode-run-test-at-point)
    (define-key map (kbd "a") #'tdd-mode-run-all-tests)
    (define-key map (kbd "l") #'tdd-mode-run-last-test)
    (define-key map (kbd "c") #'tdd-mode-copy-output-to-clipboard)
    (define-key map (kbd "r") #'tdd-mode-run-relevant-tests)
    (define-key map (kbd "f") #'tdd-mode-run-file-tests)
    (define-key map (kbd "p") #'tdd-mode-copy-test-command-to-clipboard)
    (define-key map (kbd "b") #'tdd-mode-insert-ipdb-breakpoint)
    (define-key map (kbd "B") #'tdd-mode-insert-pudb-breakpoint)
    (define-key map (kbd "C") #'tdd-mode-copy-diff-and-output)
    map)
  "Keymap for tdd-mode commands.")

;; Declare variables and functions to suppress compiler warnings
(defvar tdd-mode nil
  "Non-nil if TDD Mode is enabled.")
(defvar tdd-mode-alert-enabled nil
  "Non-nil if alert package is available.")
(declare-function alert "alert" (message &rest args))

;; Optional alert package
(if (require 'alert nil 'noerror)
    (setq tdd-mode-alert-enabled t)
  (setq tdd-mode-alert-enabled nil))

(defun tdd-mode-log (message &rest args)
  "Log MESSAGE with ARGS if tdd-mode-verbose is enabled."
  (when tdd-mode-verbose
    (message "[tdd-mode] %s" (apply #'format message args))))

(defun tdd-mode-set-mode-line-color (color)
  "Set the mode-line background color to COLOR."
  (tdd-mode-log "Setting mode-line color to: %s" color)
  (set-face-background 'mode-line color))

(defvar tdd-mode-blinking-in-progress nil
  "Flag to indicate if the mode-line blinking is in progress.")

(defun tdd-mode-blink-mode-line (color)
  "Blink the mode-line by fading from COLOR to the original background."
  (tdd-mode-log "Blinking mode-line with color: %s" color)
  (when (timerp tdd-mode-fade-timer)
    (tdd-mode-log "Canceling existing fade timer")
    (cancel-timer tdd-mode-fade-timer))
  (let* ((start-color (color-name-to-rgb color))
         (end-color (color-name-to-rgb tdd-mode-original-mode-line-bg))
         (step-colors (tdd-mode-generate-fade-colors start-color end-color tdd-mode-blink-steps)))
    (tdd-mode-log "Generated %d fade steps" (length step-colors))
    (setq tdd-mode-blinking-in-progress t)
    (setq tdd-mode-fade-timer
          (run-with-timer 0 tdd-mode-blink-interval
                          (lambda ()
                            (if (null step-colors)
                                (progn
                                  (tdd-mode-log "Fade completed")
                                  (cancel-timer tdd-mode-fade-timer)
                                  (setq tdd-mode-fade-timer nil)
                                  (setq tdd-mode-blinking-in-progress nil)
                                  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg)
                                  ;; Start a new blink cycle after a short delay
                                  (when (equal color tdd-mode-blink-fail-color)
                                    (run-with-timer 1 nil #'tdd-mode-blink-mode-line color)))
                              (tdd-mode-log "Setting mode-line color to: %s" (car step-colors))
                              (tdd-mode-set-mode-line-color (pop step-colors))))))))

(defun tdd-mode-generate-fade-colors (start-color end-color steps)
  "Generate a list of colors fading from START-COLOR to END-COLOR in STEPS steps."
  (tdd-mode-log "Generating fade colors from %s to %s in %d steps" start-color end-color steps)
  (cl-loop for i from 0 below steps
           collect (apply #'color-rgb-to-hex
                          (cl-mapcar (lambda (start end)
                                       (+ start (* i (/ (- end start) (float steps)))))
                                     start-color end-color))))

(defun tdd-mode-update-mode-line (exit-code)
  "Update the mode-line color based on the last test EXIT-CODE."
  (tdd-mode-log "Updating mode-line with exit code: %s" exit-code)
  (setq tdd-mode-last-test-exit-code exit-code)
  (let ((color (if (eq exit-code 0)
                   tdd-mode-blink-pass-color
                 tdd-mode-blink-fail-color)))
    (tdd-mode-blink-mode-line color)))

(defun tdd-mode--get-testable ()
  "Get the testable entity at point (function, class, or file)."
  (let* ((inner-obj (tdd-mode--inner-testable))
         (outer (tdd-mode--outer-testable))
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (tdd-mode-log "Inner testable: %s, Outer testable: %s" inner-obj outer-obj)
    (if (and inner-obj outer-def outer-obj)
        (cond
         ((equal outer-def "def") outer-obj)
         ((equal inner-obj outer-obj) outer-obj)
         (t (format "%s::%s" outer-obj inner-obj)))
      nil)))

(defun tdd-mode--inner-testable ()
  "Find the function name for the test at point."
  (save-excursion
    (re-search-backward
     "^[ \t]\\{0,4\\}\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (if (match-beginning 2)
        (buffer-substring-no-properties (match-beginning 2) (match-end 2))
      nil)))

(defun tdd-mode--outer-testable ()
  "Find the class or outer function around point."
  (save-excursion
    (re-search-backward
     "^\\(class\\|\\(?:async \\)?def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (if (match-beginning 2)
        (cons (match-string 1) (match-string 2))
      nil)))

(defun tdd-mode-get-test-command ()
  "Construct the test command using the test entity at point."
  (let* ((file-name (buffer-file-name))
         (runner (tdd-mode-get-runner))
         (test-entity (tdd-mode--get-testable)))
    (tdd-mode-log "Test entity: %s" test-entity)
    (if test-entity
        (format "%s %s::%s" runner file-name test-entity)
      (format "%s %s" runner file-name))))

(defun tdd-mode-get-runner ()
  "Return the configured test runner command."
  (pcase tdd-mode-test-runner
    ('pytest (concat (tdd-mode-get-python-executable) " -m pytest --color=yes"))
    ('nosetests (concat (tdd-mode-get-python-executable) " -m nose"))
    ('django (concat (tdd-mode-get-python-executable) " manage.py test"))
    (_ (error "Unsupported test runner"))))

(defun tdd-mode-get-python-executable ()
  "Get Python executable path from the current environment or virtualenv."
  (or (executable-find "python")
      (executable-find "python3")
      "python3"))

(defun tdd-mode-copy-test-command-to-clipboard ()
  "Copy the test command at point to the clipboard without running it."
  (interactive)
  (let ((test-command (tdd-mode-get-test-command)))
    (tdd-mode-log "Copying test command to clipboard: %s" test-command)
    (setq tdd-mode-last-test-command test-command)
    (kill-new test-command)
    (message "Copied test command to clipboard: %s" test-command)))

(defun tdd-mode-insert-pudb-breakpoint ()
  "Insert a pudb breakpoint at the current line and disable tdd-mode."
  (interactive)
  (tdd-mode-log "Inserting pudb breakpoint")
  (insert "import pudb; pudb.set_trace() # fmt: off")
  (tdd-mode -1))

(defun tdd-mode-insert-ipdb-breakpoint ()
  "Insert an ipdb breakpoint at the current line and disable tdd-mode."
  (interactive)
  (tdd-mode-log "Inserting ipdb breakpoint")
  (insert "import ipdb; ipdb.set_trace() # fmt: off")
  (tdd-mode -1))

(defun tdd-mode-display-test-output-buffer ()
  "Display the test output buffer and scroll to the end if enabled.
Non-nil tdd-mode-scroll-output scrolls the buffer to the end."
  (let ((buffer (get-buffer-create tdd-mode-test-buffer)))
    (tdd-mode-log "Displaying test output buffer")
    (display-buffer buffer '((display-buffer-reuse-window
                             display-buffer-in-side-window)
                            (side . right)
                            (slot . 0)
                            (window-width . 0.5)))
    (when tdd-mode-scroll-output
      (with-current-buffer buffer
        (goto-char (point-max))))))

(defun tdd-mode--compilation-filter ()
  "Process the output of the compilation buffer."
  (when (derived-mode-p 'tdd-mode-compilation-mode)
    (let ((inhibit-read-only t))
      (tdd-mode-log "Processing compilation output")
      (ansi-color-apply-on-region compilation-filter-start (point-max))
      (when tdd-mode-scroll-output
        (goto-char (point-max))
        (with-selected-window (get-buffer-window tdd-mode-test-buffer)
          (set-window-point (selected-window) (point-max)))))))

(defun tdd-mode--compilation-exit-message (process-status exit-status msg)
  "Handle the exit message of the compilation process.
PROCESS-STATUS is a symbol describing how the process finished.
EXIT-STATUS is the exit code or signal number.
MSG is the message string."
  (let ((exit-code (if (numberp exit-status) exit-status 1)))
    (tdd-mode-log "Compilation process exited with status: %s, exit code: %s, message: %s" process-status exit-code msg)
    (tdd-mode-update-mode-line exit-code)
    (tdd-mode-notify exit-code)
    (cons msg exit-status)))

(define-derived-mode tdd-mode-compilation-mode compilation-mode "TDD-Compilation"
  "Compilation mode for TDD Mode."
  (setq-local compilation-error-regexp-alist
              '(("\\([^ \t\n]+\\):\\([0-9]+\\)" 1 2))))

(defun tdd-mode-run-test (command)
  "Run the test COMMAND using \"compilation-mode\" and ensure the output scrolls."
  (interactive)
  (tdd-mode-log "Running test command: %s" command)
  (setq tdd-mode-last-test-command command)
  (setq tdd-mode-last-test-exit-code nil)
  (let ((compilation-buffer-name-function (lambda (_)
                                           tdd-mode-test-buffer))
        (default-directory (tdd-mode-get-project-root))
        (compilation-scroll-output tdd-mode-scroll-output)
        (compilation-environment '("TERM=xterm-256color" "PYTHONUNBUFFERED=1")))
    (let ((compilation-buffer
           (compilation-start command 'tdd-mode-compilation-mode)))
      (with-current-buffer compilation-buffer
        (setq-local compilation-exit-message-function #'tdd-mode--compilation-exit-message)
        (add-hook 'compilation-filter-hook #'tdd-mode--compilation-filter nil t)))
    (tdd-mode-display-test-output-buffer)))

(defun tdd-mode-notify (exit-code)
  "Notify user based on EXIT-CODE and user preferences."
  (let ((msg (if (eq exit-code 0) "✅ Tests Passed!" "❌ Tests Failed!")))
    (tdd-mode-log "Notifying user with message: %s" msg)
    (cond
     ((and tdd-mode-notify-on-pass (eq exit-code 0))
      (if tdd-mode-alert-enabled
          (alert msg :title "TDD Mode" :severity 'normal)
        (message msg)))
     ((and tdd-mode-notify-on-fail (not (eq exit-code 0)))
      (if tdd-mode-alert-enabled
          (alert msg :title "TDD Mode" :severity 'high)
        (message msg))))))

(defun tdd-mode-run-test-at-point ()
  "Run the test at point (function, class, or file level)."
  (interactive)
  (let ((test-command (tdd-mode-get-test-command)))
    (tdd-mode-log "Running test at point with command: %s" test-command)
    (tdd-mode-run-test test-command)))

(defun tdd-mode-run-last-test ()
  "Run the last executed test command."
  (interactive)
  (if tdd-mode-last-test-command
      (progn
        (tdd-mode-log "Running last test command '%s'" tdd-mode-last-test-command)
        (tdd-mode-run-test tdd-mode-last-test-command))
    (message "No last test command to run.")))

(defun tdd-mode-run-all-tests ()
  "Run all tests in the project."
  (interactive)
  (let ((command (format "%s %s" (tdd-mode-get-runner) (tdd-mode-get-project-root))))
    (tdd-mode-log "Running all tests with command '%s'" command)
    (tdd-mode-run-test command)))

(defun tdd-mode-run-relevant-tests ()
  "Run tests relevant to the change in the git diff.
Only Python test files are included."
  (interactive)
  (let* ((project-root (tdd-mode-get-project-root))
         (default-directory project-root)
         (changed-files (shell-command-to-string
                         "git diff --name-only --diff-filter=AM HEAD^ | grep 'tests/.*\\.py$'"))
         (test-files (split-string changed-files "\n" t)))
    (tdd-mode-log "Found relevant test files: %s" test-files)
    (if test-files
        (let ((command (format "%s %s %s"
                              (tdd-mode-get-runner)
                              project-root
                              (mapconcat #'identity test-files " "))))
          (tdd-mode-log "Running relevant tests with command '%s'" command)
          (tdd-mode-run-test command))
      (message "No relevant test files found."))))

(defun tdd-mode-run-file-tests ()
  "Run all tests in the current file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (runner (tdd-mode-get-runner))
         (test-command (format "%s %s" runner file-name)))
    (tdd-mode-log "Running all tests in the file: %s" test-command)
    (tdd-mode-run-test test-command)))

(defun tdd-mode-get-project-root ()
  "Detect project root based on common markers."
  (or (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun tdd-mode-is-test-related-file ()
  "Return t if current buffer is a .py file or test-related config file."
  (let ((file-name (file-name-nondirectory (or (buffer-file-name) ""))))
    (or (string-match-p "\\.py\\'" file-name)
        (member file-name '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg")))))

(defun tdd-mode-after-save-handler ()
  "Automatically rerun last test if configured and if buffer matches criteria."
  (when (and tdd-mode-auto-run-on-save
             (tdd-mode-is-test-related-file))
    (tdd-mode-log "Buffer saved, rerunning last test")
    (tdd-mode-run-last-test)))

(defun tdd-mode-copy-output-to-clipboard ()
  "Copy the test output to the clipboard."
  (interactive)
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (tdd-mode-log "Copied test output to clipboard")
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

(defun tdd-mode-copy-diff-and-output ()
  "Copy the git diff of the project and the test output to the clipboard."
  (interactive)
  (let* ((project-root (tdd-mode-get-project-root))
         (default-directory project-root)
         (diff (shell-command-to-string "git diff"))
         (test-output (if (get-buffer tdd-mode-test-buffer)
                          (with-current-buffer tdd-mode-test-buffer
                            (buffer-substring-no-properties (point-min) (point-max)))
                        "No test output found.")))
    (tdd-mode-log "Copied diff and test output to clipboard")
    (kill-new (format "Diff:\n\n%s\n\nTest output:\n\n%s" diff test-output))
    (message "Copied diff and test output to clipboard.")))

(defun tdd-mode-reset-mode-line-color ()
  "Reset the mode-line color to the original background color."
  (tdd-mode-log "Resetting mode-line color to original")
  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg))

(defun tdd-mode-apply-color-to-buffer (&rest _)
  "Reapply the mode-line color from last test result when switching buffers."
  (when (and tdd-mode tdd-mode-blink-enabled (not tdd-mode-blinking-in-progress))
    (let ((color (cond
                  ((null tdd-mode-last-test-exit-code)
                   tdd-mode-original-mode-line-bg)
                  ((eq tdd-mode-last-test-exit-code 0)
                   tdd-mode-blink-pass-color)
                  (t
                   tdd-mode-blink-fail-color))))
      (tdd-mode-log "Reapplying mode-line color: %s" color)
      (tdd-mode-set-mode-line-color color))))

(defun tdd-mode-buffer-change-hook ()
  "Handle buffer changes and ensure mode-line color is consistent."
  (when (and tdd-mode tdd-mode-blink-enabled)
    (tdd-mode-log "Buffer change detected, applying color")
    (tdd-mode-apply-color-to-buffer)))

;;;###autoload
(define-minor-mode tdd-mode
  "Test-Driven Development mode for Python in Emacs."
  :lighter " TDD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") tdd-mode-command-map)
            map)
  :global t
  (if tdd-mode
      (progn
        (tdd-mode-log "TDD Mode activated")
        (setq tdd-mode-original-mode-line-bg (face-background 'mode-line))
        (add-hook 'after-save-hook #'tdd-mode-after-save-handler)
        (message "[tdd-mode] TDD Mode activated"))
    (tdd-mode-log "TDD Mode deactivated")
    (remove-hook 'after-save-hook #'tdd-mode-after-save-handler)
    (when (timerp tdd-mode-fade-timer)
      (cancel-timer tdd-mode-fade-timer)
      (setq tdd-mode-fade-timer nil))
    (tdd-mode-reset-mode-line-color)
    (setq tdd-mode-last-test-exit-code nil)
    (message "[tdd-mode] TDD Mode deactivated")))

(provide 'tdd-mode)
;;; tdd-mode.el ends here
