;;; tdd-mode.el --- Modern TDD Mode for Python -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)

(defgroup tdd-mode nil
  "Test-Driven Development mode for Python projects in Emacs."
  :group 'tools
  :prefix "tdd-mode-")

(defcustom tdd-mode-test-runner 'pytest
  "Test runner to use for TDD mode."
  :type '(choice (const :tag "Pytest" pytest)
                 (const :tag "Nosetests" nosetests)
                 (const :tag "Django" django))
  :group 'tdd-mode)

(defcustom tdd-mode-notify-on-pass t
  "Notify when a test passes."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-notify-on-fail t
  "Notify when a test fails."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-auto-run-on-save t
  "Automatically rerun the last test command on save."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-buffer-popup t
  "If non-nil, displays the `*tdd-output*` buffer after each test run.
If set to nil, keeps the buffer in the background."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-verbose t
  "Toggle verbose debug output for TDD Mode."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-blink-enabled t
  "If non-nil, enables mode-line blinking on test failures."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-blink-fail-color "#F44336"
  "Color for the mode-line when a test fails."
  :type 'string
  :group 'tdd-mode)

(defcustom tdd-mode-blink-interval 0.6
  "Interval in seconds for the mode-line blinking effect."
  :type 'number
  :group 'tdd-mode)

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "Stores the last test command used in TDD mode.")

(defvar tdd-mode-original-mode-line-bg (face-background 'mode-line)
  "Original mode-line background color.")

(defvar tdd-mode-failure-timer nil
  "Timer to control mode-line blinking for test failures.")

(defvar tdd-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'tdd-mode-run-test-at-point)
    (define-key map (kbd "a") 'tdd-mode-run-all-tests)
    (define-key map (kbd "r") 'tdd-mode-run-last-test)
    (define-key map (kbd "c") 'tdd-mode-copy-output-to-clipboard)
    map)
  "Keymap for `tdd-mode` commands.")

;; Optional alert package
(if (require 'alert nil 'noerror)
    (defvar tdd-mode-alert-enabled t
      "Non-nil if `alert` package is available.")
  (defvar tdd-mode-alert-enabled nil
    "Nil if `alert` package is unavailable; fallback to `message` notifications."))

(defun tdd-mode-log (message &rest args)
  "Log MESSAGE with ARGS if `tdd-mode-verbose` is enabled."
  (when tdd-mode-verbose
    (message "[tdd-mode] %s" (apply 'format message args))))

(defun tdd-mode-set-mode-line-color (color)
  "Set the mode-line background color to COLOR."
  (set-face-background 'mode-line color))

(defun tdd-mode-blink-failure-mode-line ()
  "Blink the mode-line color to indicate test failure."
  (when (timerp tdd-mode-failure-timer)
    (cancel-timer tdd-mode-failure-timer))
  (when tdd-mode-blink-enabled
    (let ((blink-color tdd-mode-blink-fail-color))
      (setq tdd-mode-failure-timer
            (run-with-timer 0 tdd-mode-blink-interval
                            (lambda ()
                              (tdd-mode-set-mode-line-color
                               (if (eq (face-background 'mode-line) blink-color)
                                   tdd-mode-original-mode-line-bg
                                 blink-color))))))))

(defun tdd-mode-reset-mode-line-color ()
  "Reset mode-line background to original color and stop blinking."
  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg)
  (when (timerp tdd-mode-failure-timer)
    (cancel-timer tdd-mode-failure-timer)))

(defun tdd-mode--get-current-defun ()
  "Retrieve the name of the current function for constructing a pytest command."
  (save-excursion
    (when (python-nav-beginning-of-defun)
      (let ((full-name (python-info-current-defun)))
        (when full-name
          (car (last (split-string full-name "\\."))))))))

(defun tdd-mode-get-test-command ()
  "Get test command based on position (function, class, or file)."
  (let* ((file-name (buffer-file-name))
         (func-name (tdd-mode--get-current-defun))
         (class-name (tdd-mode--get-class-name))
         (runner (tdd-mode-get-runner)))
    (tdd-mode-log "File name = %s" file-name)
    (tdd-mode-log "Class name = %s" (or class-name "nil"))
    (tdd-mode-log "Function name = %s" (or func-name "nil"))
    (cond
     ((and func-name class-name)
      (format "%s %s::%s::%s" runner file-name class-name func-name))
     (func-name
      (format "%s %s::%s" runner file-name func-name))
     (class-name
      (format "%s %s::%s" runner file-name class-name))
     (file-name
      (format "%s %s" runner file-name))
     (t
      (error "Unable to determine test command at point")))))

(defun tdd-mode--get-class-name ()
  "Retrieve the class name at point."
  (save-excursion
    (re-search-backward "^class \\([A-Za-z0-9_]+\\)" nil t)
    (let ((class-name (match-string 1)))
      (tdd-mode-log "Found class name: %s" class-name)
      class-name)))

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

(defun tdd-mode-run-test (command)
  "Run test COMMAND and display results in `tdd-mode-test-buffer`."
  (interactive)
  (setq tdd-mode-last-test-command command)
  (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "Running test command: %s\n\n" command))
    (let ((exit-code (call-process-shell-command command nil tdd-mode-test-buffer t)))
      (tdd-mode-display-output)
      (tdd-mode-update-mode-line exit-code)
      (tdd-mode-notify exit-code)
      (setq buffer-read-only t))))

(defun tdd-mode-run-test-at-point ()
  "Run the test at point (function, class, or file level)."
  (interactive)
  (let ((test-command (tdd-mode-get-test-command)))
    (tdd-mode-log "Running test at point with command `%s`" test-command)
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

(defun tdd-mode-get-project-root ()
  "Detect project root based on common markers."
  (or (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun tdd-mode-display-output ()
  "Display the test output buffer based on `tdd-mode-buffer-popup` setting, applying ansi color."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))
    (if tdd-mode-buffer-popup
        (display-buffer (current-buffer))
      (bury-buffer))))

(defun tdd-mode-update-mode-line (exit-code)
  "Update the mode line based on EXIT-CODE."
  (if (eq exit-code 0)
      (tdd-mode-reset-mode-line-color)
    (tdd-mode-blink-failure-mode-line)))

(defun tdd-mode-notify (exit-code)
  "Notify user based on EXIT-CODE and user preferences."
  (let ((msg (if (eq exit-code 0) "✅ Tests Passed!" "❌ Tests Failed!")))
    (cond
     ((and tdd-mode-notify-on-pass (eq exit-code 0))
      (if tdd-mode-alert-enabled
          (alert msg :title "TDD Mode" :severity 'normal)
        (message msg)))
     ((and tdd-mode-notify-on-fail (not (eq exit-code 0)))
      (if tdd-mode-alert-enabled
          (alert msg :title "TDD Mode" :severity 'high)
        (message msg))))))

(defun tdd-mode-after-save-handler ()
  "Automatically rerun last test if configured and if buffer matches file criteria."
  (when (and tdd-mode-auto-run-on-save
             (string-match-p "\\.py\\'" (buffer-file-name)))
    (tdd-mode-run-last-test)))

(defun tdd-mode-copy-output-to-clipboard ()
  "Copy the test output to the clipboard."
  (interactive)
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

;;;###autoload
(define-minor-mode tdd-mode
  "Test-Driven Development mode for Python in Emacs."
  :lighter " TDD"
  :keymap tdd-mode-command-map
  (if tdd-mode
      (progn
        (setq tdd-mode-original-mode-line-bg (face-background 'mode-line))
        (add-hook 'after-save-hook 'tdd-mode-after-save-handler nil t)
        (message "[tdd-mode] TDD Mode activated"))
    (remove-hook 'after-save-hook 'tdd-mode-after-save-handler t)
    (tdd-mode-reset-mode-line-color)
    (message "[tdd-mode] TDD Mode deactivated")))

(provide 'tdd-mode)
;;; tdd-mode.el ends here
