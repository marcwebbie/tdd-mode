;;; tdd-mode.el --- Enhanced TDD Mode for Python development -*- lexical-binding: t; -*-

(require 'ansi-color)
(unless (require 'alert nil 'noerror)
  (message "Warning: `alert` package not found. Notifications may not work."))

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer name for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "The last test command run by the user.")

(defvar tdd-mode-test-runner 'pytest
  "The test runner to use. Options are 'pytest, 'nosetests, and 'django.")

(defvar tdd-mode-notify-on-pass t
  "Whether to show a notification on test pass.")

(defvar tdd-mode-notify-on-fail t
  "Whether to show a notification on test fail.")

(defvar tdd-mode-buffer-popup t
  "Controls whether the `*tdd-output*` buffer should pop up.
If non-nil, the buffer will pop up. If nil, it will run in the background.")

(defvar tdd-mode-original-mode-line-bg (face-background 'mode-line)
  "Store the original mode-line background color.")

(defun tdd-mode-set-mode-line-color (color)
  "Set the mode-line background color to COLOR."
  (set-face-background 'mode-line color))

(defun tdd-mode-reset-mode-line-color ()
  "Reset the mode-line background color to its original value."
  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg))

(defun tdd-mode-display-buffer ()
  "Display the test output buffer based on `tdd-mode-buffer-popup` setting."
  (if tdd-mode-buffer-popup
      (display-buffer tdd-mode-test-buffer)
    (with-current-buffer tdd-mode-test-buffer
      (bury-buffer))))

(defun tdd-mode-update-status (exit-code)
  "Show a pass or fail indication by changing mode-line background color based on the test result EXIT-CODE."
  (if (eq exit-code 0)
      (tdd-mode-set-mode-line-color "#4CAF50")  ;; Green for pass
    (tdd-mode-set-mode-line-color "#F44336")))  ;; Red for fail

(defun tdd-mode-popup-notification (message color)
  "Display a popup with MESSAGE and background COLOR."
  (let ((popup (make-overlay (point-min) (point-min))))
    (overlay-put popup 'before-string (propertize message 'face `(:background ,color :foreground "black")))
    (run-with-timer 0.5 nil (lambda () (delete-overlay popup)))))

(defun tdd-mode-get-python-executable ()
  "Retrieve the Python executable path from the active virtual environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (if venv
        (concat venv "/bin/python")
      (error "No active virtual environment found"))))

(defun tdd-mode-get-pytest-executable ()
  "Retrieve the pytest executable from the active virtual environment."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (if (and venv (file-exists-p (concat venv "/bin/pytest")))
        (concat venv "/bin/pytest")
      (error "pytest not found in active virtual environment"))))

(defun tdd-mode-get-class-name-at-point ()
  "Retrieve the name of the class at point if the cursor is within a Python class."
  (save-excursion
    (let ((class-name nil))
      (when (re-search-backward "^[[:space:]]*class[[:space:]]+\$begin:math:text$[A-Za-z0-9_]+\\$end:math:text$" nil t)
        (setq class-name (match-string 1)))
      class-name)))

(defun tdd-mode-get-function-name-at-point ()
  "Retrieve the name of the function at point if the cursor is within a Python function."
  (save-excursion
    (let ((function-name nil))
      (when (re-search-backward "^[[:space:]]*def[[:space:]]+\$begin:math:text$[a-zA-Z0-9_]+\\$end:math:text$" nil t)
        (setq function-name (match-string 1)))
      function-name)))

(defun tdd-mode-get-project-root ()
  "Detect and return the project root directory based on common project markers."
  (if (buffer-file-name)
      (or (locate-dominating-file default-directory ".git")
          (locate-dominating-file default-directory "pyproject.toml")
          (locate-dominating-file default-directory "setup.py"))
    nil))

(defun tdd-mode-get-test-command-at-point ()
  "Generate the test command for pytest with ClassName::test_function format at point."
  (let* ((file-name (buffer-file-name))
         (function-name (tdd-mode-get-function-name-at-point))
         (class-name (tdd-mode-get-class-name-at-point))
         (pytest-executable (tdd-mode-get-pytest-executable)))
    (cond
     ;; Run function in the class at point
     ((and function-name class-name)
      (format "%s --color=yes %s::%s::%s" pytest-executable file-name class-name function-name))
     ;; Run the class if no specific function is detected
     (class-name
      (format "%s --color=yes %s::%s" pytest-executable file-name class-name))
     ;; Run the entire file if neither class nor function is detected
     (t (format "%s --color=yes %s" pytest-executable file-name)))))

(defun tdd-mode-run-test (&optional command)
  "Run the given test COMMAND or the last test if no COMMAND is provided."
  (interactive)
  (let ((test-command (or command tdd-mode-last-test-command)))
    (when test-command
      (setq tdd-mode-last-test-command test-command)
      (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (concat "$ " test-command "\n\n"))
        (let ((exit-code (call-process-shell-command test-command nil tdd-mode-test-buffer t)))
          (tdd-mode-apply-ansi-color)
          (setq buffer-read-only t)
          (tdd-mode-display-buffer)
          (tdd-mode-update-status exit-code)
          (tdd-mode-popup-notification (if (eq exit-code 0) "Tests Passed" "Tests Failed")
                                       (if (eq exit-code 0) "#d0ffd0" "#ffd0d0"))
          (when (featurep 'alert)
            (let ((msg (if (eq exit-code 0) "✅ Test passed!" "❌ Test failed!")))
              (alert msg :title "TDD Mode" :severity (if (eq exit-code 0) 'normal 'high)))))))))

(defun tdd-mode-run-test-at-point ()
  "Run the test command at the current point, if possible."
  (interactive)
  (let ((command (tdd-mode-get-test-command-at-point)))
    (if command
        (tdd-mode-run-test command)
      (message "No test command found at point."))))

(defun tdd-mode-run-test-file ()
  "Run the current test file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (pytest-executable (tdd-mode-get-pytest-executable)))
    (if file-name
        (tdd-mode-run-test (format "%s --color=yes %s" pytest-executable file-name))
      (message "No file associated with the current buffer."))))

(defun tdd-mode-run-all-tests ()
  "Run all tests in the project using the configured test runner."
  (interactive)
  (let* ((project-root (tdd-mode-get-project-root))
         (command
          (cond
           ((eq tdd-mode-test-runner 'pytest)
            (format "%s --color=yes %s" (tdd-mode-get-pytest-executable) project-root))
           ((eq tdd-mode-test-runner 'nosetests)
            (format "%s -v" (concat (tdd-mode-get-python-executable) "/bin/nosetests")))
           ((eq tdd-mode-test-runner 'django)
            (format "%s manage.py test" (tdd-mode-get-python-executable)))
           (t (error "Unsupported test runner")))))
    (tdd-mode-run-test command)))

(defun tdd-mode-copy-output ()
  "Copy the contents of the test output buffer to the clipboard."
  (interactive)
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;###autoload
(define-minor-mode tdd-mode
  "Enhanced TDD mode for Python development with real-time feedback and notifications."
  :lighter " TDD"
  (if tdd-mode
      (progn
        (unless tdd-mode-original-mode-line-bg
          (setq tdd-mode-original-mode-line-bg (face-background 'mode-line)))
        (add-hook 'after-save-hook #'tdd-mode-run-test-at-point nil t)
        (message "tdd-mode activated"))
    (remove-hook 'after-save-hook #'tdd-mode-run-test-at-point t)
    (tdd-mode-reset-mode-line-color)
    (message "tdd-mode deactivated")))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
