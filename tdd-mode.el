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

(defvar tdd-mode-watch-files '("\\.py\\'" "\\.toml\\'")
  "List of regex patterns for file extensions that trigger test re-run on save.")

(defvar tdd-mode-original-mode-line-bg (face-background 'mode-line)
  "Store the original mode-line background color.")

(defvar tdd-mode-buffer-popup t
  "Controls whether the `*tdd-output*` buffer should pop up.
If non-nil, the buffer will pop up. If nil, it will run in the background.")

(defvar tdd-mode-project-root nil
  "Stores the project root where `tdd-mode` was last activated.")

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

(defun tdd-mode-get-project-root ()
  "Detect and return the project root directory based on common project markers.
If the buffer is not associated with a file, return nil."
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
    (message "[tdd-mode] Using pytest executable: %s" pytest-executable)
    (cond
     ((eq tdd-mode-test-runner 'pytest)
      (when file-name
        (cond
         ((and class-name function-name)
          (format "%s --color=yes %s::%s::%s" pytest-executable file-name class-name function-name))
         (function-name
          (format "%s --color=yes %s::%s" pytest-executable file-name function-name))
         (t (format "%s --color=yes %s" pytest-executable file-name)))))
     ((eq tdd-mode-test-runner 'nosetests)
      (when (and file-name function-name)
        (format "nosetests %s:%s" file-name function-name)))
     ((eq tdd-mode-test-runner 'django)
      (when file-name
        (format "%s manage.py test %s" (tdd-mode-get-python-executable) file-name)))
     (t (error "Unsupported test runner")))))

(defun tdd-mode-get-class-name-at-point ()
  "Retrieve the name of the class at point if the cursor is within a Python class."
  (save-excursion
    (let ((class-name nil))
      (beginning-of-defun)
      (when (re-search-backward "^\s*class\s+\$begin:math:text$[A-Za-z0-9_]+\\$end:math:text$" nil t)
        (setq class-name (match-string 1)))
      class-name)))

(defun tdd-mode-get-function-name-at-point ()
  "Retrieve the name of the function at point if the cursor is within a Python function."
  (save-excursion
    (let ((function-name nil))
      (beginning-of-defun)
      (when (re-search-forward "def \$begin:math:text$[a-zA-Z0-9_]+\\$end:math:text$" nil t)
        (setq function-name (match-string 1)))
      function-name)))

(defun tdd-mode-run-test (&optional command)
  "Run the given test COMMAND or the last test if no COMMAND is provided."
  (interactive)
  (let ((test-command (or command tdd-mode-last-test-command)))
    (when test-command
      (setq tdd-mode-last-test-command test-command)
      (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
        (setq buffer-read-only nil) ;; Allow writing in the buffer
        (erase-buffer)
        (insert (concat "$ " test-command "\n\n"))
        (let ((exit-code (call-process-shell-command test-command nil tdd-mode-test-buffer t)))
          (tdd-mode-apply-ansi-color)
          (setq buffer-read-only t) ;; Set buffer as read-only after output
          (tdd-mode-display-buffer) ;; Display output based on popup setting
          (tdd-mode-update-status exit-code)
          (tdd-mode-popup-notification (if (eq exit-code 0) "Tests Passed" "Tests Failed")
                                       (if (eq exit-code 0) "#d0ffd0" "#ffd0d0"))
          (tdd-mode-notify exit-code)
          (tdd-mode-log-last-test exit-code))))))

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
  "Run all tests from the project root."
  (interactive)
  (let ((pytest-executable (tdd-mode-get-pytest-executable))
        (project-root (tdd-mode-get-project-root)))
    (if (and pytest-executable project-root)
        (tdd-mode-run-test (format "%s --color=yes %s" pytest-executable project-root))
      (message "Cannot determine project root or pytest executable."))))

(defun tdd-mode-apply-ansi-color ()
  "Apply ANSI color codes in the test buffer for improved readability."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun tdd-mode-notify (exit-code)
  "Send a notification based on the EXIT-CODE of the test."
  (let ((msg (if (eq exit-code 0) "✅ Test passed!" "❌ Test failed!"))
        (severity (if (eq exit-code 0) 'normal 'high)))
    (alert msg :title "TDD Mode" :severity severity)))

(defun tdd-mode-log-last-test (exit-code)
  "Log the last test command with the EXIT-CODE and timestamp."
  (let ((log-file (concat (or (tdd-mode-get-project-root) "") ".tdd-mode-log")))
    (append-to-file (format "[%s] %s - %s\n"
                            (format-time-string "%Y-%m-%d %H:%M:%S")
                            (if (eq exit-code 0) "PASSED" "FAILED")
                            tdd-mode-last-test-command)
                    nil
                    log-file)))

(defun tdd-mode-run-last-test-on-save ()
  "Re-run the last test command if a relevant file in the project is saved."
  (when (and tdd-mode-last-test-command
             (tdd-mode-same-project-p))
    (tdd-mode-run-test tdd-mode-last-test-command)))

(defun tdd-mode-same-project-p ()
  "Check if the current buffer's project root matches `tdd-mode-project-root`."
  (let ((current-root (tdd-mode-get-project-root)))
    (and current-root
         (string= current-root tdd-mode-project-root))))

(defun tdd-mode-should-trigger-test-reload (filename)
  "Check if a FILENAME matches the patterns in `tdd-mode-watch-files`."
  (cl-some (lambda (pattern) (string-match-p pattern filename))
           tdd-mode-watch-files))

(defun tdd-mode-after-save-handler ()
  "Run tests if the saved file matches a pattern in `tdd-mode-watch-files`."
  (when (and buffer-file-name
             (tdd-mode-should-trigger-test-reload buffer-file-name))
    (tdd-mode-run-last-test-on-save)))

(add-hook 'after-save-hook #'tdd-mode-after-save-handler)

(defun tdd-mode-copy-output ()
  "Copy the contents of the test output buffer to the clipboard."
  (interactive)
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

;;;###autoload
(define-minor-mode tdd-mode
  "Enhanced TDD mode for Python development with real-time feedback and notifications."
  :lighter " TDD"
  (if tdd-mode
      (progn
        (setq tdd-mode-project-root (tdd-mode-get-project-root))
        (add-hook 'after-save-hook #'tdd-mode-after-save-handler nil t)
        (message "tdd-mode activated"))
    (remove-hook 'after-save-hook #'tdd-mode-after-save-handler t)
    (tdd-mode-reset-mode-line-color)
    (message "tdd-mode deactivated")))

;;;###autoload
(define-globalized-minor-mode global-tdd-mode tdd-mode
  (lambda ()
    (when (derived-mode-p 'python-mode)
      (tdd-mode 1))))

(provide 'tdd-mode)

;;; tdd-mode.el ends here
