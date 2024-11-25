;;; tdd-mode.el --- Modern TDD Mode for Python -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'python)
(require 'compile)

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
  "If non-nil, enables mode-line blinking on test failures and success."
  :type 'boolean
  :group 'tdd-mode)

(defcustom tdd-mode-blink-fail-color "#F44336"
  "Color for the mode-line when a test fails."
  :type 'string
  :group 'tdd-mode)

(defcustom tdd-mode-blink-pass-color "#4CAF50"
  "Color for the mode-line when a test passes."
  :type 'string
  :group 'tdd-mode)

(defcustom tdd-mode-blink-steps 10
  "Number of steps for the mode-line fade effect."
  :type 'integer
  :group 'tdd-mode)

(defcustom tdd-mode-blink-interval 0.1
  "Interval in seconds between each fade step."
  :type 'number
  :group 'tdd-mode)

(defcustom tdd-mode-scroll-output t
  "If non-nil, the compilation buffer will automatically scroll to follow output."
  :type 'boolean
  :group 'tdd-mode)

(defvar tdd-mode-test-buffer "*tdd-output*"
  "Buffer for displaying test output.")

(defvar tdd-mode-last-test-command nil
  "Stores the last test command used in TDD mode.")

(defvar tdd-mode-last-test-exit-code nil
  "Stores the last exit code to show status across buffers.")

(defvar tdd-mode-original-mode-line-bg (face-background 'mode-line)
  "Original mode-line background color.")

(defvar tdd-mode-fade-timer nil
  "Timer to control mode-line fading.")

(defvar tdd-mode-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'tdd-mode-run-test-at-point)
    (define-key map (kbd "a") 'tdd-mode-run-all-tests)
    (define-key map (kbd "l") 'tdd-mode-run-last-test)
    (define-key map (kbd "c") 'tdd-mode-copy-output-to-clipboard)
    (define-key map (kbd "r") 'tdd-mode-run-relevant-tests)
    (define-key map (kbd "f") 'tdd-mode-run-file-tests)
    (define-key map (kbd "p") 'tdd-mode-copy-test-command-to-clipboard)
    (define-key map (kbd "b") 'tdd-mode-insert-ipdb-breakpoint)
    (define-key map (kbd "B") 'tdd-mode-insert-pudb-breakpoint)
    (define-key map (kbd "C") 'tdd-mode-copy-diff-and-output)
    map)
  "Keymap for `tdd-mode` commands.")

(defvar tdd-mode-prefix-map (make-sparse-keymap)
  "Prefix map for TDD Mode commands.")
(define-key tdd-mode-prefix-map (kbd "t") tdd-mode-command-map)
(define-key tdd-mode-prefix-map (kbd "C-c t") tdd-mode-command-map)

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
  (tdd-mode-log "Setting mode-line color to %s" color)
  (set-face-background 'mode-line color))

(defun tdd-mode-blink-mode-line (color)
  "Blink the mode-line by fading from COLOR to the original background."
  (tdd-mode-log "Blinking mode-line with color %s" color)
  (when (timerp tdd-mode-fade-timer)
    (cancel-timer tdd-mode-fade-timer)
    (tdd-mode-log "Cancelled existing fade timer"))
  (let* ((start-color (color-name-to-rgb color))
         (end-color (color-name-to-rgb tdd-mode-original-mode-line-bg))
         (step-colors (tdd-mode-generate-fade-colors start-color end-color tdd-mode-blink-steps)))
    (tdd-mode-log "Start color: %s, End color: %s, Step colors: %s" start-color end-color step-colors)
    (setq tdd-mode-fade-timer
          (run-with-timer 0 tdd-mode-blink-interval
                          (lambda ()
                            (if (null step-colors)
                                (progn
                                  (cancel-timer tdd-mode-fade-timer)
                                  (setq tdd-mode-fade-timer nil)
                                  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg)
                                  (tdd-mode-log "Fade complete, resetting mode-line color to original"))
                              (tdd-mode-set-mode-line-color (pop step-colors))))))))

(defun tdd-mode-generate-fade-colors (start-color end-color steps)
  "Generate a list of colors fading from START-COLOR to END-COLOR in STEPS."
  (tdd-mode-log "Generating fade colors from %s to %s in %d steps" start-color end-color steps)
  (cl-loop for i from 0 below steps
           collect (apply 'color-rgb-to-hex
                          (cl-mapcar (lambda (start end)
                                       (+ start (* i (/ (- end start) (float steps)))))
                                     start-color end-color))))

(defun tdd-mode-update-mode-line (exit-code)
  "Update the mode-line color based on the last test EXIT-CODE."
  (tdd-mode-log "Updating mode-line with exit code %s" exit-code)
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
    (tdd-mode-log "Inner testable: %s, Outer testable: %s" inner-obj outer)
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
    (tdd-mode-log "File name: %s, Runner: %s, Test entity: %s" file-name runner test-entity)
    (if test-entity
        (format "%s %s::%s" runner file-name test-entity)
      (format "%s %s" runner file-name))))

(defun tdd-mode-get-runner ()
  "Return the configured test runner command."
  (tdd-mode-log "Getting test runner")
  (pcase tdd-mode-test-runner
    ('pytest (concat (tdd-mode-get-python-executable) " -m pytest --color=yes"))
    ('nosetests (concat (tdd-mode-get-python-executable) " -m nose"))
    ('django (concat (tdd-mode-get-python-executable) " manage.py test"))
    (_ (error "Unsupported test runner"))))

(defun tdd-mode-get-python-executable ()
  "Get Python executable path from the current environment or virtualenv."
  (tdd-mode-log "Getting Python executable")
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
  "Display the test output buffer and ensure it's scrolled to the end."
  (tdd-mode-log "Displaying test output buffer")
  (let ((buffer (get-buffer-create tdd-mode-test-buffer)))
    ;; Display the buffer without switching focus
    (display-buffer buffer '((display-buffer-reuse-window
                              display-buffer-in-side-window)
                             (side . right)
                             (slot . 0)
                             (window-width . 0.5)))
    ;; Scroll to the end of the buffer if enabled
    (when tdd-mode-scroll-output
      (with-current-buffer buffer
        (goto-char (point-max))))))

(defun tdd-mode--compilation-filter ()
  "Process the output of the compilation buffer."
  (tdd-mode-log "Processing compilation filter")
  (when (eq major-mode 'tdd-mode-compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))
      (when tdd-mode-scroll-output
        ;; Scroll to the end of the buffer
        (goto-char (point-max))
        ;; Ensure window scrolls with point
        (with-selected-window (get-buffer-window tdd-mode-test-buffer)
          (set-window-point (selected-window) (point-max)))))))

(defun tdd-mode--compilation-exit-message (process-status exit-status msg)
  "Handle the exit message of the compilation process.
PROCESS-STATUS is a symbol describing how the process finished.
EXIT-STATUS is the exit code or signal number.
MSG is the message string."
  (tdd-mode-log "Handling compilation exit message: %s, %s, %s" process-status exit-status msg)
  (let ((exit-code (if (numberp exit-status) exit-status 1)))
    (tdd-mode-update-mode-line exit-code)
    (tdd-mode-notify exit-code)
    ;; Return the default message
    (cons msg exit-status)))

(define-derived-mode tdd-mode-compilation-mode compilation-mode "TDD-Compilation"
  "Compilation mode for TDD Mode."
  (setq-local compilation-error-regexp-alist
              '(("\\([^ \t\n]+\\):\\([0-9]+\\)" 1 2))))

(defun tdd-mode-run-test (command)
  "Run the test COMMAND using compilation-mode and ensure the output scrolls."
  (interactive)
  (tdd-mode-log "Running test command: %s" command)
  (setq tdd-mode-last-test-command command)
  (setq tdd-mode-last-test-exit-code nil)
  (let ((compilation-buffer-name-function (lambda (mode)
                                            tdd-mode-test-buffer))
        (default-directory (tdd-mode-get-project-root))
        (compilation-scroll-output tdd-mode-scroll-output)
        ;; Set environment variables for color support
        (compilation-environment '("TERM=xterm-256color" "PYTHONUNBUFFERED=1")))
    (let ((compilation-buffer
           (compilation-start command 'tdd-mode-compilation-mode)))
      ;; Apply ANSI colors and scroll to the end
      (with-current-buffer compilation-buffer
        (setq-local compilation-exit-message-function #'tdd-mode--compilation-exit-message)
        (add-hook 'compilation-filter-hook 'tdd-mode--compilation-filter nil t)))
    ;; Display the test output buffer without switching focus
    (tdd-mode-display-test-output-buffer)))

(defun tdd-mode-notify (exit-code)
  "Notify user based on EXIT-CODE and user preferences."
  (tdd-mode-log "Notifying user with exit code: %s" exit-code)
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

(defun tdd-mode-run-test-at-point ()
  "Run the test at point (function, class, or file level)."
  (interactive)
  (tdd-mode-log "Running test at point")
  (let ((test-command (tdd-mode-get-test-command)))
    (tdd-mode-run-test test-command)))

(defun tdd-mode-run-last-test ()
  "Run the last executed test command."
  (interactive)
  (tdd-mode-log "Running last test command")
  (if tdd-mode-last-test-command
      (progn
        (tdd-mode-log "Running last test command '%s'" tdd-mode-last-test-command)
        (tdd-mode-run-test tdd-mode-last-test-command))
    (message "No last test command to run.")))

(defun tdd-mode-run-all-tests ()
  "Run all tests in the project."
  (interactive)
  (tdd-mode-log "Running all tests")
  (let ((command (format "%s %s" (tdd-mode-get-runner) (tdd-mode-get-project-root))))
    (tdd-mode-log "Running all tests with command '%s'" command)
    (tdd-mode-run-test command)))

(defun tdd-mode-run-relevant-tests ()
  "Run the tests relevant to the changes in the git diff, only running Python test files."
  (interactive)
  (tdd-mode-log "Running relevant tests")
  (let* ((project-root (tdd-mode-get-project-root))
         (default-directory project-root)
         (changed-files (shell-command-to-string
                         "git diff --name-only --diff-filter=AM HEAD^ | grep 'tests/.*\\.py$'"))
         (test-files (split-string changed-files "\n" t)))
    (if test-files
        (let ((command (format "%s %s %s"
                               (tdd-mode-get-runner)
                               project-root
                               (mapconcat 'identity test-files " "))))
          (tdd-mode-log "Running relevant tests with command '%s'" command)
          (tdd-mode-run-test command))
      (message "No relevant test files found."))))

(defun tdd-mode-run-file-tests ()
  "Run all tests in the current file."
  (interactive)
  (tdd-mode-log "Running all tests in the current file")
  (let* ((file-name (buffer-file-name))
         (runner (tdd-mode-get-runner))
         (test-command (format "%s %s" runner file-name)))
    (tdd-mode-log "Running all tests in the file: `%s`" test-command)
    (tdd-mode-run-test test-command)))

(defun tdd-mode-get-project-root ()
  "Detect project root based on common markers."
  (tdd-mode-log "Getting project root")
  (or (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun tdd-mode-is-test-related-file ()
  "Return t if current buffer is a .py file or test-related config file."
  (tdd-mode-log "Checking if current buffer is test-related")
  (let ((file-name (file-name-nondirectory (or (buffer-file-name) ""))))
    (or (string-match-p "\\.py\\'" file-name)
        (member file-name '("pyproject.toml" "pytest.ini" "tox.ini" "setup.cfg")))))

(defun tdd-mode-after-save-handler ()
  "Automatically rerun last test if configured and if buffer matches criteria."
  (tdd-mode-log "Running after-save handler")
  (when (and tdd-mode-auto-run-on-save
             (tdd-mode-is-test-related-file))
    (tdd-mode-run-last-test)))

(defun tdd-mode-copy-output-to-clipboard ()
  "Copy the test output to the clipboard."
  (interactive)
  (tdd-mode-log "Copying test output to clipboard")
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

(defun tdd-mode-copy-diff-and-output ()
  "Copy the git diff of the project and the test output to the clipboard."
  (interactive)
  (tdd-mode-log "Copying diff and test output to clipboard")
  (let* ((project-root (tdd-mode-get-project-root))
         (default-directory project-root)
         (diff (shell-command-to-string "git diff"))
         (test-output (if (get-buffer tdd-mode-test-buffer)
                          (with-current-buffer tdd-mode-test-buffer
                            (buffer-substring-no-properties (point-min) (point-max)))
                        "No test output found.")))
    (kill-new (format "Diff:\n\n%s\n\nTest output:\n\n%s" diff test-output))
    (message "Copied diff and test output to clipboard.")))

(defun tdd-mode-apply-color-to-buffer (&rest _)
  "Reapply the mode-line color based on the last test result when switching buffers."
  (tdd-mode-log "Applying color to buffer")
  (when (and tdd-mode tdd-mode-blink-enabled)
    (let ((color (cond
                  ((null tdd-mode-last-test-exit-code)
                   tdd-mode-original-mode-line-bg)
                  ((eq tdd-mode-last-test-exit-code 0)
                   tdd-mode-blink-pass-color)
                  (t
                   tdd-mode-blink-fail-color))))
      (tdd-mode-set-mode-line-color color))))

(defun tdd-mode-reset-mode-line-color ()
  "Reset the mode-line color to the original background color."
  (tdd-mode-log "Resetting mode-line color to original")
  (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg))

(defun tdd-mode-buffer-change-hook ()
  "Hook function to handle buffer changes and ensure mode-line color is consistent."
  (tdd-mode-log "Handling buffer change hook")
  (when (and tdd-mode tdd-mode-blink-enabled)
    (tdd-mode-apply-color-to-buffer)))

;;;###autoload
(define-minor-mode tdd-mode
  "Test-Driven Development mode for Python in Emacs."
  :lighter " TDD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") tdd-mode-command-map)
            map)
  :global t
  (if tdd-mode
      (progn
        (setq tdd-mode-original-mode-line-bg (face-background 'mode-line))
        (add-hook 'after-save-hook 'tdd-mode-after-save-handler)
        (message "[tdd-mode] TDD Mode activated"))
    ;; Deactivation code
    (remove-hook 'after-save-hook 'tdd-mode-after-save-handler)
    (when (timerp tdd-mode-fade-timer)
      (cancel-timer tdd-mode-fade-timer)
      (setq tdd-mode-fade-timer nil))
    ;; Reset the mode-line color
    (tdd-mode-reset-mode-line-color)
    ;; Reset last test exit code
    (setq tdd-mode-last-test-exit-code nil)
    (message "[tdd-mode] TDD Mode deactivated")))

(provide 'tdd-mode)
;;; tdd-mode.el ends here
