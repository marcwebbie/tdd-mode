;;; tdd-mode.el --- Modern TDD Mode for Python -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'python)

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
    (define-key map (kbd "r") 'tdd-mode-run-last-test)
    (define-key map (kbd "c") 'tdd-mode-copy-output-to-clipboard)
    (define-key map (kbd "d") 'tdd-mode-run-relevant-tests)  ;; New keybinding for diff-based relevant tests
    (define-key map (kbd "f") 'tdd-mode-run-file-tests)      ;; New keybinding for running tests on file/module
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
  (set-face-background 'mode-line color))

(defun tdd-mode-blink-mode-line (color)
  "Blink the mode-line by fading from COLOR to the original background."
  (when (timerp tdd-mode-fade-timer)
    (cancel-timer tdd-mode-fade-timer))
  (let* ((start-color (color-name-to-rgb color))
         (end-color (color-name-to-rgb tdd-mode-original-mode-line-bg))
         (step-colors (tdd-mode-generate-fade-colors start-color end-color tdd-mode-blink-steps)))
    (setq tdd-mode-fade-timer
          (run-with-timer 0 tdd-mode-blink-interval
                          (lambda ()
                            (if (null step-colors)
                                (cancel-timer tdd-mode-fade-timer)
                              (tdd-mode-set-mode-line-color (pop step-colors))))))))

(defun tdd-mode-generate-fade-colors (start-color end-color steps)
  "Generate a list of colors fading from START-COLOR to END-COLOR in STEPS."
  (cl-loop for i from 0 below steps
           collect (apply 'color-rgb-to-hex
                          (cl-mapcar (lambda (start end)
                                       (+ start (* i (/ (- end start) (float steps)))))
                                     start-color end-color))))

(defun tdd-mode-update-mode-line (exit-code)
  "Update the mode line based on EXIT-CODE."
  (setq tdd-mode-last-test-exit-code exit-code)
  (let ((color (if (eq exit-code 0) tdd-mode-blink-pass-color tdd-mode-blink-fail-color)))
    (tdd-mode-blink-mode-line color)))

(defun tdd-mode--get-testable ()
  "Get the testable entity at point (function, class, or file).
Adapted from pytest.el to mimic its test resolution."
  (let* ((inner-obj (tdd-mode--inner-testable))
         (outer (tdd-mode--outer-testable))
         (outer-def (car outer))
         (outer-obj (cdr outer)))
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

(defun tdd-mode-run-test (command)
  "Run test COMMAND and display results in `tdd-mode-test-buffer`."
  (interactive)
  (let ((default-directory (tdd-mode-get-project-root))) ;; set project root
    (setq tdd-mode-last-test-command command)
    (with-current-buffer (get-buffer-create tdd-mode-test-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Running test command: %s\n\n" command))
      (let ((exit-code (call-process-shell-command command nil tdd-mode-test-buffer t)))
        (tdd-mode-display-output)
        (tdd-mode-update-mode-line exit-code)
        (tdd-mode-notify exit-code)
        (setq buffer-read-only t)))))

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

(defun tdd-mode-run-relevant-tests ()
  "Run the tests relevant to the changes in the git diff, only running Python test files."
  (interactive)
  (let* ((project-root (tdd-mode-get-project-root))
         (changed-files (shell-command-to-string
                         (format "git diff --name-only --diff-filter=AM HEAD^ | grep 'tests/.*\\.py$'")))
         (test-files (split-string changed-files "\n" t)))
    (when test-files
      (let ((command (format "%s %s %s"
                             (tdd-mode-get-runner)
                             project-root
                             (mapconcat 'identity test-files " "))))
        (tdd-mode-log "Running relevant tests with command '%s'" command)
        (tdd-mode-run-test command)))))

(defun tdd-mode-run-file-tests ()
  "Run tests on the current file/module."
  (interactive)
  (let ((test-command (tdd-mode-get-test-command)))
    (tdd-mode-log "Running tests on the current file/module: `%s`" test-command)
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
    (tdd-mode-run-last-test)))

(defun tdd-mode-display-output ()
  "Display the test output buffer based on `tdd-mode-buffer-popup` setting, applying ansi color."
  (with-current-buffer tdd-mode-test-buffer
    (ansi-color-apply-on-region (point-min) (point-max))
    (if tdd-mode-buffer-popup
        (display-buffer (current-buffer))
      (bury-buffer))))

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

(defun tdd-mode-copy-output-to-clipboard ()
  "Copy the test output to the clipboard."
  (interactive)
  (if (get-buffer tdd-mode-test-buffer)
      (with-current-buffer tdd-mode-test-buffer
        (kill-ring-save (point-min) (point-max))
        (message "Test output copied to clipboard."))
    (message "No test output buffer found.")))

(defun tdd-mode-apply-color-to-buffer (&rest _)
  "Reapply the mode-line color based on last test result when switching buffers."
  (when tdd-mode-blink-enabled
    (let ((color (if (eq tdd-mode-last-test-exit-code 0)
                     tdd-mode-blink-pass-color
                   tdd-mode-blink-fail-color)))
      (tdd-mode-blink-mode-line color))))

(add-hook 'window-selection-change-functions #'tdd-mode-apply-color-to-buffer)

;;;###autoload
(define-minor-mode tdd-mode
  "Test-Driven Development mode for Python in Emacs."
  :lighter " TDD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c t") tdd-mode-command-map)
            map)
  (if tdd-mode
      (progn
        (setq tdd-mode-original-mode-line-bg (face-background 'mode-line))
        (add-hook 'after-save-hook 'tdd-mode-after-save-handler nil t)
        (add-hook 'window-selection-change-functions #'tdd-mode-apply-color-to-buffer nil t)
        (message "[tdd-mode] TDD Mode activated"))
    (remove-hook 'after-save-hook 'tdd-mode-after-save-handler t)
    (remove-hook 'window-selection-change-functions #'tdd-mode-apply-color-to-buffer t)
    (tdd-mode-set-mode-line-color tdd-mode-original-mode-line-bg)
    (message "[tdd-mode] TDD Mode deactivated")))

(provide 'tdd-mode)
;;; tdd-mode.el ends here
