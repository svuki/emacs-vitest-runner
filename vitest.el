(require 'json)
(require 'subr-x)

(defvar vitest-buffer-name "*vitest*"
  "Name of the buffer to display vitest results.")

(defvar vitest-test-results nil
  "List to store test results.")

(defun vitest-project ()
  "Activate vitest mode and open the *vitest* buffer."
  (interactive)
  (let ((buffer (get-buffer-create vitest-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (vitest-mode))
    (display-buffer buffer)
    (vitest-run-tests)))

(define-derived-mode vitest-mode special-mode "Vitest"
  "Major mode for displaying vitest results."
  (define-key vitest-mode-map (kbd "TAB") 'vitest-toggle-section)
  (define-key vitest-mode-map (kbd "C-c C-t") 'vitest-toggle-section)
  (setq-local outline-regexp "^\\(-\\|--\\|---\\)") 
  (outline-minor-mode 1))

(defun vitest-toggle-section ()
  "Toggle visibility of the current section."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (outline-invisible-p (line-end-position))
        (outline-show-subtree)
      (outline-hide-subtree))))

(defun vitest-run-tests ()
  "Run vitest with JSON reporter and parse the output."
  (let ((process (start-process "vitest-process" vitest-buffer-name
                                "npx" "vitest" "--reporter" "json")))
    (set-process-filter process 'vitest-process-filter)
    (set-process-sentinel process 'vitest-process-sentinel)))

;; Processing JSON test messages
(defun ->standard-single-result (raw-result)
  (list
   (cons 'passed (string= (alist-get 'status raw-result) "passed"))
   (cons 'name (alist-get 'fullName raw-result))
   (cons 'failure-messages (seq-into (alist-get 'failureMessages raw-result) 'list))))

(defun ->standard-file-result (file-result)
  (let ((filename (alist-get 'name file-result))
	(standard-single-results (mapcar #'->standard-single-result (alist-get 'assertionResults file-result))))
    (mapcar
     (lambda (result) (cons (cons 'file filename) result))
     standard-single-results)))

;; alist with keys 'passed 'name 'failure-messages 'file
(defun ->standard-results (results)
  (let ((test-results (alist-get 'testResults results)))
    (apply #'append
	   (mapcar #'->standard-file-result test-results))))

;; The test filter declaration
(defun vitest-process-filter (process output)
  "Process filter to handle vitest output."
  (with-current-buffer (get-buffer-create "*vitest*")
    (let* ((json-result (json-read-from-string output))
	   (rows (->standard-results json-result))
	   (inhibit-read-only t))
      (erase-buffer)
      (dolist (row rows)
	(insert (propertize (format "- %s | %s\n"
				    (if (alist-get 'passed row) "PASSED" "FAILED")
				    (alist-get 'name row))
			    'font-lock-face (list :foreground (if (alist-get 'passed row) "green" "red"))
			    ))
	(mapcar
	 (lambda (msg) (progn (insert msg) (insert "\n")))
	 (alist-get 'failure-messages row))))
    (outline-cycle-buffer 1)
    (align (point-min) (point-max))))


(defun vitest-process-sentinel (process event)
  "Process sentinel to handle process events."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (insert (format "\nProcess %s %s" process event)))))))

(provide 'vitest)
