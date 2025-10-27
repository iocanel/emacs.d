;;; mode.el --- Test Python mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-python-mode-available ()
  "Test that python-mode is available in the container."
  (message "Testing Python mode availability...")
  (if (fboundp 'python-mode)
      (progn
        (message "✅ Python mode test passed")
        (kill-emacs 0))
    (progn
      (message "❌ Python mode not available")
      (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-python-mode-available))

(provide 'mode)
;;; mode.el ends here