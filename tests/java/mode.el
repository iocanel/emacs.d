;;; mode.el --- Test Java mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-java-mode-available ()
  "Test that java-mode is available in the container."
  (message "Testing Java mode availability...")
  (if (fboundp 'java-mode)
      (progn
        (message "✅ Java mode test passed")
        (kill-emacs 0))
    (progn
      (message "❌ Java mode not available")
      (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-java-mode-available))

(provide 'mode)
;;; mode.el ends here