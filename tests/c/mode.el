;;; mode.el --- Test C mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-c-mode-available ()
  "Test that c-mode is available in the container."
  (message "Testing C mode availability...")
  (if (fboundp 'c-mode)
      (progn
        (message "✅ C mode test passed")
        (kill-emacs 0))
    (progn
      (message "❌ C mode not available")
      (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-c-mode-available))

(provide 'mode)
;;; mode.el ends here