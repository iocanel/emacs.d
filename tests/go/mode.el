;;; mode.el --- Test Go mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-go-mode-available ()
  "Test that go-mode is available in the container."
  (message "Testing Go mode availability...")
  (if (fboundp 'go-mode)
      (progn
        (message "✅ Go mode test passed")
        (kill-emacs 0))
    (progn
      (message "❌ Go mode not available")
      (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-go-mode-available))

(provide 'mode)
;;; mode.el ends here