;;; mode.el --- Test JavaScript mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-javascript-mode-available ()
  "Test that js-mode is available in the container."
  (message "Testing JavaScript mode availability...")
  (if (fboundp 'js-mode)
      (progn
        (message "✅ JavaScript mode test passed")
        (kill-emacs 0))
    (progn
      (message "❌ JavaScript mode not available")
      (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-javascript-mode-available))

(provide 'mode)
;;; mode.el ends here