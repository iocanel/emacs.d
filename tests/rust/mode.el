;;; mode.el --- Test Rust mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-rust-mode-available ()
  "Test that rust-mode is available in the container."
  (message "Testing Rust mode availability...")
  (add-to-list 'load-path "/emacs/.config/emacs/.local/elpaca/builds/rust-mode")
  (condition-case err
      (progn
        (require 'rust-mode)
        (if (fboundp 'rust-mode)
            (progn
              (message "✅ Rust mode test passed")
              (kill-emacs 0))
          (progn
            (message "❌ Rust mode not available after require")
            (kill-emacs 1))))
    (error
     (message "❌ Rust mode not available: %s" err)
     (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-rust-mode-available))

(provide 'mode)
;;; mode.el ends here