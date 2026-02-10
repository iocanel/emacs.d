;;; mode.el --- Test Go mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-go-mode-available ()
  "Test that go-mode is available."
  (message "Testing Go mode availability...")
  (dolist (path '("/emacs/.config/emacs/.local/elpaca/builds/go-mode"
                  "~/.emacs.d/.local/elpaca/builds/go-mode"))
    (when (file-directory-p (expand-file-name path))
      (add-to-list 'load-path (expand-file-name path))))
  (condition-case err
      (progn
        (require 'go-mode)
        (if (fboundp 'go-mode)
            (progn
              (message "✅ Go mode test passed")
              (kill-emacs 0))
          (progn
            (message "❌ Go mode not available after require")
            (kill-emacs 1))))
    (error
     (message "❌ Go mode not available: %s" err)
     (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-go-mode-available))

(provide 'mode)
;;; mode.el ends here