;;; mode.el --- Test TypeScript mode availability

;; Usage: emacs --batch --load mode.el

;;; Code:

(defun test-typescript-mode-available ()
  "Test that typescript-mode is available."
  (message "Testing TypeScript mode availability...")
  (dolist (path '("/emacs/.config/emacs/.local/elpaca/builds/typescript-mode"
                  "~/.emacs.d/.local/elpaca/builds/typescript-mode"))
    (when (file-directory-p (expand-file-name path))
      (add-to-list 'load-path (expand-file-name path))))
  (condition-case err
      (progn
        (require 'typescript-mode)
        (if (fboundp 'typescript-mode)
            (progn
              (message "✅ TypeScript mode test passed")
              (kill-emacs 0))
          (progn
            (message "❌ TypeScript mode not available after require")
            (kill-emacs 1))))
    (error
     (message "❌ TypeScript mode not available: %s" err)
     (kill-emacs 1))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-typescript-mode-available))

(provide 'mode)
;;; mode.el ends here