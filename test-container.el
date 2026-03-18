;;; test-container.el --- Simple container smoke test

;; Usage: emacs --batch --load test-container.el

;;; Code:

(defun test-container-startup ()
  "Test that container starts and Emacs loads without errors."
  (message "Testing container startup...")
  (let ((tests-passed t))
    
    ;; Test 1: Emacs basics
    (unless (boundp 'user-emacs-directory)
      (message "❌ user-emacs-directory not defined")
      (setq tests-passed nil))
    
    ;; Test 2: Git availability
    (unless (executable-find "git")
      (message "❌ git not available")
      (setq tests-passed nil))
    
    ;; Test 3: Try to require org (should be available)
    (unless (require 'org nil t)
      (message "❌ org-mode not available")
      (setq tests-passed nil))
    
    (if tests-passed
        (progn
          (message "✅ Container test passed")
          (kill-emacs 0))
      (progn
        (message "❌ Container test failed")
        (kill-emacs 1)))))

;; Run test if loaded in batch mode
(when noninteractive
  (test-container-startup))

(provide 'test-container)
;;; test-container.el ends here