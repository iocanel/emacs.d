;;; bootstrap.el --- deterministic bootstrap for elpaca + literate config

;; Make startup deterministic & quiet
(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      inhibit-startup-message t
      native-comp-deferred-compilation nil)

;; Load lock map with pinned SHAs (provided below)
(load (expand-file-name "lock.el" (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME"))))

(defun lock-ref (sym) (alist-get sym my-elisp-locks nil nil #'eq))

;; Bootstrap elpaca at an exact commit without network surprises later
(let* ((user-dir (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME")))
       (elpaca-dir (expand-file-name "elpaca" (expand-file-name "elpa" user-dir)))
       (repo-url "https://github.com/progfolio/elpaca.git")
       (commit (lock-ref 'elpaca)))
  (unless (file-directory-p elpaca-dir)
    (make-directory elpaca-dir t)
    (let ((default-directory elpaca-dir))
      (call-process "git" nil nil nil "clone" "--filter=tree:0" repo-url ".")
      (call-process "git" nil nil nil "checkout" "--detach" commit)))
  (add-to-list 'load-path (expand-file-name "elpaca" (expand-file-name "elpa" user-dir)))
  (require 'elpaca))

;; Ensure elpaca synchronizes in batch builds (builder stage only).
;; At runtime we set ELPACA_NO_AUTOINSTALL=t in the container.
(setq elpaca-log-level 'error)

(provide 'bootstrap)
