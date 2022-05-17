(use-package real-auto-save
  :ensure t ;; Won't work if we defer.
  :config
  (progn 
    (setq real-auto-save-interval 10
          backup-directory-alist `(("." . ,(concat user-emacs-directory "backups/")))
          auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosaves/") t))
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t
          create-lockfiles nil)
    (global-auto-revert-mode 1))
  :hook ((text-mode . real-auto-save-mode)
         (prog-mode . real-auto-save-mode)
         (snippet-mode . (lambda () (real-auto-save-mode -1)))))
