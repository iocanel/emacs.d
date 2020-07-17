(use-package real-auto-save
  :defer t
  :config
  (progn 
     (setq real-auto-save-interval 600
        backup-directory-alist `(("." . "~/.emacs.d/backups/"))
        auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        create-lockfiles nil)
     (global-auto-revert-mode 1))
  :hook ((text-mode . real-auto-save-mode)
         (prog-mode . real-auto-save-mode)
         (snippet-mode . (lambda () (real-auto-save-mode -1)))))
