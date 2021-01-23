(use-package projectile
  :defer t
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-use-git-grep t
        projectile-globally-ignored-file-suffixes '(".elc" ".class" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-sort-order 'recentf
        projectile-switch-project-action 'projectile-dired)
  (projectile-global-mode)
  :bind (("C-x b" . projectile-switch-to-buffer)))

;;
;; These will probably need to go
;; ("C-c p o" . projectile-switch-project)
;; ("C-c p a" . projectile-add-known-project)
;; ("C-c p r" . projectile-remove-known-project)
;; ("C-c p f" . projectile-find-file)
;; ("C-c p g" . projectile-grep)))
