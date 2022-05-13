(use-package helm
  :defer t
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 10)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . find-file)
         ("C-x C-r" . helm-resume)
         ("M-y" . helm-show-kill-ring)))

(use-package helm-ag
  :defer t
  :straight (helm-ag :host github :repo "ioforks/helm-ag")
  :commands (helm-do-ag-project-root helm-ag)
  :bind (("C-c g" . helm-do-ag-project-root)
         :map helm-ag-mode-map
         ("C-c C-f" . helm-follow-mode)))

(use-package helm-projectile :defer t :commands (helm-projectile helm-projectile-ag))
