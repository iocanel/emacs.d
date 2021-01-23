(use-package helm
  :defer t
  :config
  (setq helm-autoresize-max-height 0
        helm-autoresize-min-height 10)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . find-file)
         ("M-y" . helm-show-kill-ring)))


(use-package helm-projectile)
