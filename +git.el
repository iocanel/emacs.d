(use-package magit
  :defer t
  :config
  (setq ediff-multiframe nil)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind (("C-c g s" . magit-status)))

(use-package evil-magit
  :after magit)

(use-package browse-at-remote
  :defer t
  :bind (("C-c g g" . browse-at-remote)))

(use-package git-timemachine
  :defer t
  :bind (("C-c g t" . git-timemachine-toggle)
         :map git-timemachine-mode-map
         ("M-n" . git-timemachine-show-next-revision)
         ("M-p" . git-timemachine-show-previous-revision)
         ("M-b" . git-timemachine-blame)
         ("M-c" . git-timemachine-show-current-revision)))

