(use-package treemacs
  :defer 1
  :bind ("M-0" . 'treemacs-select-window)
  :hook (treemacs-mode . (lambda () (setq mode-line-format nil)))
  :config
  (progn
    (doom-themes-treemacs-config)
    (setq 
     treemacs-change-root-without-asking t
     treemacs-collapse-dirs              0
     treemacs-file-event-delay           5000
     treemacs-follow-after-init          t
     treemacs-follow-recenter-distance   0.1
     treemacs-goto-tag-strategy          'refetch-index
     treemacs-indentation                2
     treemacs-indentation-string         " "
     treemacs-max-git-entries            5000
     treemacs-is-never-other-window      nil
     treemacs-no-png-images              nil 
     treemacs-recenter-after-file-follow t
     treemacs-recenter-after-tag-follow  nil
     treemacs-show-hidden-files          nil 
     treemacs-silent-filewatch           t
     treemacs-silent-refresh             t
     treemacs-sorting                    'alphabetic-desc
     treemacs-tag-follow-cleanup         t
     treemacs-tag-follow-delay           1.5
     treemacs-persist-file              (f-join user-emacs-directory ".cache" "treemacs-persist") 
     treemacs-width                      30)))
                                        ;(treemacs-follow-mode t)
                                        ;(treemacs-filewatch-mode t)
                                        ;(treemacs-fringe-indicator-mode t)
;;an alternative is (treemacs-git-mode 'extended) which is currently slow for large projects.))
                                        ;(treemacs-git-mode 'extended)))

(use-package treemacs-evil :defer 1)

(use-package treemacs-projectile
  :defer 1
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(defun iocanel/git-p (name path)
  (equal name ".git"))

(setq treemacs-ignored-file-predicates '(iocanel/git-p))
