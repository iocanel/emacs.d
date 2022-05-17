(add-to-list 'straight-x-pinned-packages '("magit" . "21454777281247d97814ce5fb64f4afe39fab5da"))
(add-to-list 'straight-x-pinned-packages '("forge" . "e340c2be2aa5337c8c4c81cd6eab87961c6848b6"))
(add-to-list 'straight-x-pinned-packages '("github-review" . "db723740e02348c0760407e532ad667ef89210ec"))

(use-package magit 
  :defer t
  :config
  (setq ediff-multiframe nil)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind (("C-x g s" . magit-status)))
;; :map magit-file-mode-map
;;      ("C-x g" . nil) ;; Let`s unset current binding for magit-status
;;      ("C-x g s" . magit-status)))

(use-package browse-at-remote
  :defer t
  :bind (("C-x g g" . browse-at-remote)))

(use-package git-timemachine
  :defer t
  :bind (("C-x g t" . git-timemachine-toggle)
         :map git-timemachine-mode-map
         ("M-n" . git-timemachine-show-next-revision)
         ("M-p" . git-timemachine-show-previous-revision)
         ("M-b" . git-timemachine-blame)
         ("M-c" . git-timemachine-show-current-revision)))


(use-package forge
  :defer t
  :commands (forge-pull)
  :bind (("C-x g i c" . forge-create-issue)
         ("C-x g p c" . forge-create-pullreq)))

(use-package github-review
  :defer t
  :config

  (defadvice github-review-save-diff (after github-review-save-diff-after activate)
    "Go to the first line when opening new pr for review."
    (goto-char (point-min)))

  (defadvice github-review-approve (after github-review-approve-after activate)
    "Accept, show message in minibuffer and kill the review buffer."
    (message "Pull request approved!")
    (set-buffer-modified-p nil)
    (kill-this-buffer))

  (defadvice github-review-reject (after github-review-reject-after activate)
    "Reject, show message in minibuffer and kill the review buffer."
    (message "Pull request rejected!")
    (set-buffer-modified-p nil)
    (kill-this-buffer))

  (defadvice github-review-comment (after github-review-comment-after activate)
    "Comment, show message in minibuffer and kill the review buffer."
    (message "Pull request commented!")
    (set-buffer-modified-p nil)
    (kill-this-buffer))
  
  :bind (("C-x g r" . github-review-forge-pr-at-point)
         :map github-review-mode-map
         ("C-x a" . github-review-approve)
         ("C-x c" . github-review-comment)
         ("C-x k" . github-review-reject)))
