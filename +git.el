(use-package magit
  :defer t
  :config
  (setq ediff-multiframe nil)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind (("C-x g s" . magit-status)
         :map magit-file-mode-map
              ("C-x g" . nil) ;; Let`s unset current binding for magit-status
              ("C-x g s" . magit-status)))

(use-package evil-magit
  :after magit)

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


(use-package forge)

(use-package github-review
 :bind (("C-x g r" . github-review-forge-pr-at-point)
         :map github-review-mode-map
         ("C-x a" . github-review-approve)
         ("C-x c" . github-review-comment)
         ("C-x k" . github-review-reject)))

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
