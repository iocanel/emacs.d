(use-package evil
  :defer t
  :config
  (setq evil-want-fine-undo nil) ;Fix issue with undo granularity (See: https://github.com/syl20bnr/spacemacs/issues/2675)
  :init
  (evil-mode 1))

(use-package evil-leader
  :defer t
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key "<SPC>" 'projectile-find-file)
    (evil-leader/set-key "c" 'org-capture)
    (evil-leader/set-key "a" 'org-agenda)))

;; C-f is needed by other parts of emacs, so we need to free this up
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "<right>") nil)
(define-key evil-motion-state-map (kbd "<left>") nil)
(define-key evil-normal-state-map (kbd "<") nil)
(define-key evil-normal-state-map (kbd ">") nil)
