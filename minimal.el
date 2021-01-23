;;
;; Display settings
;;
(setq inhibit-message t)
(setq inhibit-startup-message t)
(set-face-attribute 'default nil :height 150)

;;
;; Bell
;;

(setq visible-bell nil)
(setq ring-bell-function 'silent)

;;
;; Auto revert
;;                                        
(global-auto-revert-mode 1)

;;
;; Garbage collection
;;

(setq garbage-collection-messages t)

;;;###autoload
(defun iocanel/gc-restore-settings()
  "Restore gc settings."
  (message "Restoring garbage collection settings to their original values.")
  (setq gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-percentage 0.05))


;;;###autoload
(defun iocanel/gc-maximize-threshold ()
  "Maximize the gc threshold."
  (setq gc-cons-threshold most-positive-fixnum))

;;;###autoload
(defun iocanel/gc-restore-threshold ()
  "Restore the original gc threshold."
  (setq gc-cons-threshold orig-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'iocanel/gc-maximize-threshold)
(add-hook 'minibuffer-exit-hook #'iocanel/gc-restore-threshold)

;;
;;
;; Startup hook
;;
(add-hook 'emacs-startup-hook
          (lambda ()

            (let ((inhibit-message nil))
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done))))

;; Eval Path
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 25000)

;;
;; Doom UI
;;
(use-package all-the-icons :defer t)            

(use-package doom-themes
  :config
  (when (display-graphic-p)
    (load-theme 'doom-one t)(setq mode-line-format nil)))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-lsp t
        doom-modeline-column-zero-based t)
  :config
  (when
      (display-graphic-p)
    (doom-modeline-mode)
    (column-number-mode)))

(use-package hide-mode-line
  :config
  (when (not (display-graphic-p)) (global-hide-mode-line-mode)))

;;
;; Buffers
;;
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;;
;; Clipboard
;;
(setq x-select-enable-clipboard t)

;;
;; Dired
;;
(setq dired-dwim-target t)

(use-package dired-narrow
  :bind (:map dired-mode-map 
              ("C-c C-n" . dired-narrow)
              ("C-c C-f" . dired-narrow-fuzzy)
              ("C-c C-N" . dired-narrow-regexp)))

(use-package dired-subtree
    :bind (:map dired-mode-map 
                ("<tab>" . dired-subtree-toggle)
                ("<backtab>" . dired-subtree-cycle)))

;;;###autoload
(defun iocanel/dired-expand-all ()
  (interactive)
  "Expand all subtrees in the dired buffer."
  (let ((has-more t))
    (while has-more
      (condition-case ex
          (progn
            (dired-next-dirline 1)
            (dired-subtree-toggle))
        ('error (setq has-more nil))))))
;;
;; Editor
;;

;;
;; Exit Confirmation
;;
(setq confirm-kill-emacs 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; Indentation
;;
(setq-default indent-tabs-mode nil)
(setq electric-indent-inhibit t)

;;
;; Windows
;;
(use-package ace-window
  :defer t
  :bind  ("M-o" . 'ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;;###autoload
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;;;###autoload
(defun split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;;
;; Additional files
;;

(load-file "~/.config/emacs/+autosave.el")
(load-file "~/.config/emacs/+evil.el")
(load-file "~/.config/emacs/+completion.el")
(load-file "~/.config/emacs/+treemacs.el")
(load-file "~/.config/emacs/+projectile.el")
(load-file "~/.config/emacs/+helm.el")
(load-file "~/.config/emacs/+git.el")
(load-file "~/.config/emacs/+eshell.el")

 ;; Async
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+editor.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+ide.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+org.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+email.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+latex.el")))
(run-with-idle-timer 2 nil (lambda () (load-file "~/.config/emacs/+screens.el")))
(run-with-idle-timer 2 nil (lambda () (load-file "~/.config/emacs/+elfeed.el")))
(run-with-idle-timer 2 nil (lambda () (load-file "~/.config/emacs/+bongo.el")))

;; Org files
(run-with-idle-timer 1 nil (lambda () (org-babel-load-file "~/.config/emacs/+uml.org")))
(run-with-idle-timer 1 nil (lambda () (org-babel-load-file "~/.config/emacs/+jira.org")))

(run-with-idle-timer 5 nil (lambda () (org-babel-load-file "~/Documents/org/roam/habits.org")))
(run-with-idle-timer 5 nil (lambda () (org-babel-load-file "~/Documents/org/roam/nutrition.org")))
(run-with-idle-timer 5 nil (lambda () (org-babel-load-file "~/Documents/org/roam/video-notes.org")))

 
;; Finalize
(run-with-idle-timer 3 nil (lambda () (load-file "~/.config/emacs/finalize.el")))

;; Tune garbage collect
(run-with-idle-timer 3 nil (lambda () (iocanel/gc-restore-settings)))
(add-hook 'focus-out-hook 'garbage-collect)

;(use-package explain-pause-mode :config (explain-pause-mode))
