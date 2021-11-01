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
(setq target-gc-cons-threshold (* 128 1024 1024))

;;;###autoload
(defun ic/gc-restore-settings()
  (interactive)
  "Restore gc settings."
  (message "Restoring garbage collection settings to their original values.")
  (setq gc-cons-threshold target-gc-cons-threshold)
  (setq gc-cons-percentage 0.4))


;;;###autoload
(defun ic/gc-maximize-threshold ()
  (interactive)
  "Maximize the gc threshold."
  (setq gc-cons-threshold most-positive-fixnum))

;;;###autoload
(defun ic/gc-restore-threshold ()
  "Restore the original gc threshold."
  (setq gc-cons-threshold target-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'ic/gc-maximize-threshold)
(add-hook 'minibuffer-exit-hook #'ic/gc-restore-threshold)

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
  :defer t
  :commands (global-hide-mode-line-mode)
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
  :after dired
  :commands (dired-narrow dired-narrow-fuzzy dired-narrow-regexp)
  :bind (:map dired-mode-map 
              ("C-c C-n" . dired-narrow)
              ("C-c C-f" . dired-narrow-fuzzy)
              ("C-c C-N" . dired-narrow-regexp)))

(use-package dired-subtree
    :defer t
    :after dired
    :commands (dired-subtree-toggle dired-subtree-cycle)
    :bind (:map dired-mode-map 
                ("<tab>" . dired-subtree-toggle)
                ("<backtab>" . dired-subtree-cycle)))

;;;###autoload
(defun ic/dired-expand-all ()
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
(defun ic/split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;;;###autoload
(defun ic/split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'ic/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'ic/split-and-follow-vertically)


;;
;; Additional files
;;


(load-file (concat ic/emacs-cfg-dir "+autosave.el"))
(load-file (concat ic/emacs-cfg-dir "+evil.el"))
(load-file (concat ic/emacs-cfg-dir "+completion.el"))
(load-file (concat ic/emacs-cfg-dir "+treemacs.el"))
(load-file (concat ic/emacs-cfg-dir "+projectile.el"))
(load-file (concat ic/emacs-cfg-dir "+helm.el"))
(load-file (concat ic/emacs-cfg-dir "+git.el"))
(load-file (concat ic/emacs-cfg-dir "+shell.el"))

(load-file (concat ic/emacs-cfg-dir "+editor.el"))
(load-file (concat ic/emacs-cfg-dir "+prog.el"))
(load-file (concat ic/emacs-cfg-dir "+ide.el"))
(load-file (concat ic/emacs-cfg-dir "+screens.el"))
(load-file (concat ic/emacs-cfg-dir "+elfeed.el"))
(load-file (concat ic/emacs-cfg-dir "+bongo.el"))
(load-file (concat ic/emacs-cfg-dir "+org.el"))
(load-file (concat ic/emacs-cfg-dir "+email.el"))
(ic/gc-restore-settings)

;; Async
(run-with-idle-timer 0 nil (lambda ()
;;                                (load-file (concat ic/emacs-cfg-dir "+latex.el"))
;;                                (load-file (concat ic/emacs-cfg-dir "+jira.el"))
;;                                (load-file (concat ic/emacs-cfg-dir "+jira.el"))
;;                                (load-file (concat ic/emacs-cfg-dir "+eww.el"))
;;                                (load-file (concat ic/emacs-cfg-dir "+uml.el"))
;;                                (load-file "~/Documents/org/habits.el")
;;                                (load-file "~/Documents/org/nutrition.el")
;;                                (load-file "~/Documents/org/video-notes.el")
                                (load-file (concat ic/emacs-cfg-dir "finalize.el"))

                                ;; Tune garbage collect
                                (add-hook 'focus-out-hook 'garbage-collect)))

(use-package esup)
