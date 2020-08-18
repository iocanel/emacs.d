;;
;; Startup hook
;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Eval Path
(setq max-lisp-eval-depth 10000)

;;
;; Doom UI
;;
(use-package all-the-icons)            

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
  (when (display-graphic-p) (doom-modeline-mode)))

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
(load-file "~/.config/emacs/+ivy.el")
(load-file "~/.config/emacs/+completion.el")
(load-file "~/.config/emacs/+treemacs.el")
(load-file "~/.config/emacs/+projectile.el")
(load-file "~/.config/emacs/+git.el")
(load-file "~/.config/emacs/+eshell.el")

;; Async
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+editor.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+ide.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+org.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+email.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+latex.el")))
(run-with-idle-timer 1 nil (lambda () (load-file "~/.config/emacs/+screens.el")))

(setq gc-cons-threshold 2147483648)
