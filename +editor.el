;;
;; Editor
;;

(global-set-key [escape] 'keyboard-escape-quit)

;; (use-package multiple-cursors
;;   :defer t
;;   :init (global-set-key (kbd "C-c m") nil)
;;   :bind (("C-c m e"  . mc/edit-lines)
;;          ("C-c m n" . mc/mark-next-like-this)
;;          ("C-c m a" . mc/mark-all-like-this)
;;          ("C-c m d" . mc/mark-all-like-this-dwim)
;;          ("C-c m p" . mc/mark-previous-like-this)
;;          ("C-c m s" . mc/skip-next-like-this)
;;          ("C-c m u n" . mc/unmark-next-like-this)
;;          ("C-c m u p" . mc/unmark-previous-like-this)))

(use-package ace-mc
  :bind ("C-c m f" . ace-mc))
  
(use-package avy
  :defer t
  :bind  ("M-c" . 'avy-goto-char)
  :bind  ("M-l" . 'avy-goto-line)
  :bind  ("M-w" . 'avy-goto-word-1))

(use-package ace-link
  :defer t
  :bind  (:map eww-mode-map
               ("C-c f" . 'ace-link-eww)))
  
;;
;; Ivy
;;
(use-package flx :defer t)
(use-package smex :defer t)

(use-package ivy
  :after flx
  :init
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist '((swiper . regexp-quote)
                                (swiper-isearch . regexp-quote)
                                (t . ivy--regex-fuzzy))
        completing-read-function 'ivy-completing-read)
  :config
  (ivy-mode 1)
  :bind  ("C-x C-b" . 'ivy-switch-buffer))

(use-package counsel
  :bind ("M-x" . 'counsel-M-x))

(use-package ivy-posframe
  :after ivy
  :init 
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (ivy-posframe-mode 1))

;;
;; Swiper
;;

;;;###autoload
(defun iocanel/swiper-isearch()
  (interactive)
  "Non-fuzzy version of swipper-isearch"
  (let ((ivy-re-builders-alist '((swiper-isearch . regexp-quote))))
    (swiper-isearch)))

;;;###autoload
(defun iocanel/swiper-isearch-fuzzy()
  (interactive)
  "Fuzzy version of swipper-isearch"
  (let ((ivy-re-builders-alist '((swiper-isearch . ivy--regex-fuzzy))))
    (swiper-isearch)))

;;;###autoload
(defun iocanel/swiper-isearch-with-selection (&optional start end)
  "Swiper variation that uses selected text as initial input."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (swiper (buffer-substring start end))
    (iocanel/swiper-isearch))
  (keyboard-escape-quit))

;;;###autoload
(defun iocanel/swiper-isearch-with-selection-fuzzy (&optional start end)
  "Swiper variation that uses selected text as initial input."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (swiper (buffer-substring start end))
    (iocanel/swiper-isearch-fuzzy))
  (keyboard-escape-quit))


(use-package swiper
  :defer t
  :config
  :bind (("C-s" . 'iocanel/swiper-isearch-with-selection)
         ("C-f" . 'iocanel/swiper-isearch-with-selection-fuzzy)))

(use-package mark-multiple
  :defer t
  :bind ("C-c m" . 'mark-next-like-this))

(use-package expand-region
  :defer t
  :bind ("C-q" . 'er/expand-region))

(use-package command-log-mode
  :defer t 
  :bind ("C-o" . 'clm/toogle-command-log-buffer))

;;; The following is based on Protesilaos Stavrou configuration: https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/emacs-init.org
(defvar iocanel/window-configuration nil "Current window configuration.")
(defvar iocanel/treemacs-visible nil "Is treemacs visible.")

(define-minor-mode window-single-mode
  "Toggle between multiple windows and single window. This is the equivalent of maximising a window."
  :lighter " [M]"
  :global nil
  (if (not (and (boundp 'window-single-mode) window-single-mode)) ;; If we have window-single-mode
      (when iocanel/window-configuration                          ;; And and exisitng configuration
        (progn                                                    ;; Restore ...
          (set-window-configuration iocanel/window-configuration)
          (when (and
                 (equal 'visible iocanel/treemacs-visible)
                 (not (equal 'visible (treemacs-current-visibility)))) (treemacs))))
    ;; Focus
    (progn
      (setq iocanel/window-configuration (current-window-configuration))
      (setq iocanel/treemacs-visible (treemacs-current-visibility))
      (delete-other-windows)))
  (setq iocanel/treemacs-visible (treemacs-current-visibility)))
      

(global-set-key (kbd "M-m") 'window-single-mode)
