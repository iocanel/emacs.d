;;
;; Editor
;;
(global-set-key [escape] 'keyboard-escape-quit)

(use-package avy
  :defer t
  :bind  ("M-c" . 'avy-goto-char)
  :bind  ("M-l" . 'avy-goto-line)
  :bind  ("M-w" . 'avy-goto-word-1))

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

;;
;; Swiper
;;

;;;###autoload
(defun iocanel/swiper-isearch-non-fuzzy()
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

(use-package swiper
  :defer t
  :config
  :bind (("C-s" . 'swiper-isearch)
         ("C-f" . 'iocanel/swiper-isearch-fuzzy)))

(use-package mark-multiple
  :defer t
  :bind ("C-c m" . 'mark-next-like-this))

(use-package expand-region
  :defer t
  :bind ("C-q" . 'er/expand-region))
