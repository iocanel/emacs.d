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

(use-package ace-mc)
;  :bind ("C-c m f" . ace-mc))
  
(use-package avy
  :defer t
  :bind  ("M-c" . 'avy-goto-char)
  :bind  ("M-l" . 'avy-goto-line)
  :bind  ("M-w" . 'avy-goto-word-1))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;;
;; Ivy
;;
(use-package flx :defer t)
(use-package smex :defer t)

(use-package ivy
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
  :defer t
  :bind ("M-x" . 'counsel-M-x))

(use-package ivy-posframe
  :after ivy
  :init 
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
  (ivy-posframe-mode 1))

;; Causing issues with grep
;; (use-package helm-posframe
;;   :custom (helm-posframe-poshandler #'posframe-poshandler-window-center)
;;   :init 
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   (helm-posframe-enable))

(defun error-filter (list)
  "Stip dublicates from the list.
   Credits: https://stackoverflow.com/questions/3815467/stripping-duplicate-elements-in-a-list-of-strings-in-elisp."
  (let ((new-list nil))
    (while list
      (let  ((current (car list)))
        (when (and current
                   (not (member current new-list))
                   (string-match-p (regexp-quote "ERROR") current))
          (setq new-list (cons current new-list))))
        (setq list (cdr list)))
    (nreverse new-list)))

(advice-add 'counsel-compilation-errors-cands :filter-return #'error-filter)

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
  (let ((start (or start (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil))))
    (if (and (use-region-p) start end)
        (swiper (buffer-substring start end))
      (iocanel/swiper-isearch))
    (keyboard-escape-quit)))

;;;###autoload
(defun iocanel/swiper-isearch-with-selection-fuzzy (&optional start end)
  "Swiper variation that uses selected text as initial input."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((start (or start (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil))))
    (if (and (use-region-p) start end)
        (swiper (buffer-substring start end))
      (iocanel/swiper-isearch-fuzzy))
    (keyboard-escape-quit)))

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

;;
;; Hydra posframe
;;

(use-package hydra-posframe
  :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
  :config
  (hydra-posframe-mode 1))
