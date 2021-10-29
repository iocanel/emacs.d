;;; -*- lexical-binding: t; -*-

;;
;; Editor
;;

(global-set-key [escape] 'keyboard-escape-quit)

;;
;; Prefer splitting windows side by side (see split-window-sensibly for more details
;;
(setq split-height-threshold nil)
(setq split-width-threshold 0)

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
(use-package flx :ensure t)
(use-package smex :ensure t)

(require 'flx)
(use-package ivy
  :ensure t
  :after flx
  :init
  (setq ivy-use-virtual-buffers t
        ivy--flx-featurep t ;; for some reason integration seems to be broken, so we need this
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

;;
;; Configure pop-up windows
;;
(use-package popper
  :straight t 
  :init
  (setq popper-reference-buffers
        '(
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Flycheck errors\\*"
          "\\*Flymake diagnostics for .*\\*"
          "\\*Async Shell Command\\*"
          "\\*.*compilation.*\\*"
          "\\*Org QL View: Github issues for .*\\*"
          "\\*eshell.*\\*"
          "\\*shell.*\\*"
          "\\*vterm.*\\*"
          "\\*scratch.*\\*"
          "\\*undo-tree*\\*"
          "\\*helm-ag\\*"
          "\\*helm-ag-edit\\*"
          "\\*side.*\\*")
        popper-mode-line (propertize " π " 'face 'mode-line-emphasis))
  :config
    (evil-leader/set-key "p t" 'popper-toggle-latest)
    (evil-leader/set-key "p c" 'popper-cycle)
    (evil-leader/set-key "p k" 'popper-kill-latest-popup)
  (popper-mode +1))


;;
;; Below it the display-buffer-alist rules.
;; Ideally all window placement will go through here.
;; There are however exception where windows are manually created and don't go via `display-buffer`.
;; Those cases require manual intervention (either overriding or advising).
;;
;; Source of inspiration: https://www.youtube.com/watch?v=rjOhJMbA-q0
;;
 (setq display-buffer-alist
       `(
          ;; This is needed so that popper respects my custom configuration
          `(popper-display-control-p (,popper-display-function))

          ;; Side helper buffers
          ("\\*undo-tree\\*"
           (display-buffer-in-side-window)
           (window-width . 0.10)
           (side . right)
           (slot . 0))

          ("\\*side .*\\*"
           (display-buffer-in-side-window)
           (window-width . 0.50)
           (side . right)
           (slot . 1))

          ("\\*eww\\*"
           (display-buffer-in-side-window)
           (window-width . 0.30)
           (side . right)
           (slot . 2))

          ;; Bottom Buffers
          ("\\*\\(Async [s\\|S]hell [c\\|C]ommand.*\\|eshell.*\\|shell.*\\|vterm.*\\|helm-ag\\|helm-ag-edit\\|xref\\|.*compilation\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . 0))

          ("\\*\\(Flycheck errors\\|Flymake diagnostics for .*\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . 1))

          ("\\*Messages\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . 1))

          ("\\*\\(Messages\\|Warnings\\|Backtrace\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . 2))

          ("\\*Org QL View: Github issues for .*\\*"
           (display-buffer-in-side-window)
           (window-height . 0.20)
           (side . bottom)
           (slot . 2))

          ))

;;
;; The command below is used to kill popup buffers.
;; The idea is that the function will bind to `q` and 
;; kill the buffer is buffer is a popup or otherwise record marco.
;;
(defun ic/kill-if-popup (register)
  (interactive
   (list (unless (or (popper-popup-p (current-buffer)) (and evil-this-macro defining-kbd-macro))
           (or evil-this-register (evil-read-key)))))
  "Kill the currently selected window if its a popup."
  (if (popper-popup-p (current-buffer))
      (popper-kill-latest-popup)
    (evil-record-macro register)))

(define-key evil-normal-state-map (kbd "q") #'ic/kill-if-popup)


;; (use-package ivy-posframe
;;   :after ivy
;;   :init 
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;;   (ivy-posframe-mode 1))

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

;; (use-package hydra-posframe
;;   :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
;;   :config
;;   (hydra-posframe-mode 1))
