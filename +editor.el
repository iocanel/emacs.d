;;; +editor.el --- Editor Configuration -*- lexical-binding: t; -*-

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
  :bind  (("M-c" . 'avy-goto-char)
          ("M-l" . 'avy-goto-line)
          ("M-w" . 'avy-goto-word-1)))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo/"))))
  :hook ((text-mode . undo-tree-mode)
         (prog-mode . undo-tree-mode))
  :bind (:map evil-normal-state-map
              ("u" . undo-tree-undo)
              ("C-R" . undo-tree-redo)))

;;
;; Ivy
;;
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy--flx-featurep t ;; for some reason integration seems to be broken, so we need this
        ivy-re-builders-alist '((swiper . regexp-quote)
                                (swiper-isearch . regexp-quote)
                                (t . ivy--regex-fuzzy))
        completing-read-function 'ivy-completing-read)
  :config
  (use-package flx :ensure t)
    :bind  ("C-x C-b" . 'ivy-switch-buffer)
    :hook ((text-mode . ivy-mode)
           (prog-mode . ivy-mode)
           (dired-mode . ivy-mode)))

(use-package counsel
  :defer t
  :commands (counsel-M-x counsel-ibuffer counsel-recentf counsel-ag counsel-rg counsel-google)
  :bind ("M-x" . 'counsel-M-x))

;;
;; Configure pop-up windows
;;
(use-package popper
  :defer t 
  :commands (ic/shell-pop-up-frame-enable ic/shell-pop-up-frame-disable ic/kill-if-popup)
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
    (defun ic/shell-pop-up-frame-enable()
      "Make shell windows pop-up frame."
      (interactive)
      (setq display-buffer-alist (add-to-list 'display-buffer-alist `("\\*\\(eshell.*\\|shell.*\\|vterm.*\\)\\*"
                                                                      (display-buffer-reuse-window display-buffer-pop-up-frame)
                                                                      (reusable-frames . visible)
                                                                      (window-height . 0.40)
                                                                      (side . bottom)
                                                                      (slot . 0)))))

    (defun ic/shell-pop-up-frame-dissable()
      "Make shell windows pop-up use window."
      (interactive)
      (setq display-buffer-alist (add-to-list 'display-buffer-alist `("\\*\\(eshell.*\\|shell.*\\|vterm.*\\)\\*"
                                                                      (display-buffer-in-side-window)
                                                                      (window-height . 0.40)
                                                                      (side . bottom)
                                                                      (slot . 0)))))
    ;;
    ;; The command below is used to kill popup buffers.
    ;; The idea is that the function will bind to `q` and 
    ;; kill the buffer is buffer is a popup or otherwise record marco.
    ;;
    (defun ic/kill-if-popup (register)
      "If the buffer is a pop-up buffer kill it, or record a macro using REGISTER otherwise."
      (interactive
       (list (unless (or (popper-popup-p (current-buffer)) (and evil-this-macro defining-kbd-macro))
               (or evil-this-register (evil-read-key)))))
      "Kill the currently selected window if its a popup."
      (if (popper-popup-p (current-buffer))
          (popper-kill-latest-popup)
        (evil-record-macro register)))

    (evil-leader/set-key "p t" 'popper-toggle-latest)
    (evil-leader/set-key "p c" 'popper-cycle)
    (evil-leader/set-key "p k" 'popper-kill-latest-popup)
    (define-key evil-normal-state-map (kbd "q") #'ic/kill-if-popup)
  :hook ((eshell-mode . popper-mode)
         (vterm-mode . popper-mode)
         (undo-tree-mode . popper-mode)
         (helm-ag-mode . popper-mode)
         (flycheck-error-list-mode . popper-mode)
         (flymake-mode . popper-mode)))


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
  "Stip dublicates from the LIST.
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
(defun ic/swiper-isearch()
  "Non-fuzzy version of swipper-isearch."
  (interactive)
  (let ((ivy-re-builders-alist '((swiper-isearch . regexp-quote))))
    (swiper-isearch)))

;;;###autoload
(defun ic/swiper-isearch-fuzzy()
  "Fuzzy version of swipper-isearch."
  (interactive)
  (let ((ivy-re-builders-alist '((swiper-isearch . ivy--regex-fuzzy))))
    (swiper-isearch)))

;;;###autoload
(defun ic/swiper-isearch-with-selection (&optional start end)
  "Swiper variation using selected from START to END text as initial input."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((start (or start (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil))))
    (if (and (use-region-p) start end)
        (swiper (buffer-substring start end))
      (ic/swiper-isearch))
    (keyboard-escape-quit)))

;;;###autoload
(defun ic/swiper-isearch-with-selection-fuzzy (&optional start end)
  "Swiper variation using selected from START to END text as initial input for fuzzy search."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((start (or start (if (use-region-p) (region-beginning) nil)))
        (end (or end (if (use-region-p) (region-end) nil))))
    (if (and (use-region-p) start end)
        (swiper (buffer-substring start end))
      (ic/swiper-isearch-fuzzy))
    (keyboard-escape-quit)))

(use-package swiper
  :defer t
  :config
  :bind (("C-s" . 'ic/swiper-isearch-with-selection)
         ("C-f" . 'ic/swiper-isearch-with-selection-fuzzy)))

(use-package mark-multiple
  :defer t
  :bind ("C-c m" . 'mark-next-like-this))

(use-package expand-region
  :defer t
  :bind ("C-q" . 'er/expand-region))

(use-package command-log-mode
  :defer t 
  :bind ("C-o" . 'clm/toogle-command-log-buffer))

(evil-leader/set-key "w b" 'balance-windows)

;;
;; Hydra posframe
;;

;; (use-package hydra-posframe
;;   :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
;;   :config
;;   (hydra-posframe-mode 1))

;;
;; Utils
;;
(defun ic/set-buffer-to-unix ()
  "Utility to set the buffer encoding to unix.
This is often handy to remove ^M from the end of line."
  (interactive)
  (set-buffer-file-coding-system 'unix 't))

