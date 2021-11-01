(defun laptop-mode()
  "Modify theme for latpop use"
  (interactive)
  (set-face-attribute 'default nil :height 75)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 90)))

(defun desktop-mode()
  "Modify theme for latpop use"
  (interactive)
  (set-face-attribute 'default nil :height 130)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 130)))

(defun comf-mode()
  "Modify theme for comfortable use"
  (interactive)
  (set-face-attribute 'default nil :height 150)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 160)))

(defun presentation-mode()
  "Modify theme for presentations use"
  (interactive)
  (set-face-attribute 'default nil :height 250)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 260)))

(defun asciinema-mode()
  "Modify theme for asciinema use"
  (interactive)
  (setq mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq idee-tree-enabled nil)
  (setq inhibit-message t)
  (setq-default inhibit-message t)
  (set-face-attribute 'default nil :height 150)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 160)))

(setq ic/selected-screen-mode 'comf-mode)
(funcall ic/selected-screen-mode)
