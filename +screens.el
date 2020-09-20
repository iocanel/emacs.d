(defun laptop-mode()
  "Modify theme for latpop use"
  (interactive)
  (set-face-attribute 'default nil :height 75)
  (set-face-attribute 'treemacs-root-face nil :height 90))

(defun desktop-mode()
  "Modify theme for latpop use"
  (interactive)
  (set-face-attribute 'default nil :height 100)
  (set-face-attribute 'treemacs-root-face nil :height 115))

(defun comf-mode()
  "Modify theme for comfortable use"
  (interactive)
  (set-face-attribute 'default nil :height 130)
  (set-face-attribute 'treemacs-root-face nil :height 130))

(defun presenetation-mode()
  "Modify theme for presentations use"
  (interactive)
  (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'treemacs-root-face nil :height 160))

(defun asciinema-mode()
  "Modify theme for asciinema use"
  (interactive)
  (set-face-attribute 'default nil :height 150)
  (set-face-attribute 'treemacs-root-face nil :height 160)
  (setq mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq idee-tree-enabled nil)
  (setq inhibit-message t)
  (setq-default inhibit-message t))

(presenetation-mode)
