(use-package plantuml-mode
  :commands plantuml-download-jar
  :init
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)
               (setq plantuml-jar-path (concat user-emacs-directory "plantuml/" "plantuml.jar")
                     org-plantuml-jar-path plantuml-jar-path))
  :hook (plantuml-mode . yas/minor-mode))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config (flycheck-plantuml-setup))
