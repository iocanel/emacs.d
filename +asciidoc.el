;;; +asciidoc.el --- Asciidoc -*- lexical-binding: t -*-

(use-package adoc-mode)
(use-package ox-asciidoc
  :defer t
  :commands (org-asciidoc-export-to-asciidoc org-asciidoc-export-as-asciidoc))

(use-package livemarkup
  :straight (livemarkup :host github :repo "dawsers/emacs-livemarkup")
  :defer t
  :commands (livemarkup-track-asciidoc livemarkup-track-org livemark-untrack)
  :config
  (setq livemarkup-output-directory "/tmp"
        livemarkup-close-buffer-delete-temp-files t))

(provide '+asciidoc)
;;; +asciidoc.el ends here