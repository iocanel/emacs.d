(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode plain-tex-mode)
  :hook ((larex-mode-hook . LaTeX-preview-setup))
  :init
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil))


;; Preview Pane
(use-package latex-preview-pane
  :defer t
  :hook ((latex-mode-hook . latex-preview-pane-mode)))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
