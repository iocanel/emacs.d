(use-package auctex
  :defer t
  :commands (latex-mode plain-tex-mode)
  :mode "\\.tex\\'"
  :custom (org-latex-classes '(("beamer" "\\documentclass[presentation]{beamer}"
                                ("\\section{%s}" . "\\section*{%s}")
                                ("\\subsection{%s}" . "\\subsection*{%s}")
                                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                               ("article" "\\documentclass[11pt]{article}"
                                ("\\section{%s}" . "\\section*{%s}")
                                ("\\subsection{%s}" . "\\subsection*{%s}")
                                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                               ("report" "\\documentclass[11pt]{report}"
                                ("\\part{%s}" . "\\part*{%s}")
                                ("\\chapter{%s}" . "\\chapter*{%s}")
                                ("\\section{%s}" . "\\section*{%s}")
                                ("\\subsection{%s}" . "\\subsection*{%s}")
                                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                               ("book" "\\documentclass[11pt]{book}"
                                ("\\part{%s}" . "\\part*{%s}")
                                ("\\chapter{%s}" . "\\chapter*{%s}")
                                ("\\section{%s}" . "\\section*{%s}")
                                ("\\subsection{%s}" . "\\subsection*{%s}")
                                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t)
  (setq-default TeX-master nil))

;; Preview Pane
(use-package latex-preview-pane
  :defer t
  :commands  (latex-preview-pane-mode)
  :hook ((latex-mode-hook . latex-preview-pane-mode)))
