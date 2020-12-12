(use-package emacs
  :bind (:map profiler-report-mode-map
              ("j" . profiler-report-next-entry)
              ("k" . profiler-report-previous-entry)
              ("d" . profiler-report-describe-entry)
              ("<tab>" . profiler-report-toggle-entry)))
  
