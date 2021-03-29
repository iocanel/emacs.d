(use-package ace-link
  :defer t
  :bind  (:map eww-mode-map
               ("<" . eww-back-url)
               (">" . eww-forward-url)
               ("C-c f" . 'ace-link-eww)))

(defvar ic/eww-ignore-tag-nav-enabled t "Ignore navigation tag")
  
(defun ic/eww-ignore-tag-nav (dom)
  "Ignores navigation tag."
  (when (not ic/eww-ignore-tag-nav-enabled) (shr-generic dom)))

(add-to-list 'shr-external-rendering-functions '(nav . ic/eww-ignore-tag-nav))
