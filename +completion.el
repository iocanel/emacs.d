(use-package company
  :defer t
  :config
  (setq company-tooltip-limit 20                      ; bigger popup window
        company-idle-delay 0.5                        ; decrease delay before autocompletion popup shows
        company-echo-delay 0                          ; remove annoying blinking
        company-begin-commands '(self-insert-command) ; start autocompletion only after typing
        company-tooltip-align-annotations t           ; aligns annotation to the right hand side
        company-dabbrev-downcase nil)                 ; don't downcase
  :hook (prog-mode-hook . company-mode-hook))

(use-package smart-tab
  :config
  (progn
    (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                             try-complete-file-name-partially))
                                        ;try-expand-dabbrev
                                        ;try-expand-dabbrev-visible
                                        ;try-expand-dabbrev-all-buffers
                                        ;try-complete-lisp-symbol-partially
                                        ;try-complete-lisp-symbol

     smart-tab-user-provided-completion-function 'company-complete
     smart-tab-using-hippie-expand t
     smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode inferior-python-mode)
    (global-smart-tab-mode 1)))
