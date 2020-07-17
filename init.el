(setq inhibit-startup-screen t)

;; Hide modeline
(setq mode-line-format nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use package integration
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-check-for-modifications 'never)

;(use-package org :straight t)
;(straight-use-package '(org :type built-in))

;; Profile emacs
(use-package esup)

;; Load the literate configuration
(load-file (expand-file-name "~/.config/emacs/minimal.el"))

;; Load configuration
;; (load-file (expand-file-name "~/.config/emacs/modules/core.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/evil.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/editor.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/ui.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/term.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/vcs.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/lang/java.el"))
;; (load-file (expand-file-name "~/.config/emacs/modules/lang/go.el"))

;; Load quickmarks
;; (load-file (expand-file-name "~/.config/emacs/quickmarks.el"))

;;
;; Generated Stuff
;;

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "1a6d627434899f6d21e35b85fee62079db55ef04ecd9b70b82e5d475406d9c69" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e838d6375a73fda607820c65eb3ea1f9336be7bd9a5528c9161e10c4aa663b5b" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "4ea0aa360264ff861fb0212abe4161b83ad1d8c8b74d8a04bcd1baf0ebdceeae" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/Documents/org/notes" t)
 '(deft-recursive t t)
 '(deft-use-filename-as-title t t)
 '(deft-use-filter-string-for-filename t t)
 '(idee-quarkus-version "999-SNAPSHOT"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
