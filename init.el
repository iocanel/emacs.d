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

(use-package org :straight t)
(straight-use-package '(org :type built-in))

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
 '(bongo-mplayer-extra-arguments '("-af" "scaletempo"))
 '(custom-safe-themes
   '("cf3d5d77679f7daed6a2c863e4f2e30427d5e375b254252127be9359957502ec" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "9975a41cbff4a4f13123f486ba78b536c1a6c0a2f23fcf28bdedac0af303f3b5" "21c112521fddd470f525988897822d2999024105f300e8e33392bed9bb23bf76" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "34b3219ae11acd81b2bb7f3f360505019f17d7a486deb8bb9c1b6d13c6616d2e" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "69fba6d0772396726a4e5539f67616c42df77d69d6ed0584618a43533e2fc4ad" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "427fa665823299f8258d8e27c80a1481edbb8f5463a6fb2665261e9076626710" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "1a6d627434899f6d21e35b85fee62079db55ef04ecd9b70b82e5d475406d9c69" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e838d6375a73fda607820c65eb3ea1f9336be7bd9a5528c9161e10c4aa663b5b" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "4ea0aa360264ff861fb0212abe4161b83ad1d8c8b74d8a04bcd1baf0ebdceeae" default))
 '(deft-default-extension "org" t)
 '(deft-directory "~/Documents/org/notes" t)
 '(deft-recursive t t)
 '(deft-use-filename-as-title t t)
 '(deft-use-filter-string-for-filename t t)
 '(idee-quarkus-version "999-SNAPSHOT")
 '(idee-tree-enabled-default nil)
 '(imgflip-download-dir "~/Downloads/imgflip/")
 '(org-github-issues-filter-by-assignee t)
 '(org-github-issues-tag-transformations '(("[ /-]+" "_")))
 '(org-image-actual-width nil)
 '(request-log-level 'debug)
 '(ring-bell-function 'ignore)
 '(sendmail-program "/usr/local/bin/msmtp" t)
 '(yas-prompt-functions '(yas-completing-prompt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
