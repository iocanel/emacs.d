(org-babel-load-file
 (expand-file-name
  "readme.org"
  user-emacs-directory))

;; Load container build info if available
(let ((build-info-file (expand-file-name "build-info.el" user-emacs-directory)))
  (when (file-exists-p build-info-file)
    (load build-info-file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asana-default-workspace '("CYBER RANGES" . "1200603659620142"))
 '(custom-safe-themes
   '("4825b816a58680d1da5665f8776234d4aefce7908594bea75ec9d7e3dc429753"
     default))
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("/home/iocanel/Documents/org/github.org"
     "/home/iocanel/Documents/org/habits.org"
     "/home/iocanel/Documents/org/nutrition.org"
     "/home/iocanel/Documents/org/workout.org"
     "/home/iocanel/Documents/org/calendars/personal.org"
     "/home/iocanel/Documents/org/calendars/work.org"
     "/home/iocanel/Documents/org/roam/Inbox.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-07.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-08.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-09.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-10.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-11.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-29.org"
     "/home/iocanel/Documents/org/roam/daily/2022-11-30.org"
     "/home/iocanel/Documents/org/roam/daily/2022-12-01.org"
     "/home/iocanel/Documents/org/roam/daily/2023-01-16.org"
     "/home/iocanel/Documents/org/roam/daily/2023-02-03.org"
     "/home/iocanel/Documents/org/roam/daily/2023-02-17.org"
     "/home/iocanel/Documents/org/roam/daily/2023-02-20.org"
     "/home/iocanel/Documents/org/roam/daily/2023-02-24.org"
     "/home/iocanel/Documents/org/roam/daily/2023-03-02.org"
     "/home/iocanel/Documents/org/roam/daily/2024-06-19.org"
     "/home/iocanel/Documents/org/roam/daily/2024-07-22.org"
     "/home/iocanel/Documents/org/roam/daily/2026-01-09.org"))
 '(org-ditaa-jar-path
   "/nix/store/0g51hvz2286c7skks1hcykgr3s6fx2qq-ditaa-0.11.0/lib/ditaa.jar")
 '(safe-local-variable-values
   '((org-roam-dailies-capture-templates
      ("d" "default" entry "* %?" :target
       (file+head "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d>\12* Training Log\12 #+SESSION: B%(format \"%s\" (length (directory-files \"~/Documents/org/roam-jj/daily\" t \"\\.org$\"))) \12** Techniques\12 %?\12** Rolling\12")))
     (org-roam-directory . "/home/iocanel/Documents/org/roam-jj"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
