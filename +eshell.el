(use-package eshell
  :defer t
  :config
  (setq eshell-buffer-maximum-lines 2048
        eshell-history-size 100000
        eshell-banner-message "")
  :hook ((eshell-post-command . iocanel/eshell-compilation-mode)
         (eshell-mode-hook . (lambda()
                               (eshell/alias "cls" "iocanel/eshell-clear")
                               (eshell/alias "clear" "iocanel/eshell-clear")
                               (eshell/alias "d" "dired $1")
                               (eshell/alias "e" "find-file $1")
                               (eshell/alias "ee" "find-file-other-window $1")
                               (eshell/alias "emacs" "find-file-other-window $1")
                               (eshell/alias "ff" "find-file $1")
                               (eshell/alias "gd" "magit-diff-unstaged")
                               (eshell/alias "gds" "magit-diff-staged")
                               (eshell/alias "ll" "ls -AlohG --color=always $*")
                               (eshell/alias "ls" "TERM=ansi ls --color=always $*")
                               (eshell/alias "mci" "mvn clean install")
                               (eshell/alias "vi" "find-file-silent $1")
                               (eshell/alias "kc" "kubectl $*")))))

(defun iocanel/eshell-clear ()
  (interactive)
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))
;;    (eshell-send-input nil nil nil)))

(defun iocanel/eshell-compilation-mode () 
  "Enable compilation mode"
  (interactive)
  ;; compilation shell mode doesn't work properly so it needs a nudge every now and then
  (compilation-shell-minor-mode -1)
  (compilation-shell-minor-mode 1))



