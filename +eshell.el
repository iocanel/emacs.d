(use-package eshell
  :defer t
  :config
  (setq eshell-buffer-maximum-lines 2048
        eshell-history-size 100000
        eshell-banner-message ""
        eshell-path-env (getenv "PATH"))
  :hook ((eshell-pre-command . (lambda ()
                                  (iocanel/eshell-refresh-compilation-mode)
                                  (setq eshell-path-env (getenv "PATH"))))
         (eshell-mode . (lambda()
                               (setq eshell-path-env (getenv "PATH"))
                               (eshell/alias "mvn" "~/bin/mvnnotify $*")
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
                               (eshell/alias "vi" "find-file-silent $1")
                               (eshell/alias "kc" "kubectl $*")))))

;;;###autoload
(defun iocanel/eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
;;    (eshell-send-input nil nil nil)))

;;;###autoload
(defun iocanel/eshell-refresh-compilation-mode () 
  "Re-enable compilation mode"
  (interactive)
  ;; compilation shell mode doesn't work properly so it needs a nudge every now and then
  (compilation-shell-minor-mode -1)
  (compilation-shell-minor-mode 1))



