(use-package vterm
  :defer t
  :commands (vterm)
  :bind ("C-x v" . 'vterm))

(use-package eshell
  :defer t
  :config
  (setq eshell-buffer-maximum-lines 2048
        eshell-history-size 100000
        eshell-banner-message ""
        eshell-path-env (getenv "PATH"))

  (defun ic/eshell-clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun ic/eshell-refresh-compilation-mode () 
    "Re-enable compilation mode"
    (interactive)
    ;; compilation shell mode doesn't work properly so it needs a nudge every now and then
    (compilation-shell-minor-mode -1)
    (compilation-shell-minor-mode 1))


  (defun ic/eshell-open-eww ()
    (interactive)
    "Start eww from the current ehsell buffer."
    (let* ((port (ic/find-web-port))
           (url (format "http://localhost:%s" port)))
      (eww url)))

  (defun ic/eshell-open-xwidget-webkit ()
    (interactive)
    "Start eww from the current ehsell buffer."
    (let* ((port (ic/find-web-port))
           (url (format "http://localhost:%s" port)))
      (xwidget-webkit-browse-url url)))

  (defun ic/find-web-port ()
    "Find the web port in the current buffer, or return 8080 if none is found."
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward "port.* \\([0-9]+\\)" nil t)
          (match-string 1)
        8080)))


  :bind (:map eshell-command-mode-map
              ("M-w" . 'ic/eshell-open-eww)
              ("M-x" . 'ic/eshell-open-xwidget-webkit))
  :hook ((eshell-pre-command . (lambda ()
                                 (ic/eshell-refresh-compilation-mode)
                                 (setq eshell-path-env (getenv "PATH"))))
         (eshell-mode . (lambda()
                          (setq eshell-path-env (getenv "PATH"))
                          (eshell/alias "mvn" "~/bin/mvnnotify $*")
                          (eshell/alias "cls" "ic/eshell-clear")
                          (eshell/alias "clear" "ic/eshell-clear")
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

