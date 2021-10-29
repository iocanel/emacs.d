(defun ic/show-nickserv-password (server username)
  "Returns the nicker password for USERNAME on SERVER."
  (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string (format "pass show services/irc/nickserv/%s/%s" server username))))

(defun ic/show-irc-password (username)
  "Returns the freenode password for iocanel."
  (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string (format "pass show services/irc/irc.com/%s" username))))

(use-package erc
  :ensure t
  :config
  (setq erc-nick "iocanel"
        erc-full-name "Ioannis Canellos"
        erc-nickserv-passwords `(
                                 (freenode (("iocanel" . ,(ic/show-nickserv-password "freenode.org" "iocanel"))))
                                 (libera (("iocanel" . ,(ic/show-nickserv-password "libera.chat" "iocanel"))))))
  (erc-services-mode 1))

;;
;; Sasl Support
;;

(use-package erc-sasl :straight (erc-sasl :host gitlab :repo "psachin/erc-sasl" :files (:defaults "erc-sasl.el"))
  :after erc
  :config
  (add-to-list 'erc-sasl-server-regexp-list (regexp-quote "chat.freenode.net"))
  (setq erc-session-password (pass-show-freenode-iocanel))
  (defun erc-login ()
        "Perform user authentication at the IRC server."
        (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                         (erc-current-nick)
                         (user-login-name)
                         (or erc-system-name (system-name))
                         erc-session-server
                         erc-session-user-full-name))
        (if erc-session-password
            (erc-server-send (format "PASS %s" erc-session-password))
          (message "Logging in without password"))
        (when (erc-sasl-use-sasl-p)
          (erc-server-send "CAP REQ :sasl"))
        (erc-server-send (format "NICK %s" (erc-current-nick)))
        (erc-server-send
         (format "USER %s %s %s :%s"
                 ;; hacked - S.B.
                 (if erc-anonymous-login erc-email-userid (user-login-name))
                 "0" "*"
                 erc-session-user-full-name))
        (erc-update-mode-line)))
;;
;; Servers
;;
(defun erc/freenode ()
  "Connect to freenode irc server."
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "7000" :nick "iocanel" :password (ic/show-irc-passowrd "iocanel")))

(defun erc/libera ()
  "Connect to libera irc server."
  (interactive)
  (erc-tls :server "irc.libera.chat" :port "7000" :nick "iocanel"))
