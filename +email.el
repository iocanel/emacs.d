;;; ~/.config/emacs/+email.el -*- lexical-binding: t; -*-

;; We also need to jump into the mu4e folder and run `make`

(use-package mu4e
  :defer t
  :straight (mu4e :type git :host github :repo "djcb/mu" :branch "1.6.9-signed" :files (:defaults "mu4e/*.el"))
  :defer t
  :commands (mu4e mu4e-compose-new ic/mu4e-view-unread ic/mu4e-force-next-unread ic/mu4e-mark-thread-as-unread)
  :config
  (setq user-mail-address "iocanel@gmail.com"
        user-full-name "Ioannis Canellos"
        mu4e-maildir "~/.mail"

        ;; Having Error: 102: failed to move message
        ;; The following block of config is suggested by https://github.com/djcb/mu/issues/2053
        mu4e-index-lazy-check nil
        mu4e-change-filenames-when-moving t

        mu4e-compose-context-policy 'ask
        mu4e-context-policy 'ask
        mu4e-contexts
        `( ,(make-mu4e-context
             :name "personal"
             :enter-func (lambda () (mu4e-message "Switch to iocanel@gmail.com"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/iocanel@gmail.com" (mu4e-message-field msg :maildir))))
             :vars '((smtpmail-smtp-user               . "iocanel@gmail.com")
                     (mail-reply-to                    . "iocanel@gmail.com")
                     (user-mail-address                . "iocanel@gmail.com")
                     (user-full-name                   . "Ioannis Canellos")
                     (mu4e-user-mail-address-list      . "~/.mail/iocanel@gmail.com")
                     (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Actionable")
                     (mu4e-refile-folder               . "/iocanel@gmail.com/[Email] Archived")
                     (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Deferred")
                     (mu4e-trash-folder                . "/iocanel@gmail.com/Trash")
                     (mu4e-sent-folder                 . "/iocanel@gmail.com/Sent")
                     (mu4e-compose-complete-addresses  . t)

                     (message-send-mail-function       . message-send-mail-with-sendmail)
                     (sendmail-program                 . "/usr/bin/msmtp")
                     (message-sendmail-extra-arguments . ("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
                     (message-sendmail-f-is-evil       . t)
                     (mu4e-sent-messages-behavior      . delete)
                     (mu4e-compose-signature           . t)))
           ,(make-mu4e-context
             :name "redhat"
             :enter-func (lambda () (mu4e-message "Switch to ikanello@redhat.com"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match-p "^/ikanello@redhat.com" (mu4e-message-field msg :maildir))))
             :vars '((smtpmail-smtp-user               . "ikanello@redhat.com")
                     (mail-reply-to                    . "ikanello@redhat.com")
                     (user-mail-address                . "ikanello@redhat.com")
                     (user-full-name                   . "Ioannis Canellos")
                     (mu4e-user-mail-address-list      . "~/.mail/ikanello@redhat.com")
                     (mu4e-drafts-folder               . "/ikanello@gmail.com/Drafts")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Actionable")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Archived")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Deferred")
                     (mu4e-trash-folder                . "/ikanello@gmail.com/Trash")
                     (mu4e-sent-folder                 . "/ikanello@gmail.com/Sent")
                     (mu4e-compose-complete-addresses  . t)
                     (message-send-mail-function       . message-send-mail-with-sendmail)
                     (sendmail-program                 . "/usr/bin/msmtp")
                     (message-sendmail-extra-arguments . ("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
                     (message-sendmail-f-is-evil       . t)
                     (mu4e-sent-messages-behavior      . delete)
                     (mu4e-compose-signature           .  t)))))

  (set-face-attribute 'mu4e-replied-face nil :inherit 'link :underline nil)
  (set-face-attribute 'mu4e-trashed-face nil :foreground "#555555")

  (setq mu4e-update-interval nil)
  (setq mu4e-headers-results-limit 1000000)
  ;; Why would I want to leave my message open after I've sent it?
  (setq message-kill-buffer-on-exit t)
  ;; Don't ask for a 'context' upon opening mu4e
  (setq mu4e-context-policy 'pick-first)
  ;; Don't ask to quit... why is this the default?
  (setq mu4e-confirm-quit nil)
  (setq mu4e-headers-visible-lines 25)
  ;; convert org mode to HTML automatically
  
  ;; enable inline images
  (setq mu4e-view-show-images t)
  (setq org-mu4e-convert-to-html t)

  ;; Mu4e Bookmarks
  (setq mu4e-bookmarks
        '(
          ("date:2d..now AND flag:unread AND NOT flag:trashed AND not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Must read" ?i)

          ("NOT flag:trashed AND NOT maildir:\"/Archived\"" "Messages (all)" ?U)
          ("flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Messages (unread)" ?u)

          ("not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Personal (all)" ?P)
          ("flag:unread AND not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Personal (unread)" ?p)

          ;; Github
          ("from:github AND AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Github (all)" ?G)
          ("flag:unread AND from:github AND AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Github (unread)" ?g)
          ("flag:unread AND from:notifications@github.com AND AND NOT flag:trashed AND cc:review_requested AND NOT maildir:\"/Archived\"" "Github (review)" ?r)
          ("flag:unread AND from:notifications@github.com AND AND NOT flag:trashed AND cc:mention AND NOT maildir:\"/Archived\"" "Github (mentions)" ?m)

          ;; Events
          ("mime:text/calendar" "Events (all)" ?E)
          ("flat:unread AND mime:text/calendar" "Events (unread)" ?e)

          ;; Period
          ("date:today" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)))


  ;; Sending Emails
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments '("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  (setq message-kill-buffer-on-exit t)
  (setq doom-modeline-mu4e t)

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions '("follow up" . ic/mu4e-capture-follow-up) t)
  (add-to-list 'mu4e-view-actions '("follow up" . ic/mu4e-capture-follow-up) t)
  (add-to-list 'mu4e-headers-actions '("read later" . ic/mu4e-capture-read-later) t)
  (add-to-list 'mu4e-view-actions '("read later" . ic/mu4e-capture-read-later) t)

  ;;
  ;; Functions
  ;;

  (defun ic/mu4e-force-next-unread()
    "View next unread closing the current message if stuck in loading."
    (interactive)
    (if (eq 'mu4e-loading-mode major-mode)
        (progn
          (select-window (get-buffer-window "*mu4e-headers*"))
          (delete-other-windows)
          (mu4e-view-headers-next-unread)
          (mu4e-headers-view-message))
      (mu4e-view-headers-next-unread)))

  (defun ic/mu4e-mark-thread-as-read()
    "Skip all messages from the current thread."
    (interactive)
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter)
      (mu4e-headers-mark-thread t '(read))))

  (defun ic/mu4e-view-unread()
    "Open my unread messages."
    (interactive)
    (require 'mu4e)
    (mu4e-headers-search
     (mu4e-bookmark-query (car (remove-if-not (lambda (s) (equal (mu4e-bookmark-name s) "Unread messages")) (mu4e-bookmarks))))))


  ;; Capturing, source: https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org#adding-custom-actions-for-quick-capturing
  (defun ic/mu4e-capture-follow-up (&optional msg)
    "Create a follow up todo item."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ef"))

  (defun ic/mu4e-capture-read-later (&optional msg)
    "Create a read later todo item."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "er"))


  (defun ic/mu4e-get-incoming-count ()
    "Count the number of unread messages."
    (let* ((query "flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\"")
           (command (format "mu find '%s' 2>/dev/null | wc -l" query)))
      (string-trim (shell-command-to-string command))))

  ;;
  ;; Advices
  ;;

  ;; Scroll mu4e-header along with next/prev messages
  (defadvice mu4e-view-headers-next (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-prev (around scroll-up-mu4e-header activate)
    "Scroll up the mu4e-header window when moving onto prev email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-next-unread (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-prev-unread (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (other-window 1)
      (recenter))
    ad-do-it)

  (ad-activate 'mu4e-view-headers-next)
  (ad-activate 'mu4e-view-headers-prev)
  (ad-activate 'mu4e-view-headers-next-unread)
  (ad-activate 'mu4e-view-headers-prev-unread)

  ;; Mu4e cusotmization
  (defalias 'org-mail 'org-mu4e-compose-org-mode)
                                        ; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

  (defun no-auto-fill ()
    "Turn off auto-fill-mode."
    (auto-fill-mode -1))

  :hook ((mu4e-view-mode . (lambda () (mu4e-mark-region-code) (smiley-buffer)))
         (mu4e-compose-mode . (lambda ()
                                (no-auto-fill)
                                (set-fill-column 72)
                                (auto-fill-mode 0)
                                (visual-fill-column-mode)
                                (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
                                (visual-line-mode))))
  :bind (("C-c a m m" . mu4e)
         ("C-c a m n" . mu4e-compose-new)
         ("C-c a m u" . ic/mu4e-view-unread)
         :map evil-normal-state-map
         ("SPC a m u" . ic/mu4e-view-unread)
         :map mu4e-view-mode-map
         ("C-<tab>" . mu4e-view-headers-next-unread)
         ("C-t" . ic/mu4e-mark-thread-as-read)
         ("C-r" . ic/mu4e-capture-read-later)
         ("C-f" . ic/mu4e-capture-follow-up)
         :map mu4e-loading-mode-map
         ("C-<tab>" . ic/mu4e-force-next-unread)
         :map mu4e-headers-mode-map
         ("C-<tab>" . mu4e-headers-next-unread)
         ("C-t" . ic/mu4e-mark-thread-as-read)
         ("C-r" . ic/mu4e-capture-read-later)
         ("C-f" . ic/mu4e-capture-follow-up)))
