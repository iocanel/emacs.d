;;; ~/.config/emacs/+email.el -*- lexical-binding: t; -*-

(use-package mu4e
  :straight (mu4e :type git :host github :repo "djcb/mu")
  :bind (("C-c a m m" . mu4e)
         ("C-c a m n" . mu4e-compose-new)
         ("C-c a m u" . iocanel/mu4e-view-unread))
  :config
  (setq user-mail-address "iocanel@gmail.com"
        user-full-name "Ioannis Canellos"
        mu4e-maildir "~/.mail"
        mu4e-compose-context-policy 'ask-if-none
        mu4e-context-policy 'pick-first
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
                     (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Actionable")
                     (mu4e-refile-folder               . "/iocanel@gmail.com/[Email] Archived")
                     (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Deferred")
                     (mu4e-trash-folder                . "/iocanel@gmail.com/Trash")
                     (mu4e-sent-folder                 . "/iocanel@gmail.com/Sent")

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
                     (mu4e-drafts-folder               . "/ikanello@gmail.com/Drafts")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Actionable")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Archived")
                     (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Deferred")
                     (mu4e-trash-folder                . "/ikanello@gmail.com/Trash")
                     (mu4e-sent-folder                 . "/ikanello@gmail.com/Sent")
                     (user-full-name                   . "Ioannis Canellos")
                     (message-send-mail-function       . message-send-mail-with-sendmail)
                     (sendmail-program                 . "/usr/bin/msmtp")
                     (message-sendmail-extra-arguments . ("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
                     (message-sendmail-f-is-evil       . t)
                     (mu4e-sent-messages-behavior      . delete)
                     (mu4e-compose-signature           .  t))))))

(use-package org-mu4e
  :straight (org-mu4e :type git :host github :repo "djcb/mu")
  :config
  (setq org-mu4e-link-query-in-headers-mode nil))

(use-package evil-mu4e)

;; Mu4e cusotmization
(setq doom-modeline-mu4e t)

(set-face-attribute 'mu4e-replied-face nil :inherit 'link :underline nil)
(set-face-attribute 'mu4e-trashed-face nil :foreground "#555555")
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(setq mu4e-headers-results-limit 1000000)
;; Why would I want to leave my message open after I've sent it?
(setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; Don't ask to quit... why is this the default?
(setq mu4e-confirm-quit nil)
(setq mu4e-headers-visible-lines 25)
;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

(defalias 'org-mail 'org-mu4e-compose-org-mode)

(add-hook 'mu4e-view-mode-hook 'mu4e-mark-region-code)
                ;;; Show Smileys
(add-hook 'mu4e-view-mode-hook 'smiley-buffer)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 72)
            (auto-fill-mode 0)
            (visual-fill-column-mode)
            (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
            (visual-line-mode)))

(defun no-auto-fill ()
  "Turn off auto-fill-mode."
  (auto-fill-mode -1))

(add-hook 'mu4e-compose-mode-hook #'no-auto-fill)
(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Mu4e Bookmarks
(setq mu4e-bookmarks
      '(
        ("date:2d..now AND flag:unread AND NOT flag:trashed AND not flag:list AND date:30d..now AND (to:iocanel or ikanello) AND NOT from:Connect2Go" "Must read" ?r)
        ("flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\" AND NOT from:Connect2Go" "Unread messages" ?U)
        ("date:2d..now AND flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\" AND NOT from:Connect2Go" "Recent unread messages" ?u)
        ("mime:text/calendar" "Events" ?E)
        ("date:30d..now AND mime:text/calendar" "Recent Events" ?e)
        ("not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Personal" ?P)
        ("date:2d..now AND not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Recent Personal" ?p)
        ("date:today" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("from:Connect2Go" "Home events" ?h)))

;; Sending Emails
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq message-sendmail-extra-arguments '("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)
(setq message-kill-buffer-on-exit t)

;;
;; Key bindings
;;
(define-key mu4e-view-mode-map (kbd "C-<tab>") #'mu4e-view-headers-next-unread)
(define-key mu4e-headers-mode-map (kbd "C-<tab>") #'mu4e-headers-next-unread)

;;
;; Functions
;;

;;;###autoload
(defun iocanel/mark-thread-as-read()
  (interactive)
  "Skip all messages from the current thread."
  (save-excursion
    (select-window (get-buffer-window "*mu4e-headers*"))
    (recenter)
    (mu4e-headers-mark-thread t '(read))))

(define-key mu4e-view-mode-map (kbd "C-t") #'iocanel/mark-thread-as-read)
(define-key mu4e-headers-mode-map (kbd "C-t") #'iocanel/mark-thread-as-read)

;;;###autoload
(defun iocanel/mu4e-view-unread()
  (interactive)
  "Open my unread messages."
  (require 'mu4e)
  (mu4e-headers-search
   (mu4e-bookmark-query (car (remove-if-not (lambda (s) (equal (mu4e-bookmark-name s) "Unread messages")) (mu4e-bookmarks))))))

;(define-key evil-normal-state-map (kbd "SPC a m u") #'iocanel/mu4e-view-unread)
;(global-set-key (kbd "C-c a m u") #'iocanel/mu4e-view-unread)

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
