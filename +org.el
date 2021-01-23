;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Org Mode
;;

(use-package gnuplot)

;;;###autoload
(defun iocanel/org-drill ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'directory)
  (find-file "~/Documents/org/roam/index.org")
  (org-drill)
  (org-save-all-org-buffers))

(defun iocanel/org-drill-buffer ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'file)
  (org-drill)
  (org-save-all-org-buffers))

(use-package org-drill
  :init (setq org-drill-scope 'directory))

(add-to-list 'org-modules 'org-habit t)
(setq-default org-display-custom-times nil)
(setq org-use-tag-inheritance nil)
(setq org-stuck-projects '("+project" ("TODO" "NEXT" "NEXTACTION") nil ""))
(setq org-tag-alist '((:startgroup . nil)
                      ("project" . ?p) ("area" . ?a) ("resource" . ?r)
                      (:endgroup . nil)
                      (:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      (:endgroup . nil)
                      ("@laptop" . ?l)))

;; Agenda
(setq org-agenda-scheduled-leaders '("" ""))
(setq org-agenda-tag-filter-preset '("-drill"))
(setq org-agenda-start-day "+0") 
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 2)
(setq org-agenda-files (directory-files-recursively "~/Documents/org" "\.org$"))


(use-package org-super-agenda
  :defer t
  :config
  (setq org-super-agenda-groups '((:name "Events" :time-grid t :todo "TODAY")
                                  (:name "Habbits" :tag "habit" :todo "TODAY")
                                  (:name "Due" :deadline past)
                                  (:name "Github issues" :tag "github")))
  :hook (org-agenda-mode . org-super-agenda-mode))

(setq org-capture-templates
      '(
        ("c" "Calendar")
        ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

        ("i" "Inbox")
        ("iw" "Work Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
        ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

        ("p" "Project" entry (file+headline "~/Documents/org/roam/projects.org" "Projects")(file "~/Documents/org/templates/project.orgtmpl"))
        ("b" "BJJ")
        ("bm" "Moves" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Moves")(file "~/Documents/org/templates/bjj-move.orgtmpl"))
        ("bs" "Submission" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Submissions")(file "~/Documents/org/templates/bjj-submission.orgtmpl"))
        ("bc" "Choke" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Chokes")(file "~/Documents/org/templates/bjj-choke.orgtmpl"))
        ("bw" "Sweeps" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Sweeps")(file "~/Documents/org/templates/bjj-sweep.orgtmpl"))
        ("be" "Escapes" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Escapes")(file "~/Documents/org/templates/bjj-escape.orgtmpl"))
        ("bt" "Takedowns" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Takedowns")(file "~/Documents/org/templates/bjj-takedown.orgtmpl"))
        ("bp" "Passes" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "Techniques" "Passes")(file "~/Documents/org/templates/bjj-pass.orgtmpl"))
        ("bf" "FAQ" entry (file+olp "~/Documents/org/roam/bjj/BJJ.org" "FAQ")(file "~/Documents/org/templates/bjj-faq.orgtmpl"))

        ("h" "Habit" entry (file+olp "~/Documents/org/roam/habits.org" "Habits") (file "~/Documents/org/templates/habit.orgtmpl"))

        ("f" "Flashcards")
        ("fq" "Quotes" entry (file+headline "~/Documents/org/flashcards/quotes.org" "Quotes") "* %?\n%u" :prepend t)
        ("fS" "Stories"  entry (file+headline "~/Documents/org/flashcards/stories.org" "Stories") "* Story :drill:\n %t\n %^{The story}\n")
        ("fe" "Emacs")
        ("fef" "Emacs facts"  entry (file+headline "~/Documents/org/flashcards/emacs.org" "Emacs") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("feq" "Emacs questions"  entry (file+headline "~/Documents/org/flashcards/emacs.org" "Emacs") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fh" "History")
        ("fhf" "History facts"  entry (file+headline "~/Documents/org/flashcards/history.org" "History") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fhq" "History questions"  entry (file+headline "~/Documents/org/flashcards/history.org" "History") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fm" "Maths")
        ("fmf" "Math facts"  entry (file+headline "~/Documents/org/flashcards/maths.org" "Maths") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fmq" "Math questions"  entry (file+headline "~/Documents/org/flashcards/maths.org" "Maths") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fc" "Computer Science")
        ("fcf" "Computer Science facts"  entry (file+headline "~/Documents/org/flashcards/computer-science.org" "Computer Science") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fcq" "Computer Science questions"  entry (file+headline "~/Documents/org/flashcards/computer-science.org" "Computer Science") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fs" "Sports")
        ("fsf" "Sports facts"  entry (file+headline "~/Documents/org/flashcards/sports.org" "Sports") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fsq" "Sports questions"  entry (file+headline "~/Documents/org/flashcards/sports.org" "Sports") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fn" "Nutrition")
        ("ft" "Trading")
        ("ftf" "Trading facts"  entry (file+headline "~/Documents/org/flashcards/trading.org" "Trading") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("ftq" "Trading questions"  entry (file+headline "~/Documents/org/flashcards/trading.org" "Trading") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fl" "Languages")
        ("fls" "Spanish"  entry (file+headline "~/Documents/org/flashcards/languages/spanish.org" "Spanish") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")))

;; Org roam
(use-package org-roam
  :config
  (setq org-roam-directory "~/Documents/org/roam")
  :bind (("C-c n l" . org-roam)
         ("C-c n t" . org-roam-today)
         ("C-c n f" . org-roam-find-file)
         ("C-c n i" . org-roam-insert)
         ("C-c n g" . org-roam-show-graph))
  :hook (org-mode-hook. org-roam-mode))

;; Google Calendar
(use-package org-gcal
  :defer 3
  :init
  (setq org-gcal-client-id (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/client-id"))
        org-gcal-client-secret (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/secret"))
        org-gcal-file-alist '(("iocanel@gmail.com" .  "~/Documents/org/calendars/personal.org")
                              ("ikanello@redhat.com" . "~/Documents/org/calendars/work.org"))))


;; Org Table Aggregate
(use-package orgtbl-aggregate)

;; Deft
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/notes")
  (deft-use-filename-as-title t))

(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;;
;; Org-present
;;

(defvar iocanel/fringe-mode fringe-mode)

;;;###autoload
(defun iocanel/org-present-start ()
  (interactive)
  "Setup screen for org present."
  (org-bullets-mode 1)
  (org-present-big)
  (org-present-hide-cursor)
  (turn-on-hide-mode-line-mode)
  ;; Center the text
  (set-fringe-mode (/ (- (frame-pixel-width) (* 90 (frame-char-width))) 2))
  (org-present-read-only))

;;;###autoload
(defun iocanel/org-present-stop ()
  (interactive)
  "Exit org-present."
  (org-present-small)
  (org-present-show-cursor)
  (turn-off-hide-mode-line-mode)
  (org-present-read-write)
  (set-fringe-mode iocanel/fringe-mode))

(use-package org-present
  :defer t
  :config
  (setq org-present-text-scale 3
        org-present-run-after-navigate-functions  '(org-display-inline-images))
  :bind (("C-c a p" . org-present)
         :map org-present-mode-keymap
         ("C-q" . org-present-quit)
         ("<right>" . org-present-next)
         ("<left>" . org-present-prev))
  :hook ((org-present-mode . iocanel/org-present-start)
         (org-present-mode-quit . iocanel/org-present-stop)))

(use-package hide-mode-line :defer t)

(use-package org-ql)
;;
;; Org Functions
;;
(defun iocanel/org-heading (heading tags)
  "Format the HEADING and the TAGS in the desired way."
  (format "%-80s %s" heading tags))

(defun iocanel/org-trim-tags (h)
  "Removes all tags that are present in H."
  (if h (string-trim  (replace-regexp-in-string ":[a-zA-Z0-9_\\.-:]+:$" "" h)) nil))

(defun iocanel/org-get-entries (tag &optional f)
  (interactive)
  "Collects all headings that contain TAG from the current buffer or from file F."
  (if f (mapcar #'iocanel/org-trim-tags (org-map-entries #'org-get-heading tag 'file))
    (mapcar #'iocanel/org-trim-tags (org-map-entries #'org-get-heading tag 'agenda))))

(defun iocanel/org-get-property (file name tag property)
  "Extract the PROPERTY for NAME tagged with TAG in org FILE."
  (cdr (assoc property (car (org-ql-query
                             :select #'org-entry-properties
                             :from file
                             :where `(and (tags ,tag)
                                          (equal ,name (org-get-heading t t))))))))


;;
;; Blog
;;

(use-package org2blog
  :defer t
  :config
  (setq org2blog/wp-use-sourcecode-shortcode t
        org2blog/wp-blog-alist
        `(("iocanel.com"
           :url "https://iocanel.com/xmlrpc.php"
           :username "iocanel@gmail.com"
           :password ,(replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show websites/iocanel.com/iocanel@gmail.com")))))
  :commands (org2blog-user-interface
             org2blog-buffer-new
             org2blog-buffer-post-publish
             org2blog-buffer-page-publish
             org2blog/wp-login
             org2blog/wp-loggout
             org2blog/wp-show-post-in-browser))

;;
;; Github Issues
;;

(use-package org-github-issues :straight (org-github-issues :host github :repo "iensu/org-github-issues")
  :defer t 
  :config
  (setq
        iocanel/github-repositories '("sundrio/sundrio" "fabric8io/kubernetes-client" "dekorateio/dekorate" "quarkusio/quarkus" "snowdrp-bot/snowdrop-bot" "spring-cloud/spring-cloud-kubernetes")
        gh-user "iocanel"
        org-github-issues-org-file "~/Documents/org/roam/github.org"
        org-github-issues-tags '("github" "triage")
        org-github-issues-auto-schedule "+0d"
        org-github-issues-filter-by-assignee t
        org-github-issues-headline-prefix t)
  (mapcar (lambda (r) (run-with-idle-timer 36000 t (lambda () (org-github-issues-sync-issues r)))) iocanel/github-repositories))

;;
;; Literate capture configuration
;;
(defvar org-capture-babel-list '("~/Documents/org/roam/nutrition.org" "~/Documents/org/roam/weight.org") "List of literate capture org files.")

(defun iocanel/load-capture-babel-async (orig-fun &rest args)
  "Load org files right before calling org capture."
  (mapc 'org-babel-load-file org-capture-babel-list)
  (setq org-capture-babel-list '()))

(advice-add 'org-capture :before #'iocanel/load-capture-babel-async)

;;
;; Org Babel
;;
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((shell .t)
                                                           (ruby . t)
                                                           (java . t)
                                                           (plantuml . t))))
;;
;; Refile
;;

(setq org-refile-targets
      '(
        ("~/Documents/org/roam/bjj/BJJ.org" :maxlevel . 10)

        ;; P.A.R.A
        ("~/Documents/org/roam/projects.org" :maxlevel . 10)
        ("~/Documents/org/roam/areas.org" :maxlevel . 10)
        ("~/Documents/org/roam/resources.org" :maxlevel . 10)
        ("~/Documents/org/roam/archives.org" :maxlevel . 10)))

;;
;; Agenda
;;

(defun iocanel/org-agenda-export ()
  "Export the content of org-agenda"
  (interactive)
  (org-eval-in-environment (org-make-parameter-alist
                            `(org-agenda-span 'day
                              org-agenda-use-time-grid t
                              org-agenda-remove-tags t
                              org-agenda-window-setup 'nope))
    (let* ((wins (current-window-configuration))
           org-agenda-sticky)
      (save-excursion
        (with-current-buffer
            (get-buffer-create org-agenda-buffer-name)
          (pop-to-buffer (current-buffer))
          (org-agenda nil "t")
          (let ((result (buffer-string)))
            (with-temp-file "~/.agenda" (insert result)))))
      (set-window-configuration wins))))


;;
;; Bindings
;;

(global-set-key (kbd "C-c C-c") #'org-capture)


;;
;; Imgflip
;;

(use-package imgflip.el :straight (imgflip.el :host github :repo "iocanel/imgflip.el")
  :custom (imgflip-download-dir "~/.imgflip")
  :config
  (setq imgflip-username "iocanel"
        imgflip-password (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/imgflip/iocanel/password"))))

;;
;; Inline animated gif
;;

;; Credits: https://ivanaf.com/animating_gifs_in_orgmode.html  

(defun org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-image-overlay) t) (overlays-at (point)))))

(defun org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (let ((image (org-inline-image--get-current-image)))
    (when image
      (overlay-get image prop))))

(defun org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (org-inline-image--get 'display)))
    (when (image-multi-frame-p image-props)
      (image-animate image-props))))

(defun org-inline-image-animate-auto ()
  (interactive)
  (when (eq 'org-mode major-mode)
    (while-no-input 
      (run-with-idle-timer 0.3 nil 'org-inline-image-animate))))

(setq org-inline-image--get-current-image (byte-compile 'org-inline-image--get-current-image))
(setq org-inline-image-animate  (byte-compile 'org-inline-image-animate ))
(add-hook 'post-command-hook 'org-inline-image-animate-auto)

;;
;; Window manager integraion
;;

;; Credits: https://www.reddit.com/r/emacs/comments/74gkeq/system_wide_org_capture

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (and (equal "org-capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  (delete-frame))

