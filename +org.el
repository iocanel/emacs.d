;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Org Mode
;;

;;;###autoload
(defun iocanel/org-drill ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'directory)
  (find-file "~/Documents/org/roam/index.org")
  (org-drill)
  (org-save-all-org-buffers))

(use-package org-drill
   :init (setq org-drill-scope 'directory))

  (add-to-list 'org-modules 'org-habit t)
  (setq-default org-display-custom-times nil)
  (setq org-use-tag-inheritance nil)
  (setq org-agenda-tag-filter-preset '("-drill"))
  (setq org-agenda-files (directory-files-recursively "~/Documents/org" "\.org$"))
  (setq org-stuck-projects '("+project" ("TODO" "NEXT" "NEXTACTION") nil ""))
  (setq org-tag-alist '((:startgroup . nil)
                        ("project" . ?p) ("area" . ?a) ("resource" . ?r)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("@laptop" . ?l)))

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
(defun iocanel/org-gcal-init ()
  (setq org-gcal-client-id (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/client-id"))
        org-gcal-client-secret (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/secret"))
        org-gcal-file-alist '(("iocanel@gmail.com" .  "~/Documents/org/calendars/personal.org")
                              ("ikanello@redhat.com" . "~/Documents/org/calendars/work.org"))))

(run-with-idle-timer 3 nil (lambda () (iocanel/org-gcal-init)))

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

;;;###autoload
(defun iocanel/org-present-start ()
  (interactive)
  "Setup screen for org present."
  (org-bullets-mode 1)
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (turn-on-hide-mode-line-mode)
  (org-present-read-only))

;;;###autoload
(defun iocanel/org-present-stop ()
  (interactive)
  "Exit org-present."
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (turn-off-hide-mode-line-mode)
  (org-present-read-write))
  
(use-package org-present
  :defer t
  :config
  (setq org-present-text-scale 3)
  :bind (("C-c a p" . org-present)
         :map org-present-mode-keymap
         ("C-q" . org-present-quit)
         ("<right>" . org-present-next)
         ("<left>" . org-present-prev))
  :hook ((org-present-mode . iocanel/org-present-start)
         (org-present-mode-quit . iocanel/org-present-stop)))

(use-package hide-mode-line :defer t)
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
(defun iocanel/org2blog-init ()
  (setq org2blog/wp-use-sourcecode-shortcode t)
  (setq org2blog/wp-blog-alist
        `(("iocanel.com"
           :url "https://iocanel.com/xmlrpc.php"
           :username "iocanel@gmail.com"
           :password ,(replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show websites/iocanel.com/iocanel@gmail.com"))))))

;;
;; Literate capture configuration
;;
(defvar org-capture-babel-list '("~/Documents/org/roam/nutrition.org") "List of literate capture org files.")

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
;; Bindings
;;

(global-set-key (kbd "C-c C-c") #'org-capture)
