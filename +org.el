;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Org Mode
;;
(use-package gnuplot
  :defer t
  :commands (gnuplot-mode))

(use-package org
  :commands (org-capture org-agenda org-refile)
  :config
  (add-to-list 'org-modules 'org-habit t)
  (setq-default org-display-custom-times nil)
  (setq org-use-tag-inheritance nil
        org-stuck-projects '("+project" ("TODO" "NEXT" "NEXTACTION") nil "")
        org-tag-alist '((:startgroup . nil)
                        ("project" . ?p) ("area" . ?a) ("resource" . ?r)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("@laptop" . ?l))))

   :bind (("C-c c" . ic/org-edit-special)
         ("M-n" . ic/next-code-block)
         ("M-p" . ic/previous-code-block)
          :map org-src-mode-map
         ("C-x x" . org-edit-exit)))

(setq org-capture-templates
      '(
        ("c" "Calendar")
        ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

        ("i" "Inbox")
        ("iw" "Work Inbox" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
        ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

        ("e" "Email Workflow")
        ("ef" "Follow Up" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Email" "Follow Up") "* TODO Follow up with %:fromname on %a :email:\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
        ("er" "Read Later" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Email" "Read Later") "* TODO Read %:subject :email: \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)

        ("p" "Project" entry (file+headline "~/Documents/org/para/projects.org" "Projects")(file "~/Documents/org/templates/project.orgtmpl"))
        ("b" "BJJ")
        ("bm" "Moves" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Moves")(file "~/Documents/org/templates/bjj-move.orgtmpl"))
        ("bs" "Submission" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Submissions")(file "~/Documents/org/templates/bjj-submission.orgtmpl"))
        ("bc" "Choke" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Chokes")(file "~/Documents/org/templates/bjj-choke.orgtmpl"))
        ("bw" "Sweeps" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Sweeps")(file "~/Documents/org/templates/bjj-sweep.orgtmpl"))
        ("be" "Escapes" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Escapes")(file "~/Documents/org/templates/bjj-escape.orgtmpl"))
        ("bt" "Takedowns" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Takedowns")(file "~/Documents/org/templates/bjj-takedown.orgtmpl"))
        ("bp" "Passes" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Passes")(file "~/Documents/org/templates/bjj-pass.orgtmpl"))
        ("bf" "FAQ" entry (file+olp "~/Documents/org/bjj/BJJ.org" "FAQ")(file "~/Documents/org/templates/bjj-faq.orgtmpl"))

        ("h" "Habit" entry (file+olp "~/Documents/org/habits.org" "Habits") (file "~/Documents/org/templates/habit.orgtmpl"))

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
;;
;; Bindings
;;

(global-set-key (kbd "C-c C-c") #'org-capture)

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


(use-package org-drill
  :defer t
  :commands (ic/org-drill ic/org-drill-buffer)
  :config

  (defun ic/org-drill ()
    "Require, configure and call org-drill."
    (interactive)
    (require 'org-drill)
    (setq org-drill-scope 'directory)
    (find-file "~/Documents/org/index.org")
    (org-drill)
    (org-save-all-org-buffers))

  (defun ic/org-drill-buffer ()
    "Require, configure and call org-drill."
    (interactive)
    (require 'org-drill)
    (setq org-drill-scope 'file)
    (org-drill)
    (org-save-all-org-buffers))
  :init (setq org-drill-scope 'directory))

(use-package org-super-agenda
  :defer t
  :commands (ic/org-agenda-browse-at-point ic/org-agenda-archive-at-point ic/org-agenda-export ic/org-archive ic/org-refile)
  :config
  (setq org-super-agenda-groups '((:name "Events" :time-grid t :todo "TODAY")
                                  (:name "Habbits" :tag "habit" :todo "TODAY")
                                  (:name "Due" :deadline past)
                                  (:name "Jira" :tag "jira")
                                  (:name "Email" :tag "email")
                                  (:name "Github pulls" :tag "pull")
                                  (:name "Github issues" :tag "issue"))
        ;; agenda        
        org-agenda-scheduled-leaders '("" "")
        org-agenda-tag-filter-preset '("-drill")
        org-agenda-start-day "+0"
        org-agenda-start-on-weekday nil
        org-agenda-span 2
        org-agenda-files (append
                          (directory-files-recursively "~/Documents/org/gtg" "\.org$")
                          (directory-files-recursively "~/Documents/org/jira" "\.org$")
                          '("~/Documents/org/habits.org" "~/Documents/org/github.org" "~/Documents/org/nutrition.org"))
        ;; Refile
        org-refile-targets '(
                             ;; P.A.R.A
                             ("~/Documents/org/para/projects.org" :maxlevel . 10)
                             ("~/Documents/org/para/areas.org" :maxlevel . 10)
                             ("~/Documents/org/para/resources.org" :maxlevel . 10)
                             ("~/Documents/org/para/archives.org" :maxlevel . 10)))

  (defun ic/org-agenda-browse-at-point ()
    "Browse  the url of the specified item."
    (interactive)
    (let ((agenda-window-configuration (current-window-configuration)))
      (org-agenda-switch-to)
      (let ((url (car
                  (mapcar (lambda (p) (replace-regexp-in-string (regexp-quote "\"") "" (org-entry-get (point) p)))
                          (seq-filter (lambda (n) (string-suffix-p "url" n t))
                                      (mapcar (lambda (e) (car e)) (org-entry-properties)))))))
        (when url (browse-url  url)))
      (set-window-configuration agenda-window-configuration)))

  (defun ic/org-agenda-archive-at-point ()
    "Browse  the url of the specified item."
    (interactive)
    (let ((agenda-window-configuration (current-window-configuration)))
      (org-agenda-switch-to)
      (ic/org-archive)
      (set-window-configuration agenda-window-configuration)))

  (defun ic/org-agenda-export ()
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

  (defun ic/org-refile (file headline &optional new-state)
    "Refile item to the target FILE under the HEADLINE and set the NEW-STATE."
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (save-excursion
        (org-refile nil nil (list headline file nil pos))
        (org-refile-goto-last-stored)
        (when new-state (org-todo new-state)))))

  (defun ic/org-archive ()
    "Mark item as complete and refile to archieve."
    (interactive)
    (let ((archive-headline (or (org-entry-get (point) "archive-headline") "Unsorted")))
      (ic/org-refile "~/Documents/org/para/archives.org" archive-headline "DONE")))

  :hook (org-agenda-mode . org-super-agenda-mode)
  :bind (:map org-agenda-mode-map
              ("C-a" . ic/org-agenda-archive-at-point)
              ("C-b" . ic/org-agenda-browse-at-point)))

;;
;; Org roam
;;
(use-package org-roam
  :defer t
  :commands (org-roam-capture org-roam-node-find org-roam-graph)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)

  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  ;;
  ;; Using org roam for external apps, source: https://ag91.github.io/blog/2021/07/09/org-capture-in-nyxt-taking-notes-while-browsing
  ;;

  (defun ic/org-roam-slug-string (title)  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                                                                   768 ; U+0300 COMBINING GRAVE ACCENT
                                                                   769 ; U+0301 COMBINING ACUTE ACCENT
                                                                   770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                                                   771 ; U+0303 COMBINING TILDE
                                                                   772 ; U+0304 COMBINING MACRON
                                                                   774 ; U+0306 COMBINING BREVE
                                                                   775 ; U+0307 COMBINING DOT ABOVE
                                                                   776 ; U+0308 COMBINING DIAERESIS
                                                                   777 ; U+0309 COMBINING HOOK ABOVE
                                                                   778 ; U+030A COMBINING RING ABOVE
                                                                   780 ; U+030C COMBINING CARON
                                                                   795 ; U+031B COMBINING HORN
                                                                   803 ; U+0323 COMBINING DOT BELOW
                                                                   804 ; U+0324 COMBINING DIAERESIS BELOW
                                                                   805 ; U+0325 COMBINING RING BELOW
                                                                   807 ; U+0327 COMBINING CEDILLA
                                                                   813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                                                   814 ; U+032E COMBINING BREVE BELOW
                                                                   816 ; U+0330 COMBINING TILDE BELOW
                                                                   817 ; U+0331 COMBINING MACRON BELOW
                                                                   )))
                                            (cl-flet* ((nonspacing-mark-p (char)
                                                                          (memq char slug-trim-chars))
                                                       (strip-nonspacing-marks (s)
                                                                               (ucs-normalize-NFC-string
                                                                                (apply #'string (seq-remove #'nonspacing-mark-p
                                                                                                            (ucs-normalize-NFD-string s)))))
                                                       (cl-replace (title pair)
                                                                   (replace-regexp-in-string (car pair) (cdr pair) title)))
                                              (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                                                              ("__*" . "_") ;; remove sequential underscores
                                                              ("^_" . "") ;; remove starting underscore
                                                              ("_$" . ""))) ;; remove ending underscore
                                                     (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
                                                (downcase slug)))))

  (defun ic/org-roam-make-filepath (title now &optional zone)
    "Make filename from note TITLE and NOW time (assumed in the current time ZONE)."
    (concat
     (file-name-as-directory org-roam-directory)
     (format-time-string "%Y%m%d%H%M%S_" now (or zone (current-time-zone)))
     (s-truncate 70 (ic/org-roam-slug-string title) "")
     ".org"))

  (defun ic/org-roam-insert-file (file-path title &optional url)
    "Insert org roam file in FILE-PATH with TITLE, URL."
    (with-temp-file file-path
      (insert
       "* " title "\n"
       "\n" url "\n"))
    (with-current-buffer (find-file-noselect file-path) 
      (org-id-get-create)
      (org-entry-put (point) "URL" url)
      (save-buffer)))

  (defmacro ic/org-roam-create-by-tag-functions (tag)
    "Create functions that can be used to create/search org-roam-nodes by TAG."
    (declare (indent 1) (debug t))
    `(progn
       (defun ,(intern (format "ic/org-roam-%s-node-p" tag)) (node)
         ,(format "Returns non-nill if the specified NODE is tagged with %s." tag)
         (let ((tags (org-roam-node-tags node)))
           (member ,(format "%s" tag) tags)))

       (defun ,(intern (format "ic/org-roam-node-find-%s" tag)) (&optional other-window initial-input &key templates)
         ,(format "Find or create nodes tagged with %s." tag)
         (interactive)
         (let ((node (org-roam-node-read initial-input ',(intern (format "ic/org-roam-%s-node-p" tag)))))
           (if (org-roam-node-file node)
               (org-roam-node-visit node other-window)
             (progn
               (org-roam-capture-
                :node node
                :templates templates
                :props '(:finalize find-file))
               (insert ,(format "#+filetags: %s" tag))))))))

  (ic/org-roam-create-by-tag-functions "bjj")
  (ic/org-roam-create-by-tag-functions "emacs"))

;; Google Calendar
(use-package org-gcal
  :defer t
  :after org
  :commands (org-gcal-sync org-gcal-fetch)
  :config
  (setq org-gcal-client-id (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/client-id"))
        org-gcal-client-secret (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/secret"))
        org-gcal-file-alist '(("iocanel@gmail.com" .  "~/Documents/org/calendars/personal.org")
                              ("ikanello@redhat.com" . "~/Documents/org/calendars/work.org"))))


;; Org Table Aggregate
(use-package orgtbl-aggregate
  :after org
  :defer t)

;; Deft
(use-package deft
  :after org
  :defer t
  :commands (deft)
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/notes")
  (deft-use-filename-as-title t))

(use-package org-bullets
  :after org
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;;
;; Org-present
;;

(defvar ic/fringe-mode fringe-mode)
(defvar ic/org-present-modeline-enabled nil "Flag to specify if modeline should be completly turned off when presenting.")
(defvar ic/org-present-mode-line-format "" "The modeline format to use when presenting.")
(defvar ic/org-present-original-mode-line-format nil "The original modeline to use when exisitng presentation.")

;;;###autoload
(defun ic/next-code-block ()
  "Jump to the next code block."
  (interactive)
  (re-search-forward "^[[:space:]]*\\(#\\+begin_src\\)" nil t))

;;;###autoload
(defun ic/previous-code-block ()
  "Jump to the next code block."
  (interactive)
  (re-search-backward "^[[:space:]]*\\(#\\+end_src\\)" nil t)
  (re-search-backward "^[[:space:]]*\\(#\\+begin_src\\)" nil t))

;;;###autoload
(defun ic/code-block-p ()
  "Return non-nil if in code block."
  (let* ((previous-end-pos (save-excursion
                             (progn (re-search-backward "^[[:space:]]*\\(#\\+end_src\\)" nil t)
                                    (point))))
         (previous-begin-pos (save-excursion (progn (re-search-backward "^[[:space:]]*\\(#\\+begin_src\\)" nil t)
                                                      (point)))))
    (> previous-begin-pos previous-end-pos)))

;;;###autoload
(defun ic/ensure-in-code-block ()
    "Jump to the next code block if not current not in code block."
    (interactive)
    (when (not (ic/code-block-p))
      (ic/next-code-block)))

;;;###autoload
(defun ic/org-edit-special ()
  "Edit special or create src block and edit special."
    (interactive)
    (cond
     ((bound-and-true-p org-src-mode) (org-edit-src-exit))
     ((ic/code-block-p) (org-edit-special))
     (:default 
      (progn
        (org-insert-structure-template "src")
        (insert (completing-read "Select language:" '("sh" "java" "javascript" "rust" "python" "clojre" "yaml", "json") nil nil))
        (org-edit-special)))))


(use-package org-present
  :defer t
  :commands (ic/org-present-start ic/org-present-stop)
  :config
  (setq org-present-text-scale 3
        org-present-run-after-navigate-functions  '(org-display-inline-images))

  (defun ic/org-present-start ()
    "Setup screen for org present."
    (interactive)
    (org-bullets-mode 1)
    (org-present-big)
    (org-present-hide-cursor)
    (if ic/org-present-modeline-enabled
        (progn
          (setq-local ic/org-present-original-mode-line-format mode-line-format)
          (setq-local mode-line-format ic/org-present-mode-line-format))
      (turn-on-hide-mode-line-mode))
    (presentation-mode)
    ;; Center the text
    (set-fringe-mode (/ (- (frame-pixel-width) (* 10 (frame-char-width))) 20 ))
    (org-present-read-only))

  (defun ic/org-present-stop ()
    "Exit org-present."
    (interactive)
    (org-present-small)
    (org-present-show-cursor)
    (if ic/org-present-modeline-enabled
        (setq-local mode-line-format ic/org-present-original-mode-line-format)
      (turn-off-hide-mode-line-mode))
    (org-present-read-write)
    (org-present-quit)
    (funcall ic/selected-screen-mode)
    (set-fringe-mode ic/fringe-mode))

  :bind (("C-c a p" . org-present)
         ("M-n" . ic/next-code-block)
         ("M-p" . ic/previous-code-block)
         :map org-present-mode-keymap
         ("C-q" . ic/org-present-stop)
         ("<right>" . org-present-next)
         ("<left>" . org-present-prev))
  :hook ((org-present-mode . ic/org-present-start)
         (org-present-mode-quit . ic/org-present-stop)))

(use-package org-tree-slide
  :defer t
  :commands (org-tree-slide-mode)
  :bind
  ((:map org-tree-slide-mode-map
         ("q" . org-tree-slide-mode)
         ("C-i" . org-display-inline-images)
         ("<right>" . org-tree-slide-move-next-tree)
         ("<left>" . org-tree-slide-move-previous-tree))))

(add-hook 'org-tree-slide-before-content-view-hook #'org-display-inline-images)

(use-package hide-mode-line :defer t :commands (hide-mode-line-mode))
(use-package org-ql :defer t :commands (org-ql-search))

;;
;; Org Functions
;;
(defun ic/org-heading (heading tags)
  "Format the HEADING and the TAGS in the desired way."
  (format "%-80s %s" heading tags))

(defun ic/org-trim-tags (h)
  "Removes all tags that are present in H."
  (if h (string-trim  (replace-regexp-in-string ":[a-zA-Z0-9_\\.-:]+:$" "" h)) nil))

(defun ic/org-get-entries (tag &optional f)
  (interactive)
  "Collects all headings that contain TAG from the current buffer or from file F."
  (if f (mapcar #'ic/org-trim-tags (org-map-entries #'org-get-heading tag 'file))
    (mapcar #'ic/org-trim-tags (org-map-entries #'org-get-heading tag 'agenda))))

(defun ic/org-get-property (file name tag property)
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
  :custom
  (org2blog/wp-shortcode-langs-map (list
                                    '("emacs-lisp" . "lisp")
                                    '("sh" . "bash")))
  :config
  (setq org2blog/wp-use-sourcecode-shortcode t
        org2blog/wp-image-upload t
        org2blog/wp-blog-alist
        `(("iocanel.com"
           :url "https://iocanel.com/xmlrpc.php"
           :username "iocanel@gmail.com"
           :password ,(replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show websites/iocanel.com/iocanel@gmail.com")))))
  :commands (org2blog-user-interface
             org2blog-buffer-new
             org2blog-buffer-post-publish
             org2blog-buffer-page-publish
             org2blog-user-login
             org2blog-user-logout
             org2blog/wp-show-post-in-browser))

;;
;; Github Issues
;;

(use-package org-github-issues :straight (org-github-issues :host github :repo "iensu/org-github-issues")
  :defer t
  :init
  (defvar ic/github-repositories nil "The repositories that are watched by org-github-issues")
  :commands (ic/github-issues-sync-all ic/org-github-issues-eww-at-point ic/org-github-issues--show-open-workspace-issues)
  :config
  (setq
   ic/github-repositories '("sundrio/sundrio" "fabric8io/kubernetes-client" "dekorateio/dekorate" "quarkusio/quarkus" "snowdrop-bot/snowdrop-bot" "spring-cloud/spring-cloud-kubernetes")
   gh-user "iocanel"
   org-github-issues-org-file "~/Documents/org/gh-issues.org"
   org-github-issues-tags '("github" "issue")
   org-github-issues-tag-transformations '((".*" "")) ;; force all labels to empty string so that they can be ommitted.
   org-github-issues-auto-schedule "+0d"
   org-github-issues-filter-by-assignee t
   org-github-issues-headline-prefix t)

  (defun ic/org-github-issues-eww-at-point ()
    "Browse the issue that corresponds to the org entry at point."
    (interactive)
    (let ((url (ic/org-github-issues--url-at-point)))
      (when url 
        (other-window 1)
        ;(idee/jump-to-non-ide-window)
        (ic/split-and-follow-horizontally)
        (eww url))))

  (defun ic/org-github-issues-sync-all  ()
    "Sync all github repository issues."
    (interactive)
    (mapcar (lambda (r) (org-github-issues-sync-issues r)) ic/github-repositories))

  (defun ic/org-github-issues--show-open-project-issues (root)
    "Show all the project issues currently assigned to me."
    (let* ((project (projectile-ensure-project root))
           (project-name (projectile-project-name project)))
      (org-ql-search "~/Documents/org/github.org"
                     `(and (property "GH_URL")
                           (string-match (regexp-quote ,project-name) (org-entry-get (point) "GH_URL")))
                     :title (format "Github issues for %s" project-name))
      (goto-char (point-min))
      (org-agenda-next-line)))

  (defun ic/org-github-issues--show-open-workspace-issues (workspace)
    "Show all the workspace issues currently assigned to me."
    (let* ((name (treemacs-project->name workspace))
           (projects (treemacs-workspace->projects workspace))
           (project-names (mapcar (lambda (p) (treemacs-project->name p)) projects))
           (main-project (car project-names)))
      (when main-project 
        (org-ql-search "~/Documents/org/github.org"
                       `(and (property "GH_URL")
                             (or (string-match (regexp-quote ,main-project) (org-entry-get (point) "GH_URL"))
                                 (seq-filter (lambda (p) (string-match (regexp-quote p) (org-entry-get (point) "GH_URL"))) project-names)))
                       :title (format "Github issues for %s" name))
        (goto-char (point-min))
        (org-agenda-next-line))))

  (defun ic/org-github-issues--url-at-point ()
    (save-excursion
      (let ((origin (current-buffer)))
        (when (eq major-mode 'org-agenda-mode) (org-agenda-switch-to))
        (let* ((p (point))
               (url (string-trim (org-entry-get nil "GH_URL"))))
          (when (not (equal origin (current-buffer))) (switch-to-buffer origin))
          url)))))

;;
;; Org sync
;;
(use-package org-sync :straight (org-sync :host github :repo "arbox/org-sync" :files ("org-sync.el" "org-sync-github.el"))
  :defer t
  :commands (org-sync ic/org-sync ic/org-sync-full)
  :custom
  (org-sync-github-include-closed nil)
  (org-sync-github-include-pr t)
  (org-sync-github-include-issues t)
  (org-sync-github-create-enabled nil)
  (org-sync-github-update-enabled nil)
  :config
  (setq org-sync-github-password (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/github.com/iocanel/token")))
  (setq org-sync-github-auth `("iocanel" . ,org-sync-github-password))
  (setq org-sync-github-pulls-auth `("iocanel" . ,org-sync-github-password))

(defun ic/org-sync()
  "Sync the github repos file."
  (interactive)
  (require 'org-sync-github)
  (let ((github-tasks-file "~/Documents/org/github.org"))
    (with-temp-buffer
      (insert-file github-tasks-file)
      (org-sync)
      (write-file github-tasks-file))))

(defun ic/org-sync-postprocess ()
  "Postprocess the org-sync github repos file."
  (interactive)
  (require 'org-sync-github)
  (let ((scheduled (format "%s  SCHEDULED: <%s>\n" (make-string 2 32) (org-read-date nil nil "+0d") ))
        (github-tasks-file "~/Documents/org/github.org"))
    (with-temp-buffer
      (insert-file github-tasks-file)
      (goto-char (point-min))
      (while (re-search-forward "^\*\* OPEN" nil t)
        (org-set-property "SCHEDULED" scheduled)
        (let* ((url (string-trim (org-entry-get nil "url")))
               (is-issue (string-match-p (regexp-quote "/issues/") url))
               (is-pr (string-match-p (regexp-quote "/pull/") url))
               (tags ()))
          (add-to-list 'tags "github")
          (when is-issue (add-to-list 'tags "issue"))
          (when is-pr (add-to-list 'tags "pull"))
          (org-set-tags tags)
          (org-set-property "archive-headline" "Github issues")
          (replace-regexp-in-region "\\(OPEN \\)+"  "OPEN " (point-min) (point-max))
          (write-file github-tasks-file))))))

(defun ic/org-sync-full ()
  "Full org-sync with post processing."
  (interactive)
  (ic/org-sync)
  (ic/org-sync-postprocess)))

;;
;; Literate capture configuration
;;
(defvar org-capture-babel-list '("~/Documents/org/nutrition.org" "~/Documents/org/weight.org") "List of literate capture org files.")

(defun ic/load-capture-babel-async (orig-fun &rest args)
  "Load org files right before calling org capture."
  (mapc 'org-babel-load-file org-capture-babel-list)
  (setq org-capture-babel-list '()))

(advice-add 'org-capture :before #'ic/load-capture-babel-async)

(use-package ob-typescript)
;;
;; Org Babel
;;
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((shell .t)
                                                           (ruby . t)
                                                           (java . t)
                                                           (typescript . t)
                                                           (plantuml . t))))
(use-package org-babel-eval-in-repl
  :custom (eir-shell-type 'vterm)
  :bind (:map org-mode-map
              ("C-<return>" . ober-eval-block-in-repl)))

(advice-add 'ober-eval-block-in-repl :before #'ic/ensure-in-code-block)
(advice-add 'ober-eval-block-in-repl :after #'ic/next-code-block)

(defun ic/not-empty (s)
  "Returns non-nil if S is not empty."
  (and s (stringp s) (not (= (length s) 0))))

;; Let's intercept eir-insert to make sure the text entered is trimmed.
(defun ic/eir-insert-trimmed (orig string)
  "Eir insert but with trimmed arguments."
  (let ((trimmed (replace-regexp-in-string "^[ \t\n]+" "" (replace-regexp-in-string "[ \n]+$" "" string))))
    (when (ic/not-empty trimmed)
      (apply orig (list trimmed)))))

(defun ic/eir-send-not-empty-to-repl (orig fun-change-to-repl fun-execute region-string)
  "Eir send to repl but ignore empty commands."
  (when (ic/not-empty region-string)
    (apply orig (list fun-change-to-repl fun-execute region-string))))

(advice-add 'eir-insert :around #'ic/eir-insert-trimmed)
(advice-add 'eir-send-to-repl :around #'ic/eir-send-not-empty-to-repl)

;;
;; To allow yas snippet integration with org babel and avoid org-mode shadowing the block mode (when it comes to snippets)
;;

(defun ic/yas-org-babel-integration-hook ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(defvar ic/last-tangle-source-buffer nil)
(defvar ic/last-tangle-source-buffer-point 0)

(defun ic/org-tangle-prepare ()
  (setq ic/last-tangle-source-buffer (current-buffer))
  (setq ic/last-tangle-source-buffer-point (point))
        (get-buffer-create "**tangle**")
  (copy-to-buffer "**tangle**" (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "//add:\\([a-zA-Z0-9_-]+\\)" nil t)
    (let* ((text (buffer-substring (match-beginning 1) (match-end 1)))
           (new-text (format "<<%s>>" text)))
      (replace-match new-text))))


(defun ic/org-tangle-restore ()
    (with-current-buffer "**tangle**"
      (copy-to-buffer ic/last-tangle-source-buffer (point-min) (point-max)))
    (with-current-buffer ic/last-tangle-source-buffer
      (goto-char ic/last-tangle-source-buffer-point))) 
    
(add-hook 'org-mode-hook #'ic/yas-org-babel-integration-hook)
(add-hook 'org-babel-pre-tangle-hook #'ic/org-tangle-prepare)
(add-hook 'org-babel-post-tangle-hook #'ic/org-tangle-restore)

(defun ic/org-export-use-docx ()
  "Set the odt prferred output to docx." 
  (interactive)
  (setq org-odt-preferred-output-format "docx"))

;;
;; Quickmarks
;;
(use-package quickmarks :straight (quickmarks :host github :repo "iocanel/quickmarks.el")
  :defer t
  :commands (qm-init qm-avatar-by-name qm-logo-by-name qm-url-by-name)
  :config
  (qm-init))

;;
;; Imgflip
;;

(use-package imgflip :straight (imgflip :host github :repo "iocanel/imgflip.el")
  :defer t
  :custom (imgflip-download-dir "~/.imgflip/")
  :config
  (setq imgflip-username "iocanel"
        imgflip-password (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/imgflip/iocanel/password"))))
