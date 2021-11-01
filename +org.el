;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Org Mode
;;

(use-package gnuplot :defer t)

(use-package org
  :config
  (add-to-list 'org-modules 'org-habit t)
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

  ;;
  ;; Agenda
  ;;

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

  ;;
  ;; Refile
  ;;
  (setq org-refile-targets
        '(
          ("~/Documents/org/bjj/BJJ.org" :maxlevel . 10)

          ;; P.A.R.A
          ("~/Documents/org/para/projects.org" :maxlevel . 10)
          ("~/Documents/org/para/areas.org" :maxlevel . 10)
          ("~/Documents/org/para/resources.org" :maxlevel . 10)
          ("~/Documents/org/para/archives.org" :maxlevel . 10)))

  (setq org-capture-templates
        '(
          ("c" "Calendar")
          ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

          ("i" "Inbox")
          ("iw" "Work Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
          ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

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


;;; 
  )


;;;###autoload
(defun ic/org-drill ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'directory)
  (find-file "~/Documents/org/index.org")
  (org-drill)
  (org-save-all-org-buffers))

;;;###autoload
(defun ic/org-drill-buffer ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'file)
  (org-drill)
  (org-save-all-org-buffers))

(use-package org-drill
  :after org
  :defer t
  :init (setq org-drill-scope 'directory))

(use-package org-super-agenda
  :after org
  :defer t
  :config
  (setq org-super-agenda-groups '((:name "Events" :time-grid t :todo "TODAY")
                                  (:name "Habbits" :tag "habit" :todo "TODAY")
                                  (:name "Due" :deadline past)
                                  (:name "Github issues" :tag "github")))
  :hook (org-agenda-mode . org-super-agenda-mode))

;;
;; Org roam
;;
(use-package org-roam
  :after org
  :defer t
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
  :defer t
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
(defun ic/org-present-start ()
  (interactive)
  "Setup screen for org present."
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

;;;###autoload
(defun ic/org-present-stop ()
  (interactive)
  "Exit org-present."
  (org-present-small)
  (org-present-show-cursor)
  (if ic/org-present-modeline-enabled
      (setq-local mode-line-format ic/org-present-original-mode-line-format)
    (turn-off-hide-mode-line-mode))
  (org-present-read-write)
  (org-present-quit)
  (funcall ic/selected-screen-mode)
  (set-fringe-mode ic/fringe-mode))

;;;###autoload
(defun ic/next-code-block ()
  (interactive)
  "Jump to the next code block."
  (re-search-forward "^[[:space:]]*\\(#\\+begin_src\\)" nil t))

;;;###autoload
(defun ic/previous-code-block ()
  (interactive)
  "Jump to the next code block."
  (re-search-backward "^[[:space:]]*\\(#\\+begin_src\\)" nil t))

(use-package org-present
  :defer t
  :config
  (setq org-present-text-scale 3
        org-present-run-after-navigate-functions  '(org-display-inline-images))
  :bind (("C-c a p" . org-present)
         :map org-present-mode-keymap
         ("C-q" . org-present-quit)
         ("M-n" . ic/next-code-block)
         ("M-p" . ic/previous-code-block)
         ("<right>" . org-present-next)
         ("<left>" . org-present-prev))
  :hook ((org-present-mode . ic/org-present-start)
         (org-present-mode-quit . ic/org-present-stop)))

(use-package hide-mode-line :defer t)

(use-package org-ql :after org)
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
  :after org
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
(defvar ic/github-repositories nil "The repositories that are watched by org-github-issues")

(use-package org-github-issues :straight (org-github-issues :host github :repo "iensu/org-github-issues")
  :defer t
  :commands (org-github-issues-sync-issues)
  :config
  (setq
   ic/github-repositories '("sundrio/sundrio" "fabric8io/kubernetes-client" "dekorateio/dekorate" "quarkusio/quarkus" "snowdrop-bot/snowdrop-bot" "spring-cloud/spring-cloud-kubernetes")
   gh-user "iocanel"
   org-github-issues-org-file "~/Documents/org/github.org"
   org-github-issues-tags '("github" "triage")
   org-github-issues-auto-schedule "+0d"
   org-github-issues-filter-by-assignee t
   org-github-issues-headline-prefix t)
  (defun ic/github-issues-sync-all  ()
    "Sync all github repository issues."
    (interactive)
    (mapcar (lambda (r) (org-github-issues-sync-issues r)) ic/github-repositories)))

;;
;; Literate capture configuration
;;
(defvar org-capture-babel-list '("~/Documents/org/nutrition.org" "~/Documents/org/weight.org") "List of literate capture org files.")

(defun ic/load-capture-babel-async (orig-fun &rest args)
  "Load org files right before calling org capture."
  (mapc 'org-babel-load-file org-capture-babel-list)
  (setq org-capture-babel-list '()))

(advice-add 'org-capture :before #'ic/load-capture-babel-async)

;;
;; Org Babel
;;
(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((shell .t)
                                                           (ruby . t)
                                                           (java . t)
                                                           (plantuml . t))))
(use-package org-babel-eval-in-repl
  :custom (eir-shell-type 'vterm)
  :bind (:map org-mode-map
              ("C-<return>" . ober-eval-block-in-repl)))

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

(add-hook 'org-mode-hook #'ic/yas-org-babel-integration-hook)

;;
;; Quickmarks
;;
(use-package quickmarks :straight (quickmarks :host github :repo "ic/quickmarks.el")
  :after org
  :defer t
  :config
  (qm-init))

;;
;; Imgflip
;;

(use-package imgflip :straight (imgflip :host github :repo "ic/imgflip.el")
  :after org
  :defer t
  :custom (imgflip-download-dir "~/.imgflip")
  :config
  (setq imgflip-username "iocanel"
        imgflip-password (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/imgflip/ic/password"))))

