(defvar elfeed-external-mode-map (make-sparse-keymap))
(define-minor-mode elfeed-external-mode "A minor mode to add external modes `showing` elfeed entry content"
  (use-local-map elfeed-external-mode-map))

(use-package elfeed
  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs) 
  (evil-set-initial-state 'elfeed-show-mode 'emacs) 
  :bind (:map elfeed-search-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("e" . iocanel/elfeed-open-in-eww)
              ("b" . iocanel/elfeed-open-in-bongo)
              ("y" . iocanel/elfeed-open-in-youtube)
              ("d" . iocanel/elfeed-open-in-dwim)
              ("C-<tab>" . iocanel/elfeed-external-next-entry)
              ("E" . iocanel/elfeed-enqueue-media-url)
              ("Q" . iocanel/elfeed-save-and-quit)
              ("r" . iocanel/elfeed-mark-as-read)
              ("R" . iocanel/elfeed-mark-all-as-read)
           :map elfeed-external-mode-map
           ("C-<tab>" . iocanel/elfeed-external-next-entry)))

(use-package elfeed-org
  :custom (rmh-elfeed-org-files '("~/Documents/org/roam/blogs.org"))
  :config (elfeed-org))

;;;###autoload
(defun iocanel/elfeed-external-buffer-p (buf)
  "Returns non-nil if BUF has enabled the elfeed-external-mode."
  (with-current-buffer buf
    (and (boundp 'elfeed-external-mode) elfeed-external-mode)))

;;;###autoload
(defun iocanel/elfeed-external-buffer-list ()
  "List all buffers that have teh elfeed-external-mode enabled."
  (seq-filter (lambda (b) (iocanel/elfeed-external-buffer-p b)) (buffer-list)))

;;;###autoload
(defun iocanel/elfeed-current-buffer-external ()
  (interactive)
  "Returns non-nil if BUF has enabled the elfeed-external-mode."
    (let ((result (and (boundp 'elfeed-external-mode) elfeed-external-mode)))
      (if result (message "Current buffer is in elfeed-external-mode.")
        (message "Current buffer is NOT in elfeed-external-mode."))
        result))

;;;###autoload
(defun iocanel/elfeed-delete-external-windows ()
  (interactive)
  "List all buffers that have teh elfeed-external-mode enabled."
  (mapcar (lambda (b) (delete-window (get-buffer-window b))) (iocanel/elfeed-external-buffer-list)))

;;;###autoload
(defun iocanel/elfeed-jump-to-search(&optional visited)
  (interactive)
  "Jump to a elfeed-search window. VISITED is an optional list with windows already visited."
  (let* ((visited (or visited '()))
         (buffer (current-buffer))
         (name (buffer-name buffer))
         (window (frame-selected-window))
         (found (equal "*elfeed-search*" name))
         (current-buffer-selected (equal (get-buffer-window buffer) window)))

    (cond
     ((and found current-buffer-selected) t)
     ((member name visited) nil)
     (t (progn (other-window 1)
               (iocanel/elfeed-jump-to-search (add-to-list 'visited name))))))) 

;;;###autoload
(defun iocanel/elfeed-external-next-entry (&optional visited)
  (interactive)
  "Closes external elfeed windows and moves to next entry."
  (condition-case nil
      (iocanel/elfeed-delete-external-windows)
    (error nil))
  (iocanel/elfeed-jump-to-search)
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current))
  (next-line)
  (iocanel/elfeed-open-in-dwim (elfeed-search-selected :ignore-region)))
                              
;;; credits: https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
;;;###autoload
(defun iocanel/mark-line (&optional arg)
  "Select the current line and move the cursor by ARG lines IF no region is selected.
If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (let ((lines (or arg 1)))
    (when (not (use-region-p))
      (forward-line 0)
      (set-mark-command nil))
    (forward-line lines)))

;;;###autoload
(defun iocanel/elfeed-mark-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (iocanel/mark-line 0)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun iocanel/elfeed-mark-all-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun iocanel/elfeed-save-and-quit ()
  "Wrapper to save the elfeed db to disk before quiting"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;;
;; Window placement and behavior
;;
(defadvice elfeed-search-show-entry (around elfeed-search-show-entry-around activate)
  "Open entries in a new buffer below."
  (split-and-follow-vertically)
  ad-do-it)

(defadvice elfeed-kill-buffer (around elfeed-kill-buffer-around activate)
  "Open entries in a new buffer below."
  (message "Is external mode:%s" (iocanel/elfeed-current-buffer-external))
  (if (or (derived-mode-p 'elfeed-show-mode) (iocanel/elfeed-current-buffer-external))
      (progn
        ad-do-it
        (delete-window))
    ad-do-it))

;;
;; Media
;;

;;;###autoload
(defun entry-mp3-p (entry)
  "Predicate that checks if ENTRY points to mp3."
    (let ((link (elfeed-entry-link entry))
          (enclosure (car (elt (elfeed-entry-enclosures entry) 0))))
    (or (url-mp3-p link) (url-mp3-p enclosure))))

;;;###autoload
(defun url-mp3-p (url)
  "Predicate that checks if URL points to mp3."
  (if (stringp url) (string-match-p "\\.mp3(?.*)*$" url) nil))

;;;###autoload
(defun normalize-mp3-url (url)
  "Predicate that checks if url points to mp3."
  (let ((index (string-match-p ".mp3?" url)))
    (if index (concat (substring url 0 index) ".mp3")
      url)))


;;
;; Youtube
;;
(defconst youtube-watch-url-prefix "https://www.youtube.com/watch?v=" "The prefix to the youtube urls")
(defvar youtube-downlod-path "/home/iocanel/Downloads/Youtube/" "The prefix to the youtube urls")

;;;###autoload
(defun entry-youtube-p (entry)
  "Predicate that checks if ENTRY points to youtbue."
    (let ((link (elfeed-entry-link entry))
          (enclosure (car (elt (elfeed-entry-enclosures entry) 0))))
    (or (url-youtbue-p link) (url-youtbue-p enclosure))))

;;;###autoload
(defun url-youtbue-p (url)
  "Predicate that checks if URL points to youtube."
  (if (stringp url) (string-prefix-p youtube-watch-url-prefix url) nil))

(defun youtube-get (url)
  "Download the youtube video from URL to a temporary file and return the path to it."
  (let* ((video-id (substring url (length youtube-watch-url-prefix) (length url)))
         (path (concat youtube-downlod-path video-id ".avi")))
    (shell-command (format "youtube-dl \"%s\" -o %s" url path))
    path))
  
;;;###autoload
(defun iocanel/elfeed-enqueue-media-url (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry))
          (enclosure (car (elt (elfeed-entry-enclosures entry) 0))))
      (cond
       ((url-mp3-p link) (iocanel/bongo-enqueue-file (normalize-mp3-url link)))
       ((url-mp3-p enclosure) (iocanel/bongo-enqueue-file (normalize-mp3-url enclosure)))))))

;;;###autoload
(defun iocanel/elfeed-open-in-dwim (entry)
  "Open feed in the most fitting mode."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (cond
   ((entry-mp3-p entry) (iocanel/elfeed-open-in-bongo entry))
   ((entry-youtube-p entry) (iocanel/elfeed-open-in-youtube entry))
   (t (iocanel/elfeed-open-in-eww entry))))

;;;###autoload
(defun iocanel/elfeed-open-in-youtube(entry)
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let* ((link (elfeed-entry-link entry))
           (download-path (youtube-get link)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (iocanel/bongo-enqueue-file (concat "file://" download-path)))))

;;;###autoload
(defun iocanel/elfeed-open-in-bongo (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (bongo-library)
      (iocanel/elfeed-enqueue-media-url entry)
      (elfeed-external-mode))))

;;;###autoload
(defun iocanel/elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (eww link)
      (elfeed-external-mode))))
