(defvar elfeed-external-mode-map (make-sparse-keymap))
(defvar elfeed-youtube-dl-enabled t "Download videos using youtube-dl and play them as loca files, instead of streaming them")
(define-minor-mode elfeed-external-mode "A minor mode to add external modes `showing` elfeed entry content" (use-local-map elfeed-external-mode-map))

(use-package elfeed
  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs) 
  (evil-set-initial-state 'elfeed-show-mode 'emacs) 
  :bind (:map elfeed-search-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("b" . iocanel/elfeed-open-in-bongo)
              ("e" . iocanel/elfeed-open-in-eww)
              ("x" . iocanel/elfeed-open-in-xwidget-webkit-browser)
              ("y" . iocanel/elfeed-open-in-youtube)
              ("d" . iocanel/elfeed-open-in-dwim)
              ("C-<tab>" . iocanel/elfeed-external-next-entry)
              ("E" . iocanel/elfeed-enqueue-media-url)
              ("Q" . iocanel/elfeed-save-and-quit)
              ("r" . iocanel/elfeed-mark-as-read)
              ("R" . iocanel/elfeed-mark-all-as-read)
           :map elfeed-show-mode-map
              ("C-<tab>" . iocanel/elfeed-external-next-entry)
              ("b" . iocanel/elfeed-show-in-bongo)
              ("e" . iocanel/elfeed-show-in-eww)
              ("x" . iocanel/elfeed-show-in-xwidget-webkit-browser)
              ("y" . iocanel/elfeed-show-in-youtube)
              ("d" . iocanel/elfeed-show-dwim)
           :map elfeed-external-mode-map
           ("C-<tab>" . iocanel/elfeed-external-next-entry)))

(use-package elfeed-org
  :custom (rmh-elfeed-org-files '("~/Documents/org/roam/blogs.org"))
  :config (elfeed-org))

;;
;; External mode functions
;;

;;;###autoload
(defun iocanel/elfeed-external-buffer-p (buf)
  "Returns non-nil if BUF has enabled the elfeed-external-mode."
  (with-current-buffer buf
    (and (boundp 'elfeed-external-mode) elfeed-external-mode)))

;;;###autoload
(defun iocanel/elfeed-external-buffer-list ()
  "List all buffers that have the elfeed-external-mode enabled."
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
(defun iocanel/elfeed-show-buffer-list ()
  "List all buffers that have teh elfeed-show-mode enabled."
  (seq-filter (lambda (b) (iocanel/elfeed-show-buffer-p b)) (buffer-list)))

;;;###autoload
(defun iocanel/elfeed-delete-show-windows ()
  (interactive)
  "List all buffers that have the elfeed-show-mode enabled."
  (mapcar (lambda (b) (delete-window (get-buffer-window b))) (iocanel/elfeed-show-buffer-list)))

;;;###autoload
(defun iocanel/mark-current-as-read ()
  (interactive)
  "Mark current entry as read."
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current)
    (elfeed-db-save-safe)))

(defun iocanel/elfeed-show-buffer-p (buf)
  "Returns non-nil if BUF has enabled the elfeed-external-mode."
  (with-current-buffer buf
    (and (boundp 'elfeed-show-mode) elfeed-show-mode)))


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
(defun iocanel/elfeed-delete-non-search-windows ()
  (interactive)
  "Delete all elfeed non search buffers."
  ;; External
  (condition-case nil
      (iocanel/elfeed-delete-external-windows)
    (error nil))

  ;; Show
  (condition-case nil
      (iocanel/elfeed-delete-show-windows)
    (error nil)))

;;;###autoload
(defun iocanel/elfeed-external-next-entry (&optional visited)
  (interactive)
  "Closes external elfeed windows and moves to next entry."
  (iocanel/elfeed-delete-non-search-windows)
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
  (iocanel/mark-current-as-read)
  (iocanel/elfeed-delete-show-windows)
  (split-and-follow-vertically)
  ad-do-it)

(defadvice elfeed-kill-buffer (around elfeed-kill-buffer-around activate)
  "Open entries in a new buffer below."
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

(defun youtube-get (url &optional callback)
  "Download the youtube video from URL to a temporary file and return the path to it."
  (let* ((video-id (substring url (length youtube-watch-url-prefix) (length url)))
         (path (concat youtube-downlod-path video-id ".avi"))
         (output-buffer (generate-new-buffer (format elfeed-youtube-dl-buffer-format video-id)))
         (proc (progn
                 (message "Downloading video into: %s" path)
                 (async-shell-command (format "youtube-dl \"%s\" -o %s" url path) output-buffer)
                 (get-buffer-process output-buffer))))
         (when callback (set-process-sentinel  proc callback))
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
       ((url-mp3-p link) (iocanel/bongo-enqueue-file-and-play (normalize-mp3-url link)))
       ((url-mp3-p enclosure) (iocanel/bongo-enqueue-file-and-play (normalize-mp3-url enclosure)))))))

;;;###autoload
(defun iocanel/elfeed-open-in-dwim (entry)
  "Open feed in the most fitting mode."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (cond
   ((entry-mp3-p entry) (iocanel/elfeed-open-in-bongo entry))
   ((entry-youtube-p entry) (iocanel/elfeed-open-in-youtube entry))
   (t (iocanel/elfeed-open-in-eww entry))))

;;;###autoload
(defun iocanel/elfeed-show-dwim ()
  "Open feed in the most fitting mode."
  (interactive)
  (cond
   ((entry-mp3-p elfeed-show-entry) (iocanel/elfeed-open-in-bongo elfeed-show-entry))
   ((entry-youtube-p elfeed-show-entry) (iocanel/elfeed-open-in-youtube elfeed-show-entry))
   (t (iocanel/elfeed-open-in-eww elfeed-show-entry))))


(defvar elfeed-youtube-dl-buffer-format "*Async youtube-dl: %s*")
;; We need to make sure that the youtbue-dl buffer stays burried!
(add-to-list 'display-buffer-alist (cons "\\*Async youtube-dl: .*\\*" (cons #'display-buffer-no-window nil)))


;;;###autoload
(defun iocanel/elfeed-open-in-youtube(entry)
  "Display the currently selected item in youtube."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (iocanel/mark-current-as-read)
  (iocanel/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let* ((url (elfeed-entry-link entry))
           (video-id (substring url (length youtube-watch-url-prefix) (length url)))
           (path (concat youtube-downlod-path video-id ".avi"))
           (download-path (if elfeed-youtube-dl-enabled (youtube-get url #'iocanel/elfeed-start-bongo-callback) (iocanel/bongo-enqueue-file link))))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (iocanel/bongo-enqueue-file-and-play (concat "file://" download-path))
      (elfeed-external-mode 1))))


(defun iocanel/elfeed-start-bongo-callback (process signal)
"Callback to be called when a youtube video gets downloaded."
(when (memq (process-status process) '(exit signal))
  (message "Video finished!")
  (when (not (bongo-playing-p)) (bongo-start/stop)))
  (shell-command-sentinel process signal))

;;;###autoload
(defun iocanel/elfeed-show-in-youtube ()
  "Display the currently shown item in youtube."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (download-path (if elfeed-youtube-dl-enabled (youtube-get link) (iocanel/bongo-enqueue-file link))))
      (iocanel/bongo-enqueue-file-and-play (concat "file://" download-path))
      (elfeed-external-mode 1))))


;;;###autoload
(defun iocanel/elfeed-open-in-bongo (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (iocanel/mark-current-as-read)
  (iocanel/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (iocanel/elfeed-enqueue-media-url entry)
      (elfeed-external-mode 1))))

;;;###autoload
(defun iocanel/elfeed-show-in-bongo ()
  "Display the currently shown item in eww."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (iocanel/elfeed-enqueue-media-url elfeed-show-entry)
      (elfeed-external-mode 1))))

;;;###autoload
(defun iocanel/elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (iocanel/mark-current-as-read)
  (iocanel/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (eww link)
      (elfeed-external-mode))))

;;;###autoload
(defun iocanel/elfeed-show-in-eww ()
  "Display the currently shown item in xwidget-webkit-browser."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (eww link)
      (elfeed-external-mode))))

;;;###autoload
(defun iocanel/elfeed-open-in-xwidget-webkit-browser (entry)
  "Display the currently selected item in xwidget-webkit-browser."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (iocanel/mark-current-as-read)
  (iocanel/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (split-and-follow-vertically))
      (xwidget-webkit-browse-url link)
      (elfeed-external-mode))))

;;;###autoload
(defun iocanel/elfeed-show-in-xwidget-webkit-browser ()
  "Display the currently shown item in xwidget-webkit-browser."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (xwidget-webkit-browse-url link)
      (elfeed-external-mode))))

