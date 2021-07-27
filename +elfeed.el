(defvar ic/elfeed-external-mode-map (make-sparse-keymap))
(define-minor-mode ic/elfeed-external-mode "A minor mode to add external modes `showing` elfeed entry content" (use-local-map ic/elfeed-external-mode-map))

(use-package elfeed
  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs) 
  (evil-set-initial-state 'elfeed-show-mode 'emacs) 
  :bind (:map elfeed-search-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("b" . ic/elfeed-open-in-bongo)
              ("e" . ic/elfeed-open-in-eww)
              ("x" . ic/elfeed-open-in-xwidget-webkit-browser)
              ("y" . ic/elfeed-open-in-youtube)
              ("d" . ic/elfeed-open-in-dwim)
              ("C-<tab>" . ic/elfeed-external-next-entry)
              ("E" . ic/elfeed-enqueue-media-url)
              ("Q" . ic/elfeed-save-and-quit)
              ("r" . ic/elfeed-mark-as-read)
              ("R" . ic/elfeed-mark-all-as-read)
           :map elfeed-show-mode-map
              ("C-<tab>" . ic/elfeed-external-next-entry)
              ("b" . ic/elfeed-show-in-bongo)
              ("e" . ic/elfeed-show-in-eww)
              ("x" . ic/elfeed-show-in-xwidget-webkit-browser)
              ("y" . ic/elfeed-show-in-youtube)
              ("d" . ic/elfeed-show-dwim)
           :map ic/elfeed-external-mode-map
           ("C-<tab>" . ic/elfeed-external-next-entry)))

(use-package elfeed-org
  :custom (rmh-elfeed-org-files '("~/Documents/org/blogs.org"))
  :config (elfeed-org))

;;
;; External mode functions
;;

;;;###autoload
(defun ic/elfeed-external-buffer-p (buf)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (with-current-buffer buf
    (and (boundp 'ic/elfeed-external-mode) ic/elfeed-external-mode)))

;;;###autoload
(defun ic/elfeed-external-buffer-list ()
  "List all buffers that have the elfeed-external-mode enabled."
  (seq-filter (lambda (b) (and (not (eq (elfeed-search-buffer) b)) (ic/elfeed-external-buffer-p b))) (buffer-list)))

;;;###autoload
(defun ic/elfeed-current-buffer-external ()
  (interactive)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (let ((result (and (boundp 'ic/elfeed-external-mode) ic/elfeed-external-mode))
        (name (buffer-name (current-buffer))))
        result))

;;;###autoload
(defun ic/elfeed-delete-non-search-windows ()
  (interactive)
  "Delete all elfeed non search buffers."
  ;; External
  (condition-case nil
      (ic/elfeed-delete-external-windows)
    (error nil))

  ;; Show
  (condition-case nil
      (ic/elfeed-delete-show-windows)
    (error nil)))

;;;###autoload
(defun ic/elfeed-delete-external-windows ()
  (mapcar (lambda (w) (when w (delete-window w))) (mapcar (lambda (b) (get-buffer-window b 'visible)) (ic/elfeed-external-buffer-list))))

;;;###autoload
(defun ic/elfeed-show-buffer-list ()
  "List all buffers that have teh elfeed-show-mode enabled."
  (seq-filter (lambda (b) (ic/elfeed-show-buffer-p b)) (buffer-list)))

;;;###autoload
(defun ic/elfeed-delete-show-windows ()
  (interactive)
  "List all buffers that have the elfeed-show-mode enabled."
  (mapcar (lambda (w) (when w (delete-window w))) (mapcar (lambda (b) (get-buffer-window b 'visible)) (ic/elfeed-show-buffer-list))))

;;;###autoload
(defun ic/mark-current-as-read ()
  (interactive)
  "Mark current entry as read."
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current)
    (elfeed-db-save-safe)))

(defun ic/elfeed-show-buffer-p (buf)
  "Returns non-nil if BUF has enabled the ic/elfeed-external-mode."
  (with-current-buffer buf (equal major-mode 'elfeed-show-mode)))


;;;###autoload
(defun ic/elfeed-jump-to-search(&optional visited)
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
               (ic/elfeed-jump-to-search (add-to-list 'visited name))))))) 

;;;###autoload
(defun ic/elfeed-external-next-entry (&optional visited)
  (interactive)
  "Closes external elfeed windows and moves to next entry."
  (ic/elfeed-delete-non-search-windows)
  (ic/elfeed-jump-to-search)
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current))
  (next-line)
  (ic/elfeed-open-in-dwim (elfeed-search-selected :ignore-region)))
                              
;;; credits: https://emacs.stackexchange.com/questions/15033/how-to-mark-current-line-and-move-cursor-to-next-line
;;;###autoload
(defun ic/mark-line (&optional arg)
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
(defun ic/elfeed-mark-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (ic/mark-line 0)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun ic/elfeed-mark-all-as-read ()
  "Mark all items in the elfeed buffer as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun ic/elfeed-save-and-quit ()
  "Wrapper to save the elfeed db to disk before quiting"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;;
;; Window placement and behavior
;;
(defadvice elfeed-search-show-entry (around elfeed-search-show-entry-around activate)
  "Open entries in a new buffer below."
  (ic/mark-current-as-read)
  (ic/elfeed-delete-show-windows)
  (ic/split-and-follow-vertically)
  ad-do-it)

(defun ic/elfeed-kill-external-buffer-and-window (&optional buffer-or-name)
  "Kill the github issues window and buffer.  Return t if grep window was found."
  (if (or (derived-mode-p 'elfeed-show-mode) (ic/elfeed-current-buffer-external))
        (progn
          (kill-buffer-and-window)
          t)
        nil))


(add-to-list 'idee-kill-current-buffer-listener-list #'ic/elfeed-kill-external-buffer-and-window)
(add-to-list 'idee-quit-window-listener-list #'ic/elfeed-kill-external-buffer-and-window)

;; elfeed-kill-buffer should use (kill-current-buffer) vs (kill-buffer (current-buffer))  to trigger listeners
;; Let's use an advice to fix that.
(advice-add #'elfeed-kill-buffer :around #'kill-current-buffer)

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

(defun ic/elfeed-youtube-pre-download ()
  "Update the elfeed-search buffer based on the contents of the minibuffer."
  (interactive)
  (message "Predownloading youtube videos")
  (with-temp-buffer
    (let ((elfeed-search-filter "+youtube +unread -downloaded"))
      (elfeed-search--update-list)
      (dolist (entry elfeed-search-entries)
        (ic/elfeed-download-from-youtube entry)))))

(defun ic/elfeed-download-from-youtube (entry)
  "Download the currently selected item from youtube."
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let* ((url (elfeed-entry-link entry)))
      (message "Prefectching form youtube: %s" url)
      (ignore-errors
        (ic/youtube-download url))
      (elfeed-tag entry 'downloaded))))

;;;###autoload
(defun ic/elfeed-entry-youtube-p (entry)
  "Predicate that checks if ENTRY points to youtube."
    (let ((link (elfeed-entry-link entry))
          (enclosure (car (elt (elfeed-entry-enclosures entry) 0))))
    (or (ic/youtube-url-p link) (ic/youtube-url-p enclosure))))


;;;###autoload
(defun ic/elfeed-enqueue-media-url (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry))
          (enclosure (car (elt (elfeed-entry-enclosures entry) 0))))
      (cond
       ((url-mp3-p link) (ic/bongo-enqueue-file-and-play (normalize-mp3-url link)))
       ((url-mp3-p enclosure) (ic/bongo-enqueue-file-and-play (normalize-mp3-url enclosure)))))))

;;;###autoload
(defun ic/elfeed-open-in-dwim (entry)
  "Open feed in the most fitting mode."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (cond
   ((entry-mp3-p entry) (ic/elfeed-open-in-bongo entry))
   ((ic/elfeed-entry-youtube-p entry) (ic/elfeed-open-in-youtube entry))
   (t (ic/elfeed-open-in-eww entry))))

;;;###autoload
(defun ic/elfeed-show-dwim ()
  "Open feed in the most fitting mode."
  (interactive)
  (cond
   ((entry-mp3-p elfeed-show-entry) (ic/elfeed-open-in-bongo elfeed-show-entry))
   ((ic/elfeed-entry-youtube-p elfeed-show-entry) (ic/elfeed-open-in-youtube elfeed-show-entry))
   (t (ic/elfeed-open-in-eww elfeed-show-entry))))

;;;###autoload
(defun ic/elfeed-open-in-youtube (entry)
  "Display the currently selected item in youtube."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (ic/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let* ((url (elfeed-entry-link entry)))
      (ic/bongo-play url)
      (ic/elfeed-external-mode 1))))


(defun ic/elfeed-start-bongo-callback (process signal)
"Callback to be called when a youtube video gets downloaded."
(when (memq (process-status process) '(exit signal))
  (message "Video download finished!")
  (when (not (bongo-playing-p)) (bongo-start/stop)))
  (shell-command-sentinel process signal))

;;;###autoload
(defun ic/elfeed-show-in-youtube ()
  "Display the currently shown item in youtube."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (download-path (if elfeed-youtube-dl-enabled (youtube-get link) (ic/bongo-enqueue-file link))))
      (ic/bongo-enqueue-file-and-play (concat "file://" download-path))
      (ic/elfeed-external-mode 1))))


;;;###autoload
(defun ic/elfeed-open-in-bongo (entry)
  "Display the currently selected item in bongo."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (ic/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (ic/split-and-follow-vertically))
      (ic/elfeed-enqueue-media-url entry)
      (ic/elfeed-external-mode 1))))

;;;###autoload
(defun ic/elfeed-show-in-bongo ()
  "Display the currently shown item in bongo."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (ic/elfeed-enqueue-media-url elfeed-show-entry)
      (ic/elfeed-external-mode 1))))

;;;###autoload
(defun ic/elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (ic/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (ic/split-and-follow-vertically))
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link))
      (ic/elfeed-external-mode))))

;;;###autoload
(defun ic/elfeed-show-in-eww ()
  "Display the currently shown item in xwidget-webkit-browser."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link))
      (ic/elfeed-external-mode))))

;;;###autoload
(defun ic/elfeed-open-in-xwidget-webkit-browser (entry)
  "Display the currently selected item in xwidget-webkit-browser."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (ic/elfeed-delete-non-search-windows)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (ic/split-and-follow-vertically))
      (xwidget-webkit-browse-url link)
      (ic/elfeed-external-mode))))

;;;###autoload
(defun ic/elfeed-show-in-xwidget-webkit-browser ()
  "Display the currently shown item in xwidget-webkit-browser."
  (interactive)
  (require 'elfeed-show)
  (when (elfeed-entry-p elfeed-show-entry)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (xwidget-webkit-browse-url link)
      (ic/elfeed-external-mode))))

