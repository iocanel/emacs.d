;; +bongo.el --- Bongo Extras -*- lexical-binding: t -*-


(use-package bongo
  :config
  (setq bongo-enabled-backends '(mplayer mpv)
        bongo-backend-matchers '((mplayer (local-file "file:" "http:" "https:" "ftp:") "ogg" "flac" "mp3" "mka" "wav" "wma" "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv" "mov" "asf" "wmv" "rm" "rmvb" "ts")
                                 (mpv ("https:") . "youtube")))
  (evil-set-initial-state 'bongo-mode 'emacs) ;; Let's disable evil mode for bongo
  :custom
  (bongo-default-directory "~/Documents/music")
  (bongo-mplayer-extra-arguments '("-af" "scaletempo" "-vf" "scale"))
  :bind (:map bongo-mode-map
              ("C-x i" . bongo-insert-file)
              ("C-x I" . bongo-insert-special)
              ("C-x u" . bongo-insert-uri)
              ("C-x e" . bongo-append-enqueue)
              ("C-x h" . bongo-switch-buffers)))


;; Bongo filename mampings
;; Some podcasts do use redirects, that are not supported by all backends. In these cases we want to apply the redirects upfront.
;; A hacky solution
(defvar ic/bongo-filename-mapping-alist '(("http://dts.podtrac.com/redirect.mp3/feeds.soundcloud.com" . "https://feeds.soundcloud.com")))

;;;###autoload
(defun ic/apply-string-mappings (source mappings)
  "Apply MAPPINGS to the SOURCE string"
  (let ((result source))
    (dolist (mapping mappings)
      (let ((key (car mapping))
            (value (cdr mapping)))
        (setq result (replace-regexp-in-string (regexp-quote key) value result))))
    result))

;;;###autoload
(defun ic/bongo-enqueue-file (filename &rest ignored)
  "Enqueue media from FILENAME to playlist."
  (let ((f (ic/apply-string-mappings filename ic/bongo-filename-mapping-alist)))
    (bongo-playlist)
    (goto-char (point-max))
    (bongo-insert-file filename)
    (goto-char (point-max))
    (search-backward filename)))

;;;###autoload
(defun ic/bongo-enqueue-file-and-play (filename &rest ignored)
  "Enqueue media from FILENAME to playlist."
    (ic/bongo-enqueue-file filename)
    (when (not (bongo-playing-p)) (bongo-start/stop)))

;;;###autoload
(defun ic/bongo-play-file (filename &rest ignored)
  "Play media from FILENAME."
  (with-temp-bongo-playlist-buffer
    (bongo-insert-file filename)
    (backward-char)
    (bongo-play-line))) 

(defun ic/bongo-play (file-or-url)
  "Play the FILE-OR-URL in the bongo player."
  (interactive)
  (if (and ic/bongo-youtube-dl-enabled (ic/youtube-url-p file-or-url))
      (let* ((video-id (substring file-or-url (length ic/youtube-watch-url-prefix) (length file-or-url)))
             (template (concat ic/youtube-download-path video-id))
             (existing-file-name (ic/youtube-get-by-title-filename url))
             (output-path (ic/youtube-local-path file-or-url)))
        (if (and existing-file-name (file-exists-p existing-file-name))
            (progn 
              (message "Youtube video:%s already exists, playing ..." existing-file-name)
              (ic/bongo-play-file existing-file-name))
          (progn
            (message "Youtube video file:%s does not exist, donwloading ..." output-path)
            (ic/youtube-download file-or-url (ic/youtube-callback video-id 'ic/bongo-play-file)))))
    (ic/bongo-play-file file-or-url)))

(defun ic/bongo-play-url-at-point ()
  "Play the url at point in the bonog player."
  (interactive)
  (let* ((url (or (thing-at-point-url-at-point) (ic/org-link-url-at-point))))
         (when url (ic/bongo-play url))))

(defun ic/org-link-url-at-point ()
  "Fetch the url of the org link at point."
  (let* ((org-link (org-element-context))
        (raw-link (org-element-property :raw-link org-link)))
    raw-link))


;;
;; Youtube
;;

(defconst ic/youtube-watch-url-prefix "https://www.youtube.com/watch?v=" "The prefix to the youtube urls")
(defvar ic/youtube-download-path "/home/iocanel/Downloads/Youtube/" "The path to the youtube download folder")
(defvar ic/youtube-download-by-id-path "/home/iocanel/Downloads/Youtube/by-id/" "The path to the youtube by-id folder")
(defvar ic/youtube-download-by-title-path "/home/iocanel/Downloads/Youtube/by-title/" "The path to the youtube by-title folder")
(defvar ic/bongo-youtube-dl-enabled t "Download videos using youtube-dl and play them as loca files, instead of streaming them")
(defvar ic/youtube-dl-buffer-format "*Async youtube-dl: %s*" "The format of the buffer name that will be used to async download the video")
(defvar ic/youtube-rencode-format "mkv" "The format that the downloaded video will be encoded into")
(defvar ic/youtube-max-video-quality "480" "The maximum video quality")
(defvar ic/youtube-max-audio-quality "480" "The maximum audio quality")
(defvar ic/youtube-title-alist '())

;; We need to make sure that the youtbue-dl buffer stays burried!
(add-to-list 'display-buffer-alist (cons "\\*Async youtube-dl: .*\\*" (cons #'display-buffer-no-window nil)))

(defun ic/youtube-url-p (url)
  "Predicate that checks if URL points to youtube."
  (if (stringp url) (string-prefix-p ic/youtube-watch-url-prefix url) nil))

(defun ic/youtube-by-id-path (video-id-or-url)
  "Return the output path (by-id) for the specified youtube url."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url))) video-id-or-url))
    (concat ic/youtube-download-by-id-path video-id "." ic/youtube-rencode-format)))

(defun ic/youtube-by-title-path (video-id-or-url)
  "Return the output path (by-title) for the specified youtube url."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url))) video-id-or-url)
         (title (replace-regexp-in-string "[^[:alnum:]]-" "_" (ic/youtube-get-title video-id))))
    (concat ic/youtube-download-by-title-path title "." ic/youtube-rencode-format)))

(defun ic/youtube-local-path (url)
  "Return the output path for the specified youtube url."
  (concat ic/youtube-download-path
                   (substring url (length ic/youtube-watch-url-prefix) (length url))))

(defun ic/youtube-get-by-id-prefix (video-id-or-url)
  "Return the prefix (by-id) of the youtube video that corresponds to the specified VIDEO-ID-OR-URL."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
         (url (if (ic/youtube-url-p video-id-or-url) video-id-or-url (concat ic/youtube-watch-url-prefix video-id)))
         (output-template (concat ic/youtube-download-by-id-path video-id)))
    (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "youtube-dl \"%s\" --get-filename -o %s" url output-template)))))

(defun ic/youtube-get-by-title-prefix (video-id-or-url)
  "Return the prefix (by-title) of the youtube video that corresponds to the specified VIDEO-ID-OR-URL."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
         (url (if (ic/youtube-url-p video-id-or-url) video-id-or-url (concat ic/youtube-watch-url-prefix video-id)))
         (output-template (concat ic/youtube-download-by-title-path video-id)))
    (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "youtube-dl \"%s\" --get-filename -o %s" url output-template)))))

(defun ic/youtube-get-by-id-filename (video-id-or-url)
  "Return the filename (by-id) of the youtube video that corresponds to the specified VIDEO-ID-OR-URL."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url)))
    (concat ic/youtube-download-by-id-path video-id "." ic/youtube-rencode-format)))

(defun ic/youtube-get-by-title-filename (video-id-or-url)
  "Return the filename (by-title) of the youtube video that corresponds to the specified VIDEO-ID-OR-URL."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
         (title (replace-regexp-in-string "[^[:alnum:]]" "_" (ic/youtube-get-title video-id))))
    (concat ic/youtube-download-by-title-path title "." ic/youtube-rencode-format )))

(defun ic/youtube-get-title (video-id-or-url)
  "Return the filename of the youtube video that corresponds to the specified VIDEO-ID-OR-URL."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
         (url (if (ic/youtube-url-p video-id-or-url) video-id-or-url (concat ic/youtube-watch-url-prefix video-id)))
         (entry (assoc video-id ic/youtube-title-alist))
         (output-template (concat ic/youtube-download-path video-id)))
    (if entry
        (cdr entry)
       (progn
        (let ((title (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "youtube-dl \"%s\" --get-title -o %s" url output-template)))))
          (setq ic/youtube-title-alist (cons `(,video-id . ,title) ic/youtube-title-alist))
        title)))))

(defun ic/youtube-download (video-id-or-url &optional callback)
  "Download the youtube video from VIDEO-ID-OR-URL to a temporary file and return the path to it."
  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
         (template (ic/youtube-get-by-id-prefix url))
         (output-buffer (generate-new-buffer (format ic/youtube-dl-buffer-format video-id)))
         (proc (progn
                 (message "Downloading video into: %s" template)
                 (async-shell-command (format "youtube-dl \"%s\" --no-part --hls-prefer-ffmpeg --recode-video %s -f 'bestvideo[height<=%s]+bestaudio/best[height<=%s]' -o %s" url ic/youtube-rencode-format ic/youtube-max-video-quality ic/youtube-max-audio-quality template) output-buffer)
                 (get-buffer-process output-buffer))))
         (when callback (set-process-sentinel  proc callback))
    template))
  
(defun ic/youtube-callback (video-id-or-url &optional func)
  "Create a callback for the specified VIDEO-ID-OR-URL"
  (lambda (p s) (when (memq (process-status p) `(exit signal))
                  (let* ((video-id (if (ic/youtube-url-p video-id-or-url) (substring video-id-or-url (length ic/youtube-watch-url-prefix) (length video-id-or-url)) video-id-or-url))
                         (by-id-filename (ic/youtube-get-by-id-filename video-id))
                         (by-title-filename (ic/youtube-get-by-title-filename video-id)))
                    (message "Linking %s to %s" by-id-filename by-title-filename)
                    (shell-command-to-string (format "ln -s %s %s" by-id-filename by-title-filename))
                    (cond
                     ((stringp func) (funcall (intern func) by-title-filename))
                     ((symbolp func) (funcall func by-title-filename))
                     (t "video downloaded and linked")))
                  (shell-command-sentinel p s))))
 
 
(defun ic/bongo-start-callback (process signal)
"Callback to be called when a youtube video gets downloaded."
(when (memq (process-status process) '(exit signal))
  (message "Video finished!")
  (with-bongo-playlist-buffer
    (when (not (bongo-playing-p)) (bongo-start/stop)))
  (shell-command-sentinel process signal)))

;;
;; Bluetooth utils
;; 

(defun ic/bluetooth-device-connected-p (device-id)
  "Predicate that checks if bluetooth device with DEVICE-ID is currently connected"
  (not (= (length (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "bluetoothctl info %s | grep 'Connected: yes'" device-id)))) 0)))

(defun ic/get-bluetooth-audio-devices (&optional connected)
  "Returns the the bluetooth audio devices that are available. Optional flag CONNECTED can filter out devices that are/aren't currently connected"
  (let ((device-ids (split-string (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "bluetoothctl paired-devices | cut -d ' ' -f2")) "\n")))
    (if connected
        (seq-filter 'ic/bluetooth-device-connected-p device-ids)
      device-ids)))

;;
;; Capturing
;;

(defun bongo-currently-playing-elapsed-time()
  (interactive)
  "Log the elapsed time"
  (format "%s" (bongo-elapsed-time)))

(defun bongo-currently-playing-url ()
  (interactive)
  "Return the file name of the file currently playing."
  (with-bongo-playlist-buffer
    (cdr (assoc 'file-name (cdr bongo-player)))))

(defun bongo-play-org-entry-at-point ()
  "Play the media file that corresponds to the currently selected org entry."
  (interactive)
  "Play the play the media file at point."
  (let* ((p (point))
         (url  (org-entry-get nil "URL"))
         (time (org-entry-get nil "ELAPSED")))
    (with-bongo-playlist-buffer
      (bongo-insert-uri url)
      (bongo-previous-object)
      (bongo-play)
      (when (stringp time) (bongo-seek-to (string-to-number time))))))


;;
;; Advices
;;

(defadvice bongo-play-line (around bongo-play-line-around activate)
  "Check if bluetooth is connected and use pulse audio driver."
  (interactive)
  (if (ic/get-bluetooth-audio-devices t)
      (let ((bongo-mplayer-audio-driver "pulse")) ad-do-it)
    (let ((bongo-mplayer-audio-driver nil)) ad-do-it)))

(defadvice shr-browse-url (around shr-browse-url-around (&optional external mouse-event new-window) activate)
  "Check if bluetooth is connected and use pulse audio driver."
  (let ((url (get-text-property (point) 'shr-url)))
    (if (s-suffix? ".mp3" url)
        (ic/bongo-enqueue-file url)
      ad-do-it))) 
