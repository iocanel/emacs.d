;; +bongo.el --- Bongo Extras -*- lexical-binding: t -*-


(defconst ic/youtube-watch-url-prefix "https://www.youtube.com/watch?v=" "The prefix to the youtube urls")
(defvar ic/youtube-download-path "/home/iocanel/Downloads/Youtube/" "The prefix to the youtube urls")

(defvar ic/bongo-youtube-dl-enabled t "Download videos using youtube-dl and play them as loca files, instead of streaming them")

(use-package bongo
  :config
  (setq bongo-enabled-backends '(mplayer mpv)
        bongo-backend-matchers '((mplayer (local-file "file:" "http:" "https:" "ftp:") "ogg" "flac" "mp3" "mka" "wav" "wma" "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv" "mov" "asf" "wmv" "rm" "rmvb" "ts")
                                 (mpv ("https:") . "youtube")))
  (evil-set-initial-state 'bongo-mode 'emacs) ;; Let's disable evil mode for bongo
  :custom
  (bongo-default-directory "~/Documents/music")
  (bongo-mplayer-extra-arguments '("-af" "scaletempo"))
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
             (template (concat ic/youtube-download-path video-id)))
        (ic/youtube-get file-or-url (ic/bongo-play-callback video-id)))
    (ic/bongo-play-file file-or-url)))

(defun ic/bongo-play-url-at-point ()
  "Play the url at point in the bonog player."
  (interactive)
  (let* ((url (or (thing-at-point-url-at-point) (ic/org-link-url-at-point))))
         (when url (ic/bongo-play url))))

;;
;; Youtube
;;
(defvar ic/youtube-dl-buffer-format "*Async youtube-dl: %s*")
;; We need to make sure that the youtbue-dl buffer stays burried!
(add-to-list 'display-buffer-alist (cons "\\*Async youtube-dl: .*\\*" (cons #'display-buffer-no-window nil)))


(defun ic/org-link-url-at-point ()
  (interactive)
  (let* ((org-link (org-element-context))
        (raw-link (org-element-property :raw-link org-link)))
    raw-link))

(defun ic/youtube-url-p (url)
  "Predicate that checks if URL points to youtube."
  (if (stringp url) (string-prefix-p ic/youtube-watch-url-prefix url) nil))

(defun ic/youtube-get (url &optional callback)
  "Download the youtube video from URL to a temporary file and return the path to it."
  (let* ((video-id (substring url (length ic/youtube-watch-url-prefix) (length url)))
         (template (concat ic/youtube-download-path video-id))
         (output-buffer (generate-new-buffer (format ic/youtube-dl-buffer-format video-id)))
         (proc (progn
                 (message "Downloading video into: %s" template)
                 (async-shell-command (format "youtube-dl \"%s\" -o %s" url template) output-buffer)
                 (get-buffer-process output-buffer))))
         (when callback (set-process-sentinel  proc callback))
    template))
  
(defun ic/bongo-play-callback (video-id)
  "Create a callback for the specified VIDEO-ID"
  (lambda (p s) (when (memq (process-status p) `(exit signal))
                  (ic/bongo-play-file (concat ic/youtube-download-path
                                              (car (seq-filter
                                                    (lambda (f) (string-prefix-p video-id f))
                                                    (directory-files ic/youtube-download-path)))))
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
