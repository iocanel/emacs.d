(use-package bongo
  :config
  (setq bongo-enabled-backends '(mplayer mpv)
        bongo-backend-matchers '((mplayer (local-file "file:" "http:" "https:" "ftp:") "ogg" "flac" "mp3" "mka" "wav" "wma" "mpg" "mpeg" "vob" "avi" "ogm" "mp4" "mkv" "mov" "asf" "wmv" "rm" "rmvb" "ts")
                                 (mpv ("https:") . "youtube")))
  (evil-set-initial-state 'bongo-mode 'emacs) ;; Let's disable evil mode for bongo
  :custom
  (bongo-default-directory "~/Documents/music")
  :bind (:map bongo-mode-map
              ("C-x i" . bongo-insert-file)
              ("C-x I" . bongo-insert-special)
              ("C-x u" . bongo-insert-uri)
              ("C-x e" . bongo-append-enqueue)
              ("C-x h" . bongo-switch-buffers)))


;; Bongo filename mampings
;; Some podcasts do use redirects, that are not supported by all backends. In these cases we want to apply the redirects upfront.
;; A hacky solution
(defvar bongo-filename-mapping-alist '(("http://dts.podtrac.com/redirect.mp3/feeds.soundcloud.com" . "https://feeds.soundcloud.com")))

;;;###autoload
(defun apply-string-mappings (source mappings)
  "Apply MAPPINGS to the SOURCE string"
  (let ((result source))
    (dolist (mapping mappings)
      (let ((key (car mapping))
            (value (cdr mapping)))
        (setq result (replace-regexp-in-string (regexp-quote key) value result))))
    result))

;;;###autoload
(defun iocanel/bongo-enqueue-file (filename &rest ignored)
  "Enqueue media from FILENAME to playlist."
  (let ((f (apply-string-mappings filename bongo-filename-mapping-alist)))
    (bongo-playlist)
    (goto-char (point-max))
    (bongo-insert-file filename)
    (goto-char (point-max))
    (search-backward filename)))

;;;###autoload
(defun iocanel/bongo-enqueue-file-and-play (filename &rest ignored)
  "Enqueue media from FILENAME to playlist."
    (iocanel/bongo-enqueue-file filename)
    (when (not (bongo-playing-p)) (bongo-start/stop)))

;;;###autoload
(defun iocanel/bongo-play-file (filename &rest ignored)
  "Play media from FILENAME."
  (with-temp-bongo-playlist-buffer
    (bongo-insert-file filename)
    (backward-char)
    (bongo-play-line))) 

;;
;; Bluetooth utils
;; 

(defun iocanel/bluetooth-device-connected-p (device-id)
  "Predicate that checks if bluetooth device with DEVICE-ID is currently connected"
  (not (= (length (replace-regexp-in-string "\n\\'" "" (shell-command-to-string (format "bluetoothctl info %s | grep 'Connected: yes'" device-id)))) 0)))

(defun iocanel/get-bluetooth-audio-devices (&optional connected)
  "Returns the the bluetooth audio devices that are available. Optional flag CONNECTED can filter out devices that are/aren't currently connected"
  (let ((device-ids (split-string (replace-regexp-in-string "\n\\'" "" (shell-command-to-string "bluetoothctl paired-devices | cut -d ' ' -f2")) "\n")))
    (if connected
        (seq-filter 'iocanel/bluetooth-device-connected-p device-ids)
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

(defun bongo-play-org-entry-at-poing ()
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
  (if (iocanel/get-bluetooth-audio-devices t)
      (let ((bongo-mplayer-audio-driver "pulse")) ad-do-it)
    (let ((bongo-mplayer-audio-driver nil)) ad-do-it)))

(defadvice shr-browse-url (around shr-browse-url-around (&optional external mouse-event new-window) activate)
  "Check if bluetooth is connected and use pulse audio driver."
  (let ((url (get-text-property (point) 'shr-url)))
    (if (s-suffix? ".mp3" url)
        (iocanel/bongo-enqueue-file url)
      ad-do-it))) 
