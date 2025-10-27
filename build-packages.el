;;; build-packages.el --- Build-time package installation script

;; Disable interactive prompts
(setq confirm-kill-emacs nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Load the configuration, ignoring errors that prevent package installation
(message "Loading init.el...")
(condition-case err
    (load-file "init.el")
  (error 
   (message "Warning: Configuration load failed with error: %s" err)
   (message "Continuing with package installation...")))

;; Force installation of all packages regardless of defer settings
(message "Processing Elpaca queues and installing all packages...")
(when (fboundp 'elpaca-process-queues)
  (elpaca-process-queues)
  
  ;; Wait for initial queue processing
  (message "Initial queue processing...")
  (when (fboundp 'elpaca-wait)
    (elpaca-wait))
  
  ;; Process any remaining queues
  (elpaca-process-queues)
  
  ;; Final wait with timeout
  (message "Final package installation wait...")
  (let ((start-time (current-time))
        (timeout 300)) ; 5 minutes
    (while (and (fboundp 'elpaca--queue-count)
                (> (elpaca--queue-count) 0)
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (message "Waiting for %d packages to complete..." (elpaca--queue-count))
      (sleep-for 2)
      (when (fboundp 'elpaca-wait)
        (elpaca-wait 1))))
  
  (message "Package installation phase completed"))

;; Show final status
(when (fboundp 'elpaca--queue-count)
  (let ((remaining (elpaca--queue-count)))
    (if (> remaining 0)
        (message "WARNING: %d packages may not have completed installation" remaining)
      (message "All packages processed successfully!"))))

(message "Build script completed")
(kill-emacs 0)