;;: Turn messages back on

(defun ic/recompile-configuration()
  "Recompile configurration."
  (interactive)
    (let ((config-dir "/home/iocanel/workspace/src/github.com/iocanel/emacs.d/"))
    (mapc (lambda (e)
            (let ((elisp-file (concat config-dir e)))
              (message "Compiling %s." elisp-file)
              (condition-case nil
                  (native-compile elisp-file)
                (error nil))))
          (seq-filter (lambda (f) (string-match "\.el$" f)) (directory-files config-dir)))))



;; Recomplie config if it's not already compiled
(let ((lock-file (locate-user-emacs-file ".config.lock")))
  (when (not (file-exists-p lock-file))
    ;; Compile configuration files
    (ic/recompile-configuration)
    ;; Create lockfile
    (with-temp-buffer (write-file lock-file))))
(setq inhibit-message nil)
