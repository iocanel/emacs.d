;;: Turn messages back on

(byte-recompile-directory (concat
                           (file-name-as-directory user-emacs-directory) "straight/build"))
 (setq inhibit-message nil)
