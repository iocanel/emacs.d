;;
;; IDEE
;;
(use-package queue)


;;
;; IDEE optional packages
;;
(use-package ag :ensure t)
(use-package polymode :ensure t)
(use-package helm-ag
  :defer t
  :straight (helm-ag :host github :repo "ioforks/helm-ag")
  :commands (helm-do-ag-project-root helm-ag)
  :bind (("C-c g" . helm-do-ag-project-root)
         ("C-c r" . helm-resume)))

(use-package idee
  :defer t
  :straight (idee :host github :repo "iocanel/idee" :files ("idee.el"
                                                            "idee-utils.el"
                                                            "idee-vars.el"
                                                            "idee-actions.el"
                                                            "idee-comments.el"
                                                            "idee-headers.el"
                                                            "idee-navigation.el"
                                                            "idee-views.el"
                                                            "idee-hydra.el"
                                                            "idee-treemacs.el"
                                                            "idee-projects.el"
                                                            "idee-templates.el"
                                                            "idee-eshell.el"
                                                            "idee-vterm.el"
                                                            "idee-visitors.el"
                                                            "idee-arch.el"
                                                            "idee-yml.el"))
  :custom
  (idee/display-buffer-enabled t)
  (idee/popper-enabled t)
  :init
  (evil-leader/set-key "m" 'idee/maven-hydra/body)
  (evil-leader/set-key "t" 'idee/treemacs-hydra/body)
  :commands (idee/init
             idee/open
             idee/vcs
             idee/terminal-view
             idee/side-by-side-view
             idee/new-project
             idee/treemacs-hydra/body
             idee/treemacs-switch-to-project-workspace
             idee/treemacs-create-and-switch-to-workspace
             idee/focus-mode)
  :bind (("C-c i" . 'idee/hydra/body)
         ("C-c p" . 'idee/project-hydra/body)
         ("C-c f" . 'idee/file-hydra/body)
         ("C-c t" . 'idee/treemacs-hydra/body)
         ("M-m" . 'idee/focus-mode))
  :config
  ;;
  ;; Additional code
  ;;

;;;###autoload
  (defun ic/org-github-issues--show-open-project-issues (root)
    "Show all the project issues currently assigned to me."
    (let* ((project (projectile-ensure-project root))
           (project-name (projectile-project-name project)))
      (org-ql-search "~/Documents/org/github.org"
                     `(and (property "GH_URL")
                           (string-match (regexp-quote ,project-name) (org-entry-get (point) "GH_URL")))
                     :title (format "Github issues for %s" project-name))
      (goto-char (point-min))
      (org-agenda-next-line)))

;;;###autoload
  (defun ic/org-github-issues--show-open-workspace-issues (workspace)
    "Show all the workspace issues currently assigned to me."
    (let* ((name (treemacs-project->name workspace))
           (projects (treemacs-workspace->projects workspace))
           (project-names (mapcar (lambda (p) (treemacs-project->name p)) projects))
           (main-project (car project-names)))
      (when main-project 
        (org-ql-search "~/Documents/org/github.org"
                       `(and (property "GH_URL")
                             (or (string-match (regexp-quote ,main-project) (org-entry-get (point) "GH_URL"))
                                 (seq-filter (lambda (p) (string-match (regexp-quote p) (org-entry-get (point) "GH_URL"))) project-names)))
                       :title (format "Github issues for %s" name))
        (goto-char (point-min))
        (org-agenda-next-line))))


  (defun ic/org-github-issues--url-at-point ()
    (save-excursion
      (let ((origin (current-buffer)))
        (when (eq major-mode 'org-agenda-mode) (org-agenda-switch-to))
        (let* ((p (point))
               (url (string-trim (org-entry-get nil "GH_URL"))))
          (when (not (equal origin (current-buffer))) (switch-to-buffer origin))
          url))))

  (defun ic/org-github-issues--eww-entry-at-point ()
    "Browse the issue that corresponds to the org entry at point."
    (interactive)
    (let ((url (ic/org-github-issues--url-at-point)))
      (when url 
        (other-window 1)
        (idee/jump-to-non-ide-window)
        (ic/split-and-follow-horizontally)
        (eww url))))



  ;;
  ;; End of additional code
  ;;
  (advice-add 'idee-treemacs-open-project-workspace :after (lambda (w) (ic/org-github-issues--show-open-workspace-issues w)))
  )

(use-package idee-counsel :straight (idee-counsel :host github :repo "iocanel/idee" :files ("idee-counsel.el"))
  :defer t
  :commands (idee-shell-show-errors)
  :bind (("M-e" .  'idee/shell-show-errors)))

(use-package idee-lsp :straight (idee-lsp :host github :repo "iocanel/idee" :files ("idee-lsp.el"))
  :after idee
  :config
  (idee/lsp-init))

(use-package idee-java :straight (idee-java :host github :repo "iocanel/idee" :files ("idee-java.el" "idee-java-utils.el" "idee-lsp-java.el" "idee-jshell.el" "idee-maven.el" "idee-spring.el" "idee-quarkus.el"))
  :defer t
  :commands (idee/maven-hydra/body)
  :init
  (evil-leader/set-key "m" 'idee-maven-hydra/body)
  :config
  (idee/java-init)

  ;; Maven configuration
  (define-derived-mode maven-pom-mode nxml-mode "maven-pom-mode" "Major mode for editting Maven pom files")
  (add-to-list 'auto-mode-alist '("pom\\.xml\\'" . maven-pom-mode))
  (add-to-list 'ide-module-root-markers "pom.xml")
  ;; Populate maven known group ids
  (add-to-list 'idee/maven-known-group-ids "io.dekorate")
  (add-to-list 'idee/maven-known-group-ids "io.fabric8")
  (add-to-list 'idee/maven-known-group-ids "io.quarkus")
  (add-to-list 'idee/maven-known-group-ids "io.sundr")
  (add-to-list 'idee/maven-known-group-ids "org.springframework")
  (add-to-list 'idee/maven-known-group-ids "org.junit"))

(use-package idee-dap :straight (idee-dap :host github :repo "iocanel/idee" :files ("idee-dap.el")))

(use-package idee-javascript :straight (idee-javascript :host github :repo "iocanel/idee" :files ("idee-javascript.el"))
  :config
  (idee/javascript-init))

(use-package idee-rust :straight
  (idee-rust :host github :repo "iocanel/idee" :files ("idee-rust.el"))
  :config
  (idee/rust-init))

(use-package idee-kubernetes :straight (idee-kubernetes :host github :repo "iocanel/idee" :files ("idee-kubernetes.el"))
  :defer t
  :commands (idee/kubernetes-create-dwim
             idee/kubernetes-create-from-buffer
             idee/kubernetes-create-from-region
             idee/kubernetes-delete-dwim
             idee/kubernetes-delete-from-buffer
             idee/kubernetes-delete-from-region
             idee/kubernetes-replace-dwim
             idee/kubernetes-replace-from-buffer
             idee/kubernetes-replace-from-region)
  :bind (:map yaml-mode-map 
              ("C-c k c" . 'idee/kubernetes-create-dwim)
              ("C-c k d" . 'idee/kubernetes-delete-dwim)
              ("C-c u d" . 'idee/kubernetes-update-dwim)))

(use-package idee-docker :straight (idee-docker :host github :repo "iocanel/idee" :files ("idee-docker.el"))
  :commands (idee/docker-build idee/docker-kill idee/docker-run-dockerfile idee/docker-push-dockerfile)
  :bind (:map dockerfile-mode-map 
              ("C-c d b" . 'idee/docker-build)
              ("C-c d k" . 'idee/docker-kill)
              ("C-c d r" . 'idee/docker-run-dockerfile)
              ("C-c d p" . 'idee/docker-push-dockerfile)))

;;
;; Web
;;
(defun ic/open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


;;
;; Skip confirmation when killing project buffers (from EmacsWiki)
;;
;; (defadvice projectile-kill-buffers (around auto-confirm compile activate)
;;   (flet ((yes-or-no-p (&rest args) t)
;;          (y-or-n-p (&rest args) t))
;;     ad-do-it))

;;
;; Bootstrap
;;
(run-with-idle-timer 0 nil 'idee/init)
