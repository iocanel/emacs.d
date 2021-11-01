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
  :straight (idee :host github :repo "iocanel/idee")
  :bind (("C-c i" . 'idee-hydra/body)
         ("C-c p" . 'idee-project-hydra/body)
         ("C-c f" . 'idee-file-hydra/body)
         ("C-c t" . 'idee-treemacs-hydra/body)
         ("M-m" . 'idee-focus-mode))
  :config (idee-init)

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
          (idee-jump-to-non-ide-window)
          (ic/split-and-follow-horizontally)
          (eww url))))
 

;;
;; End of additional code
;;
(advice-add 'idee-treemacs-open-project-workspace :after (lambda (w) (ic/org-github-issues--show-open-workspace-issues w)))
)

(use-package idee-counsel :straight (idee :host github :repo "iocanel/idee")
  :after idee
  :defer t
  :commands (idee-shell-show-errors)
  :bind (("M-e" .  'idee-shell-show-errors)))

(use-package idee-lsp :straight (idee :host github :repo "iocanel/idee")
  :after idee
  :config
  (idee-lsp-init))

(use-package idee-java :straight (idee :host github :repo "iocanel/idee")
  :after idee
  :config
  (idee-java-init)
 ;; Maven configuration
 (define-derived-mode maven-pom-mode nxml-mode "maven-pom-mode" "Major mode for editting Maven pom files")
 (add-to-list 'auto-mode-alist '("pom\\.xml\\'" . maven-pom-mode))
 (add-to-list 'idee-module-root-markers "pom.xml")
 ;; Populate maven known group ids
 (add-to-list 'idee-maven-known-group-ids "io.dekorate")
 (add-to-list 'idee-maven-known-group-ids "io.fabric8")
 (add-to-list 'idee-maven-known-group-ids "io.quarkus")
 (add-to-list 'idee-maven-known-group-ids "io.sundr")
 (add-to-list 'idee-maven-known-group-ids "org.springframework")
 (add-to-list 'idee-maven-known-group-ids "org.junit"))

(use-package idee-dap :straight (idee :host github :repo "iocanel/idee")
  :after idee
  :defer t)

(use-package idee-kubernetes :straight (idee :host github :repo "iocanel/idee")
  :after (idee yaml-mode)
  :defer t
  :commands (idee-kubernetes-create-dwim
             idee-kubernetes-create-from-buffer
             idee-kubernetes-create-from-region
             idee-kubernetes-delete-dwim
             idee-kubernetes-delete-from-buffer
             idee-kubernetes-delete-from-region
             idee-kubernetes-replace-dwim
             idee-kubernetes-replace-from-buffer
             idee-kubernetes-replace-from-region)
  :bind (:map yaml-mode-map 
         ("C-c k c" . 'idee-kubernetes-create-dwim)
         ("C-c k d" . 'idee-kubernetes-delete-dwim)
         ("C-c u d" . 'idee-kubernetes-update-dwim)))

(use-package idee-docker :straight (idee :host github :repo "iocanel/idee")
  :after (idee dockerfile-mode)
  :bind (:map dockerfile-mode-map 
         ("C-c d b" . 'idee-docker-build)
         ("C-c d k" . 'idee-docker-kill)
         ("C-c d r" . 'idee-docker-run-dockerfile)
         ("C-c d p" . 'idee-docker-push-dockerfile)))

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
(defadvice projectile-kill-buffers (around auto-confirm compile activate)
      (flet ((yes-or-no-p (&rest args) t)
             (y-or-n-p (&rest args) t))
        ad-do-it))
