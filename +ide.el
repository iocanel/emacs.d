;;
;; IDEE
;;


;;
;; IDEE optional packages
;;

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
                                                            "idee-treemacs.el"
                                                            "idee-projects.el"
                                                            "idee-templates.el"
                                                            "idee-eshell.el"
                                                            "idee-vterm.el"
                                                            "idee-visitors.el"
                                                            "idee-arch.el"
                                                            "idee-yml.el"
                                                            "idee-hydra.el"))
  :custom
  (idee/display-buffer-setup-enabled t)
  (idee/popper-enabled t)
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
  :config
  (use-package polymode :ensure t)
  ;; This is currently broken so let's comment it out
  ;; (advice-add 'idee/treemacs-open-project-workspace :after (lambda (w) (ic/org-github-issues--show-open-workspace-issues w)))
  :bind (("C-c i" . 'idee/hydra/body)
         ("C-c p" . 'idee/project-hydra/body)
         ("C-c f" . 'idee/file-hydra/body)
         ("C-c t" . 'idee/treemacs-hydra/body)
         ("M-m" . 'idee/focus-mode)
         (:map evil-normal-state-map
               ("SPC m" . idee/maven-hydra/body)
               ("SPC t" . idee/treemacs-hydra/body))))

(use-package idee-counsel :straight (idee-counsel :host github :repo "iocanel/idee" :files ("idee-counsel.el"))
  :defer t
  :commands (idee/shell-show-errors)
  :bind (("M-e" .  'idee/shell-show-errors)))

(use-package idee-lsp :straight (idee-lsp :host github :repo "iocanel/idee" :files ("idee-lsp.el"))
  :after idee
  :commands (idee/lsp-init)
  :config
  (idee/lsp-init))

(use-package idee-java :straight (idee-java :host github :repo "iocanel/idee" :files ("idee-java.el" "idee-java-utils.el" "idee-lsp-java.el" "idee-jshell.el" "idee-maven.el" "idee-spring.el" "idee-quarkus.el"))
  :commands (idee/java-init idee/maven-hydra/body)
  :bind (:map evil-normal-state-map
              ("SPC m" . idee/maven-hydra/body))
  :config
  ;; Maven configuration
  (define-derived-mode maven-pom-mode nxml-mode "maven-pom-mode" "Major mode for editting Maven pom files")
  (add-to-list 'auto-mode-alist '("pom\\.xml\\'" . maven-pom-mode))
  (add-to-list 'idee/module-root-markers "pom.xml")
  ;; Populate maven known group ids
  (idee/maven-add-known-group-ids '("io.dekorate"
                                    "io.fabric8"
                                    "io.quarkus"
                                    "io.sundr"
                                    "org.springframework"
                                    "org.junit")))


(use-package idee-dap :straight (idee-dap :host github :repo "iocanel/idee" :files ("idee-dap.el")) :after idee-java)

(use-package idee-javascript :straight (idee-javascript :host github :repo "iocanel/idee" :files ("idee-javascript.el"))
  :defer t
  :commands (idee/javascript-init))

(use-package idee-rust :straight (idee-rust :host github :repo "iocanel/idee" :files ("idee-rust.el"))
  :defer t
  :commands (idee/rust-init))

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
  :defer t
  :commands (idee/docker-build idee/docker-kill idee/docker-run-dockerfile idee/docker-push-dockerfile)
  :bind (:map dockerfile-mode-map 
              ("C-c d b" . 'idee/docker-build)
              ("C-c d k" . 'idee/docker-kill)
              ("C-c d r" . 'idee/docker-run-dockerfile)
              ("C-c d p" . 'idee/docker-push-dockerfile)))

;;
;; Web
;;
;;;###autoload
(defun ic/open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))

;;
;; Bootstrap
;;
(run-with-idle-timer 0 nil 'idee/init)
(run-with-idle-timer 0 nil 'idee/java-init)
(run-with-idle-timer 0 nil 'idee/javascript-init)
(run-with-idle-timer 0 nil 'idee/rust-init)
