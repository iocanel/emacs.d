;;
;; IDE configuration
;;

;;
;; Snippets
;;
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"                 ;; personal snippets
                           "~/.config/emacs/snippets"
                           "~/.config/emacs/templates")
        yas-indent-line 'fixed  ;; Use yas-indent-line fixed in yaml-mode. This fixes issues with parameter mirroring breaking indentation
        yas-prompt-functions '(yas-completing-prompt))
  :config (yas-reload-all)
  :hook ((text-mode prog-mode org-mode eshell-mode conf-javaprop-mode) . yas-minor-mode))


(use-package auto-yasnippet
  :config (setq aya-case-fold t)
  :bind (("C-c a c" . aya-create)
         ("C-c a e" . aya-expand)))

(use-package lsp-mode
  :config (setq lsp-enable-file-watchers nil
                lsp-idle-del 1)
  :bind (("C-c l m" . lsp)
         ("C-c l a" . lsp-execute-code-action)))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil))


(use-package helm-lsp
  :custom (helm-lsp-treemacs-icons nil)
  :bind ("C-c l s" . helm-lsp-workspace-symbol))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :init
  (lsp-treemacs-sync-mode 1))

;;
;; Hideshow
;;

;; From: https://github.com/mwfogleman/.emacs.d/blob/master/michael.org#hideshow
(use-package hideshow
  :defer t
  :bind ("M-h" . ic/toggle-fold)
  :hook ((prog-mode . hs-minor-mode)))

;;;###autoload
(defun ic/toggle-fold ()
  (interactive)
  "Toggle folding."
  (hs-toggle-hiding)
  (backward-char))


;;
;; YAML
;;

(use-package yaml-mode :ensure t)

;;
;; Java
;;

(defvar java-home "/home/iocanel/sdk/candidates/java/current")
(defvar m2-home "/home/iocanel/sdk/candidates/maven/current")

(use-package lsp-java
  :defer t
  :init
  (setenv "JAVA_HOME" java-home)
  (setenv "M2_HOME" m2-home)
  (setenv "PATH" (format "/bin:/usr/bin:/usr/local/bin:%s/bin:%s/bin:%s/bin" (expand-file-name "~")  java-home m2-home))
  (setq
        lsp-java-vmargs '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx8G" "-Xms1024m" "-Xverify:none" "-jar")
        lsp-java-java-path "/home/iocanel/.sdkman/candidates/java/current/bin/java"
        lsp-java-save-action-organize-imports nil
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled nil
        lsp-java-import-gradle-enabled nil
        lsp-inhibit-message nil
        lsp-java-format-on-type-enabled nil
        lsp-java-completion-guess-arguments t
        lsp-java-completion-overwrite nil
        c-basic-offset 2
        tab-width 2)
  :hook ((java-mode . lsp))
  :bind (("C-c j b" . lsp-java-build-project)
         ("C-c j o" . lsp-java-organize-imports)
         ("C-c j g g" . lsp-java-generate-getters-and-setters)
         ("C-c j g s" . lsp-java-generate-to-string)
         ("C-c j g e" . lsp-java-generate-equals-and-hash-code)
         ("C-c j u" . lsp-java-update-project-configuration)))

(evil-leader/set-key "m" #'idee-maven-hydra/body)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  :bind (("C-c j r t c" . dap-java-run-test-class)
         ("C-c j r t m" . dap-java-run-test-method)
         ("C-c j d t c" . dap-java-debug-test-class)
         ("C-c j d t m" . dap-java-debug-test-method)))


;;
;; Clojure
;;
(use-package clojure-mode
  :defer t
  :init
  (defconst clojure--prettify-symbols-alist '(("fn"   . ?λ)))
  :hook (clojure-mode . global-prettify-symboles-mode)
  :bind ( :map clojure-mode-map
               ("C-c d f" . cider-code)
               ("C-c d g" . cider-grimoire)
               ("C-c d w" . cider-grimoire-web)
               ("C-c d c" . clojure-cheatsheet)
               ("C-c d d" . dash-at-point)))

(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t
        ;; Stop error buffer from popping up while working in buffers other than the REPL:
        nrepl-popup-stacktraces nil)
  :hook ((cider-mode . company-mode)
         (cider-repl-mode . company-mode))
  :bind (:map cider-mode-map
              ("C-c C-v C-c" . cider-send-and-evaluate-sexp)
              ("C-c C-p"     . cider-eval-print-last-sexp)))
 
(use-package paredit
  :bind ("M-^" . paredit-delete-indentation)
  :bind ("C-^" . paredit-remove-newlines)
  :hook ((clojure-mode . paredit-mode)
         (cider-mode . paredit-mode)))

;;;###autoload
(defun paredit-delete-indentation (&optional arg)
  "Handle joining lines that end in a comment."
  (interactive "*P")
  (let (comt)
    (save-excursion
      (move-beginning-of-line (if arg 1 0))
      (when (skip-syntax-forward "^<" (point-at-eol))
        (setq comt (delete-and-extract-region (point) (point-at-eol)))))
    (delete-indentation arg)
    (when comt
      (save-excursion
        (move-end-of-line 1)
        (insert " ")
        (insert comt)))))

;;;###autoload
(defun paredit-remove-newlines ()
  "Removes extras whitespace and newlines from the current point
        to the next parenthesis."
  (interactive)
  (let ((up-to (point))
        (from (re-search-forward "[])}]")))
    (backward-char)
    (while (> (point) up-to)
      (paredit-delete-indentation))))

;;
;; Rust
;;

(use-package rustic
  :defer t
  :hook ((rustic-mode . lsp))
  :bind (:map rustic-mode-map
              ("C-c c a" . rustic-cargo-add)
              ("C-c c r" . rustic-cargo-remove)
              ("C-c c b" . rustic-cargo-build)
              ("C-c c x" . rustic-cargo-run)
              ("C-c r a s" . lsp-rust-analyzer-status)))
;;
;; Golang
;;

(use-package go-mode
  :defer t
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save)))



;; (use-package lsp-go :straight (lsp-go :host github :repo "emacs-lsp/lsp-mode" :files ("lsp-mode.el" "lsp-protocol.el" "lsp-lens.el" "lsp-completion.el" "clients/lsp-go.el"))
;;   :config
;;   (setq lsp-gopls-staticcheck t
;;         lsp-eldoc-render-all t
;;         lsp-gopls-complete-unimported t))


;;
;; Haskell
;;

(use-package haskell-mode
  :commands haskell-mode
  :bind
  (:map haskell-mode-map
        ("C-c h" . haskell-hoogle)))

 (use-package hindent
  :disabled t
  :after haskell-mode
  :bind (:map hindent-mode-map
              ("C-c d" . hindent-reformat-decl))

  (use-package ghc
  :after haskell-mode
  :disabled t
  :config
  (progn
    (setq ghc-debug t)
    (add-hook 'haskell-mode-hook 'ghc-init)))
  :config
  (progn
    (setq hindent-style nil)
    (add-hook 'haskell-mode-hook 'hindent-mode)))

(use-package company-ghc
  :disabled t
  :demand t
  :config
  (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code)))

(use-package flycheck
  :defer t
  :hook (java-mode . flycheck-mode))


;;
;; Python
;;
(use-package python-mode)
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

;;
;; IDEE
;;
(use-package queue)
(use-package counsel)
(use-package editorconfig
  :hook (prog-mode . (lambda () (editorconfig-mode 1))))

(use-package dockerfile-mode)
(use-package dockerfile-mode)
(use-package demo-it :defer t)
(use-package async-await :defer t)

;;
;; IDEE optional packages
;;
(use-package ag)
(use-package helm-ag :bind (("C-c g" . helm-do-ag-project-root)))
(use-package polymode)


(use-package idee
  :straight (idee :host github :repo "iocanel/idee")
  :config (idee-init)
  :bind (("C-c i" . 'idee-hydra/body)
         ("C-c p" . 'idee-project-hydra/body)
         ("C-c f" . 'idee-file-hydra/body)
         ("C-c t" . 'idee-treemacs-hydra/body)
         ("M-m" . 'idee-focus-mode)))

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

;;;###autoload
(defun ic/org-github-issues--kill-buffer-and-window (&optional buffer-or-name)
  "Kill the github issues window and buffer.  Return t if grep window was found."
  (let* ((buffer (current-buffer)))
    (if (string-match "Github issues for" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (idee-refresh-view)
          t)
        nil)))

(add-to-list 'idee-burry-buffer-listener-list #'ic/org-github-issues--kill-buffer-and-window)

(use-package idee-counsel :straight (idee :host github :repo "iocanel/idee")
  :bind (("M-e" .  'idee-shell-show-errors)))

(use-package idee-lsp :straight (idee :host github :repo "iocanel/idee")
  :config
  (idee-lsp-init))

(use-package idee-java :straight (idee :host github :repo "iocanel/idee")
  :config
  (idee-java-init))

(use-package idee-dap :straight (idee :host github :repo "iocanel/idee"))

(use-package idee-kubernetes :straight (idee :host github :repo "iocanel/idee"))

(use-package idee-docker :straight (idee :host github :repo "iocanel/idee")
  :bind (:map dockerfile-mode-map 
         ("C-c d b" . 'idee-docker-build)
         ("C-c d k" . 'idee-docker-kill)
         ("C-c d r" . 'idee-docker-run-dockerfile)
         ("C-c d p" . 'idee-docker-push-dockerfile)))


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
(add-to-list 'idee-maven-known-group-ids "org.junit")


;;
;; Advices
;;
(advice-add 'idee-treemacs-open-project-workspace :after (lambda (w) (ic/org-github-issues--show-open-workspace-issues w)))
(advice-add 'iocanel/swiper-isearch-with-selection :after 'idee-refresh-view)
(advice-add 'iocanel/swiper-isearch-with-selection-fuzzy :after 'idee-refresh-view)

;;
;; Web
;;
(defun iocanel/open-in-browser()
  (interactive)
  (let ((filename (buffer-file-name)))
    (browse-url (concat "file://" filename))))


;;
;; Yas Snippets
;;
(yas-reload-all)


;;
;; Skip confirmation when killing project buffers (from EmacsWiki)
;;
(defadvice projectile-kill-buffers (around auto-confirm compile activate)
      (flet ((yes-or-no-p (&rest args) t)
             (y-or-n-p (&rest args) t))
        ad-do-it))
