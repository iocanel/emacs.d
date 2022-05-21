;;
;; IDE configuration
;;

;;
;; Snippets
;;
(use-package yasnippet
  :defer t
  :init
  (setq yas-snippet-dirs `(
                           ,(concat user-emacs-directory "snippets") ;; personal snippets
                           ,(concat (file-name-as-directory (concat user-emacs-directory "idee")) "snippets") ;; idee snippets
                           ,(concat (file-name-as-directory (concat user-emacs-directory "idee")) "templates") ;; idee templates
                           "~/.config/emacs/snippets"
                           "~/.config/emacs/templates")
        yas-indent-line 'fixed  ;; Use yas-indent-line fixed in yaml-mode. This fixes issues with parameter mirroring breaking indentation
        yas-prompt-functions '(yas-completing-prompt))
  :config
  (yas-recompile-all)
  (yas-reload-all)
  :commands (yas-recompile-all yas-reload-all)
  :hook ((text-mode prog-mode org-mode eshell-mode conf-javaprop-mode) . yas-minor-mode))


(use-package auto-yasnippet
  :after yasnippet
  :defer t
  :config (setq aya-case-fold t)
  :bind (("C-c a c" . aya-create)
         ("C-c a e" . aya-expand)))

(use-package lsp-mode
  :defer t
  :custom (lsp-keymap-prefix "C-c l")
  :config (setq lsp-enable-file-watchers nil
                lsp-idle-del 1)
  :commands (lsp lsp-deferred)
  :bind (("C-c l m" . lsp)
         ("C-c l a" . lsp-execute-code-action)))

(use-package lsp-ui
  :after lsp
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil))

(use-package helm-lsp
  :after (helm lsp)
  :defer t
  :custom (helm-lsp-treemacs-icons nil)
  :bind ("C-c l s" . helm-lsp-workspace-symbol))

(use-package lsp-treemacs
  :after (lsp treemacs)
  :defer t
  :commands lsp-treemacs-errors-list
  :config
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
;; Misc 
;;
(use-package editorconfig
  :defer t
  :commands (editorconfig-format-buffer)
  :bind ("C-c e f" . editorconfig-format-buffer)
  :hook (prog-mode . (lambda () (editorconfig-mode 1))))

(use-package dockerfile-mode
  :defer t
  :commands (dockerfile-build-buffer)
  :bind (:map dockerfile-mode-map
              ("C-c b" . dockerfile-build-buffer)))

;;
;; YAML
;;

(use-package yaml-mode :ensure t)

;;
;; Java
;;

(use-package lsp-java
  :defer t
  :init
  (defvar java-home "/home/iocanel/sdk/candidates/java/current")
  (defvar m2-home "/home/iocanel/sdk/candidates/maven/current")
  (setenv "JAVA_HOME" java-home)
  (setenv "M2_HOME" m2-home)
  (setenv "PATH" (concat (getenv "PATH") (format ":%s/bin:%s/bin" java-home m2-home)))
  (setq
   lsp-java-vmargs '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms1024m" "-Xverify:none" "-jar")
   lsp-java-java-path "/home/iocanel/.sdkman/candidates/java/current/bin/java"
   lsp-java-save-action-organize-imports nil
   lsp-java-maven-download-sources nil
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

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug)
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
          ("C-c d d" . dash-at-point)
          ("C-c e b" . cider-eval-buffer)
          ("C-c e r" . cider-eval-region)
          ("C-c e s" . cider-eval-last-sexp)
          ("C-x e" . cider-eval-last-sexp)))

(use-package cider
  :after clojure-mode
  :defer t
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

;; (use-package paredit
;;   :bind ("M-^" . paredit-delete-indentation)
;;   :bind ("C-^" . paredit-remove-newlines)
;;   :hook ((clojure-mode . paredit-mode)
;;          (cider-mode . paredit-mode)))

;; ;;;###autoload
;; (defun paredit-delete-indentation (&optional arg)
;;   "Handle joining lines that end in a comment."
;;   (interactive "*P")
;;   (let (comt)
;;     (save-excursion
;;       (move-beginning-of-line (if arg 1 0))
;;       (when (skip-syntax-forward "^<" (point-at-eol))
;;         (setq comt (delete-and-extract-region (point) (point-at-eol)))))
;;     (delete-indentation arg)
;;     (when comt
;;       (save-excursion
;;         (move-end-of-line 1)
;;         (insert " ")
;;         (insert comt)))))

;; ;;;###autoload
;; (defun paredit-remove-newlines ()
;;   "Removes extras whitespace and newlines from the current point
;;         to the next parenthesis."
;;   (interactive)
;;   (let ((up-to (point))
;;         (from (re-search-forward "[])}]")))
;;     (backward-char)
;;     (while (> (point) up-to)
;;       (paredit-delete-indentation))))

;;
;; Rust
;;

(use-package rustic
  :defer t
  :after lsp
  :hook ((rustic-mode . lsp))
  :commands (rustic-cargo-add rustic-cargo-remove rustic-cargo-build rustic-cargo-run)
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
  :after lsp
  :defer t
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save)))

;; (use-package lsp-go :straight (lsp-go :host github :repo "emacs-lsp/lsp-mode" :files ("lsp-mode.el" "lsp-protocol.el" "lsp-lens.el" "lsp-completion.el" "clients/lsp-go.el"))
;;   :config
;;   (setq lsp-gopls-staticcheck t
;;         lsp-eldoc-render-all t
;;         lsp-gopls-complete-unimported t))

;;
;; Javascript
;;
(use-package javascript-mode :defer t)
;;
;; Haskell
;;
(use-package haskell-mode
  :defer t
  :commands haskell-mode
  :bind (:map haskell-mode-map ("C-c h" . haskell-hoogle)))

(use-package hindent
  :defer t
  :after haskell-mode
  :config
  (setq hindent-style nil)
  :hook (haskell-mode . hindent-mode)
  :bind (:map hindent-mode-map ("C-c d" . hindent-reformat-decl)))

(use-package ghc
  :after haskell-mode
  :defer t
  :config
  (setq ghc-debug t)
  :hook (haskell-mode . ghc-init))

(use-package company-ghc
  :after ghc
  :defer t
  :demand t
  :config
  (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code)))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

;;
;; Python
;;
(use-package python-mode :defer t)
(use-package anaconda-mode :defer t :after python-mode)
(use-package lsp-python-ms
  :after (python-mode lsp-mode)
  :defer t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred
;;
;; Web
;;
(defun iocanel/open-in-browser()
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
