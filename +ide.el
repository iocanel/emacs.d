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
        yas-prompt-functions '(yas-ido-prompt))
  :config (yas-reload-all)
  :hook ((text-mode prog-mode org-mode eshell-mode conf-javaprop-mode) . yas-minor-mode))

(use-package lsp-mode
  :defer t
  :config (setq lsp-enable-file-watchers nil)
  :bind (("C-c l s" . lsp)
         ("C-c l a" . lsp-ui-sideline-apply-code-actions)))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable nil))

(use-package company-lsp)

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list
  :init
  (lsp-treemacs-sync-mode 1))

(use-package lsp-java
  :defer t
  :init
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m" "-Xverify:none")
        lsp-java-save-action-organize-imports nil
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled nil
        lsp-java-import-gradle-enabled nil
        lsp-inhibit-message t
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
;; IDEE
;;
(use-package queue)
(use-package editorconfig
  :hook (prog-mode . (lambda () (editorconfig-mode 1))))

(use-package dockerfile-mode)
(use-package dockerfile-mode)
(use-package demo-it :defer t)
(use-package async-await :defer t)

(use-package idee
  :straight (idee :host github :repo "iocanel/idee")
  :bind (("C-c t" . 'idee-treemacs-hydra/body)))

(use-package idee-java :straight (idee :host github :repo "iocanel/idee"))
(use-package idee-kubernetes :straight (idee :host github :repo "iocanel/idee"))
(use-package idee-docker :straight (idee :host github :repo "iocanel/idee"))

(run-with-idle-timer 1 nil #'idee-init)
(run-with-idle-timer 1 nil #'idee-lsp-init)
(run-with-idle-timer 1 nil #'idee-java-init)

;;
;; IDEE optional packages
;;
(use-package ag)
(use-package helm-ag
  :defer t
  :bind (("C-c p g" . helm-do-ag-project-root)))
