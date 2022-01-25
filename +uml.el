;;; +uml.el --- UML Configuration -*- lexical-binding: t; -*-

;;
;; PlantUML
;;
(use-package plantuml-mode
  :commands plantuml-download-jar
  :init
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)
               (setq plantuml-jar-path (concat user-emacs-directory "plantuml/" "plantuml.jar")
                     org-plantuml-jar-path plantuml-jar-path))
  :hook (plantuml-mode . yas/minor-mode))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

;;
;; yuml
;;
(use-package eyuml
  :init (add-to-list 'org-src-lang-modes '("yuml" . yuml)))

(defun org-babel-execute:yuml (body params)
  "Execute a block of yuml code with org-babel."
  (let ((in-file (org-babel-temp-file "" ".yuml"))
        (type (or (cdr (assq :type params))
		      (error "yuml requires a \":type\" header argument")))
        (out-file (or (cdr (assq :file params))
		      (error "yuml requires a \":file\" header argument")))
        (verbosity (or (cdr (assq :verbosity params)) 0)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "," nil t))
      (write-region nil nil in-file)
      (message (buffer-substring (point-min) (point-max)))
      (eyuml-create-document type out-file))
    (format "[[file:%s]]" out-file)))

(defun eyuml-create-document (type &optional out-file)
  "Fetch remote document, TYPE could be class,usecase or activity."
  (let ((out-file (or out-file (eyuml-create-file-name))))
    (request (eyuml-create-url type)
      :parser 'buffer-string
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data
                    (with-temp-buffer 
                      (set-buffer-file-coding-system 'raw-text)
                      (insert data)
                      (write-region nil nil out-file))))))))

;;
;; Flowchart.js
;;

(add-to-list 'org-src-lang-modes '("flowchart-js" . flowchart-js))
(defun org-babel-execute:flowchart-js (body params)
  "Execute a block of flowchartjs code with org-babel."
  (let* ((in-file (org-babel-temp-file "" ".flowchart-js"))
         (out-file (or (cdr (assq :file params))
		      (error "flowchart-js requires a \":file\" header argument")))
         (cmd (format "diagrams flowchart %s %s" in-file out-file))
        (verbosity (or (cdr (assq :verbosity params)) 0)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (write-region nil nil in-file))
    (shell-command cmd)
    nil))

