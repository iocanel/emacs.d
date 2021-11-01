(use-package org-jira :straight (org-jira :host github :repo "ahungry/org-jira")
  :defer t
  :config
  (setq jiralib-url "https://issues.redhat.com/"
        jiralib-user-login-name "iocanel@gmail.com"
        jira-password (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show websites/redhat.com/iocanel@gmail.com"))
        org-jira-working-dir "~/Documents/org/jira/"
        org-jira-projects-list '("ENTSBT" "SB"))
  ;; Login
  (jiralib-login jiralib-user-login-name jira-password)

  ;; Additional code
  (defvar org-jira-selected-board nil)
  (defvar org-jira-selected-sprint nil)
  (defvar org-jira-selected-epic nil)

  (defvar org-jira-boards-cache ())
  (defvar org-jira-sprint-by-board-cache ())
  (defvar org-jira-epic-by-board-cache ())

  ;;
  ;; Boards
  ;;
  (defun org-jira-list-boards()
    "List all boards."
    (unless org-jira-boards-cache
      (setq org-jira-boards-cache (jiralib--agile-call-sync "/rest/agile/1.0/board" 'values)))
    org-jira-boards-cache)

  (defun org-jira-get-board-id()
    "Select a board if one not already selected."
    (unless org-jira-selected-board
      (setq org-jira-selected-board (org-jira-board-completing-read)))
    (cdr (assoc 'id org-jira-selected-board)))

  (defun org-jira-get-board()
    "Select a board if one not already selected."
    (unless org-jira-selected-board
      (setq org-jira-selected-board (org-jira-board-completing-read)))
    org-jira-selected-board)

  (defun org-jira-select-board()
    "Select a board."
    (interactive)
    (setq org-jira-selected-board (org-jira-board-completing-read)))

  (defun org-jira-board-completing-read()
    "Select a board by name."
    (interactive)
    (let* ((boards (org-jira-list-boards))
           (board-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) boards))
           (board-name (completing-read "Choose board:" board-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) board-name)) boards))))

  ;;
  ;; Sprint
  ;;
  (defun org-jira-get-project-boards(project-id)
    "Find the board of the project.")

  (defun org-jira-get-sprints-by-board(board-id &optional filter)
    "List all sprints by BOARD-ID."
    (interactive)
    (let ((board-sprints-cache (cdr (assoc board-id org-jira-sprint-by-board-cache))))
      (unless board-sprints-cache
        (setq board-sprints-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/sprint" board-id)'values)))

      (add-to-list 'org-jira-sprint-by-board-cache `(,board-id . ,board-sprints-cache))
      (if filter
          (seq-filter filter board-sprints-cache)
        board-sprints-cache)))

  (defun org-jira--active-sprint-p(sprint)
    "Predicate that checks if SPRINT is active."
    (not (assoc 'completeDate sprint)))


  (defun org-jira-sprint-completing-read(board-id)
    "Select an active sprint by name."
    (let* ((sprints (org-jira-get-sprints-by-board board-id 'org-jira--active-sprint-p))
           (sprint-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) sprints))
           (sprint-name (completing-read "Choose sprint:" sprint-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) sprint-name)) sprints))))

  (defun org-jira-move-issue-to-sprint(issue-id sprint-id)
    "Move issue with ISSUE-ID to sprint with SPRINT-ID."
    (jiralib--rest-call-it (format "/rest/agile/1.0/sprint/%s/issue" sprint-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

  (defun org-jira-assign-current-issue-to-sprint()
    "Move the selected issue to an active sprint."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (board-id (cdr (assoc 'id (org-jira-get-board))))
           (sprint-id (cdr (assoc 'id (org-jira-sprint-completing-read board-id)))))

      (org-jira-move-issue-to-sprint issue-id sprint-id)))

  (defun org-jira-get-sprint-id()
    "Select a sprint id if one not already selected."
    (unless org-jira-selected-sprint
      (setq org-jira-selected-sprint (org-jira-sprint-completing-read)))
    (cdr (assoc 'id org-jira-selected-sprint)))

  (defun org-jira-get-sprint()
    "Select a sprint if one not already selected."
    (unless org-jira-selected-sprint
      (setq org-jira-selected-sprint (org-jira-select-sprint)))
    org-jira-selected-sprint)

  (defun org-jira-select-sprint()
    "Select a sprint."
    (interactive)
    (setq org-jira-selected-sprint (org-jira-sprint-completing-read (org-jira-get-board-id))))

  ;;
  ;; Epics
  ;;
  (defun org-jira-get-epics-by-board(board-id &optional filter)
    "List all epics by BOARD-ID."
    (interactive)
    (let ((board-epics-cache (cdr (assoc board-id org-jira-epic-by-board-cache))))
      (unless board-epics-cache
        (setq board-epics-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/epic" board-id)'values)))

      (add-to-list 'org-jira-epic-by-board-cache `(,board-id . ,board-epics-cache))
      (if filter
          (seq-filter filter board-epics-cache)
        board-epics-cache)))

  (defun org-jira--active-epic-p(epic)
    "Predicate that checks if EPIC is active."
    (not (equal (assoc 'done epic) 'false)))


  (defun org-jira-epic-completing-read(board-id)
    "Select an active epic by name."
    (let* ((epics (org-jira-get-epics-by-board board-id 'org-jira--active-epic-p))
           (epic-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) epics))
           (epic-name (completing-read "Choose epic:" epic-names)))
      (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) epic-name)) epics))))

  (defun org-jira-move-issue-to-epic(issue-id epic-id)
    "Move issue with ISSUE-ID to epic with SPRINT-ID."
    (jiralib--rest-call-it (format "/rest/agile/1.0/epic/%s/issue" epic-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

  (defun org-jira-assign-current-issue-to-epic()
    "Move the selected issue to an active epic."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (board-id (cdr (assoc 'id (org-jira-get-board))))
           (epic-id (cdr (assoc 'id (org-jira-epic-completing-read board-id)))))

      (org-jira-move-issue-to-epic issue-id epic-id)))

  (defun org-jira-get-epic-id()
    "Select a epic id if one not already selected."
    (unless org-jira-selected-epic
      (setq org-jira-selected-epic (org-jira-epic-completing-read)))
    (cdr (assoc 'id org-jira-selected-epic)))

  (defun org-jira-get-epic()
    "Select a epic if one not already selected."
    (unless org-jira-selected-epic
      (setq org-jira-selected-epic (org-jira-select-epic)))
    org-jira-selected-epic)

  (defun org-jira-select-epic()
    "Select a epic."
    (interactive)
    (setq org-jira-selected-epic (org-jira-epic-completing-read (org-jira-get-board-id))))

  (defun org-jira-create-issue-with-defaults()
    "Create an issue and assign to default sprint and epic."
    (org-jira-create-issue)
    (org-jira-move-issue-to-epic)
    (org-jira-move-issue-to-sprint))

  (defun org-jira-update-issue-description()
    "Move the selected issue to an active sprint."
    (interactive)
    (let* ((issue-id (org-jira-parse-issue-id))
           (filename (buffer-file-name))
           (org-issue-description (org-trim (org-jira-get-issue-val-from-org 'description)))
           (update-fields (list (cons 'description org-issue-description))))
      (message "Updating issue:%s from file: %s with description:%s" issue-id filename org-issue-description)
      (jiralib-update-issue issue-id update-fields
                            (org-jira-with-callback
                             (message (format "Issue '%s' updated!" issue-id))
                             (jiralib-get-issue
                              issue-id
                              (org-jira-with-callback
                               (org-jira-log "Update get issue for refresh callback hit.")
                               (-> cb-data list org-jira-get-issues))))
                            )))




  ;;
  ;; Populate caches
  (async-start (progn
                 ;;              (jiralib-get-users "SB")
                 ;;              (org-jira-list-boards)
                 ))

  (defhydra org-jira-hydra (:hint nil :exit t)
    ;; The '_' character is not displayed. This affects columns alignment.
    ;; Remove s many spaces as needed to make up for the '_' deficit.
    "
         ^Actions^           ^Issue^              ^Buffer^                         ^Defaults^ 
         ^^^^^^-----------------------------------------------------------------------------------------------
          _L_ist issues      _u_pdate issue       _R_efresh issues in buffer       Select _B_oard 
          _C_reate issue     update _c_omment                                    Select _E_pic
                           assign _s_print                                     Select _S_print
                           assign _e_print                                     Create issue with _D_efaults
                           _b_rowse issue
                           _r_efresh issue
                           _p_rogress issue
         "
    ("L" org-jira-get-issues)
    ("C" org-jira-create-issue)

    ("u" org-jira-update-issue)
    ("c" org-jira-update-comment)
    ("b" org-jira-browse-issue)
    ("s" org-jira-assign-current-issue-to-sprint)
    ("e" org-jira-assign-current-issue-to-epic)
    ("r" org-jira-refresh-issue)
    ("p" org-jira-progress-issue)

    ("R" org-jira-refresh-issues-in-buffer)

    ("B" org-jira-select-board)
    ("E" org-jira-select-epic)
    ("S" org-jira-select-sprint)
    ("D" org-jira-create-with-defaults)

    ("q" nil "quit"))

  (evil-leader/set-key "j" 'org-jira-hydra/body))
