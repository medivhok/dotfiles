;;; ~/.dotfiles/src/.config/doom/+org-agenda.el -*- lexical-binding: t; -*-

;;
;; org-agenda
;;
(use-package! org-agenda
  :after org

  :custom
  (org-agenda-diary-file medivhok/diary-file)
  (org-agenda-block-separator nil)
  (org-agenda-start-with-log-mode t)
  (org-agenda-custom-commands
   `((" " "Agenda"
      ((agenda ""
               ((org-agenda-span 'week)
                (org-deadline-warning-days 14)))
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile")
              (org-agenda-files (list medivhok/gtd-inbox-file))))
       (todo "TODO"
             ((org-agenda-overriding-header "Emails")
              (org-agenda-files (list medivhok/gtd-emails-file))))
       (todo "NEXT"
             ((org-agenda-overriding-header "In Progress")
              (org-agenda-files (list medivhok/gtd-next-file
                                      medivhok/gtd-projects-file
                                      medivhok/gtd-someday-file))))
       (todo "TODO"
             ((org-agenda-overriding-header "Projects")
              (org-agenda-files (list medivhok/gtd-projects-file))))
       (todo "TODO"
             ((org-agenda-overriding-header "Teluq")
              (org-agenda-files (list medivhok/gtd-teluq-file))))
       (todo "TODO"
             ((org-agenda-overriding-header "One-off Tasks")
              (org-agenda-files (list medivhok/gtd-next-file))
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))))

  :config
  (defvar medivhok/org-current-effort "1:00"
    "Current effort for agenda items.")

  (defun medivhok/my-org-agenda-set-effort (effort)
    "Set the effort property for the current headline."
    (interactive
     (list (read-string (format "Effort [%s]: " medivhok/org-current-effort) nil nil medivhok/org-current-effort)))
    (setq medivhok/org-current-effort effort)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (funcall-interactively 'org-set-effort nil medivhok/org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (defun medivhok/org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'medivhok/my-org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defvar medivhok/org-agenda-bulk-process-key ?f
    "Default key for bulk processing inbox items.")

  (setq org-agenda-bulk-custom-functions `((,medivhok/org-agenda-bulk-process-key medivhok/org-agenda-process-inbox-item)))

  (defun medivhok/bulk-process-entries ()
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region) (funcall 'medivhok/org-agenda-process-inbox-item))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

  (defun medivhok/org-process-inbox ()
    "Called in org-agenda-mode, processes all inbox items."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (medivhok/bulk-process-entries))

  (defun medivhok/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (map! :map org-agenda-mode-map
        "i" #'org-agenda-clock-in
        "r" #'medivhok/org-process-inbox
        "R" #'org-agenda-refile
        "c" #'medivhok/org-inbox-capture)

  (setq org-columns-default-format
        "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  )
