;;; ~/.dotfiles/src/.config/doom/org.d/+org-agenda.el -*- lexical-binding: t; -*-

(use-package! org-agenda
  :after
  (org)

  :config
  (defun medivhok/open-agenda ()
    "Opens my gtd agenda."
    (interactive)
    (org-agenda nil " "))

  (map! "<f1>" #'medivhok/open-agenda)

  ;; org-agenda variables.
  (setq org-agenda-files (list medivhok/org-agenda-gtd-file)
        org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-dim-blocked-tasks 'invisible
        org-use-tag-inheritance t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t

        org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-deadline-warning-days 14)))
            (tags-todo "@inbox"
                  ((org-agenda-overriding-header "Inbox")))
            (tags-todo "@tâches"
                  ((org-agenda-overriding-header "Tâches")))
            (tags-todo "@teluq"
                  ((org-agenda-overriding-header "Teluq")))
            (tags "@cours"
                  ((org-agenda-overriding-header "Avancement des cours")
                   (org-use-tag-inheritance nil)))
            (tags-todo "@projets"
                       ((org-agenda-overriding-header "Projets")))))))

  (defvar medivhok/org-current-effort "1:00"
    "Current effort for agenda items.")

  (defun)

  (defun medivhok/org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'medivhok/org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

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
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (org-agenda-redo)
                (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
                (message "Acted on %d entries%s%s"
                         processed
                         (if (= skipped 0)
                             ""
                           (format ", skipped %d (disappeared before their turn)"
                                   skipped))
                         (if (not org-agenda-persistent-marks) "" " (kept marked)")))))))))
