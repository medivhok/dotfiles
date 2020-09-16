;;; +agenda.el -*- lexical-binding: t; -*-

(use-package! org-agenda
  :after
  (org)

  :config
  (setq medivhok/gtd-file (concat medivhok/root-directory "agenda/gtd.org"))

  (defun medivhok/open-agenda ()
    "Opens my gtd agenda."
    (interactive)
    (org-agenda nil " "))

  (map! "<f1>" #'medivhok/open-agenda)

  ;; org-agenda variables.
  (setq org-agenda-files (list medivhok/gtd-file)
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
            (tags-todo "@projets"
                       ((org-agenda-overriding-header "Projets")))))))

  (defun medivhok/gtd-process-single-inbox-item (&optional item-position)
    "Process the current heading in the agenda."
    (interactive)
    (if (not (null item-position))
        (goto-char item-position))
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (org-agenda-set-effort)
     (org-agenda-refile nil nil t)))

  (defun medivhok/gtd--process-item-at-position (item-position)
    "Process the current heading in the agenda."
    (message "Processed an inbox item...")
    (goto-char item-position)
    (org-with-wide-buffer
     (let ((org-refile-targets '((nil . (:level . 1)))))
       (org-agenda-set-tags)
       (org-agenda-priority)
       (org-agenda-refile nil nil t))))

  (defun medivhok/gtd--get-item-position (item-mark)
    ""
    (text-property-any (point-min) (point-max) 'org-hd-marker item-mark))

  (defun medivhok/gtd--process-items (items-marks)
    ""
    (dolist (item-mark items-marks)
      (let ((item-position (medivhok/gtd--get-item-position item-mark)))
        (if (null item-position)
            (message "Skipping deleted entry at %s." item-mark)
          (medivhok/gtd--process-item-at-position item-position)))))

  (defun medivhok/gtd-process-all-inbox-items ()
    "Process all reminder in the inbox."
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (if (null org-agenda-bulk-marked-entries)
        (message "The 'inbox' is empty.")
      ;; else
      (medivhok/gtd--process-items (reverse org-agenda-bulk-marked-entries))
      (org-agenda-bulk-unmark-all)
      (org-agenda-redo)))

  (use-package! org-capture
    :config
    (setq org-capture-templates
          `(("i" "inbox" entry
             (file+headline ,medivhok/gtd-file "Inbox")
             "* TODO [#C] %?\n:PROPERTIES:\n:Effort: 1\n:END:\n")

            ("e" "email" entry
             (file+headline ,medivhok/gtd-file "Emails")
             "* TODO [#A] Reply: %a"
             :immediate-finish t)))

    (defun medivhok/inbox-capture ()
      "Capture a task for the inbox"
      (interactive)
      (org-capture nil "i"))

    (map! "<f4>" #'medivhok/inbox-capture)))

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir (concat org-directory "journal/")
        org-journal-carryover-items nil))

(use-package! org-refile
  :after
  (org)

  :custom
  (org-refile-targets '((nil :tag . "@tâches")
                        (nil :tag . "@cours")
                        (nil :tag . "@projet")
                        (nil :tag . "@teluq"))))
