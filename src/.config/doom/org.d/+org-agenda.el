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
  (setq org-agenda-files (list medivhok/agenda-directory)
        org-agenda-block-separator nil
        org-agenda-start-with-log-mode t
        org-agenda-dim-blocked-tasks 'invisible

        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t

        org-refile-targets `((,medivhok/agenda-next-file :level . 0)
                             (,medivhok/agenda-someday-file :level . 0)
                             (,medivhok/agenda-reading-file :level . 1)
                             (,medivhok/agenda-projects-file :maxlevel . 1))

        org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-deadline-warning-days 14)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-files (list medivhok/agenda-inbox-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Emails")
                   (org-agenda-files (list medivhok/agenda-emails-file))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files (list medivhok/agenda-next-file
                                           medivhok/agenda-projects-file
                                           medivhok/agenda-someday-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files (list medivhok/agenda-projects-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Teluq")
                   (org-agenda-files (list medivhok/agenda-teluq-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files (list medivhok/agenda-next-file))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))
