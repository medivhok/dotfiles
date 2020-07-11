;;; app/gtd/config.el -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------
;; org
;;
(use-package! org
  :config
  (defun medivhok/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries 'org-archive-subtree "/DONE" 'file))

  (defun medivhok/set-todo-state-next ()
    "Visit each parent task and change NEXT states to TODO"
    (org-todo "NEXT"))

  (add-hook 'org-clock-in-hook 'medivhok/set-todo-state-next 'append))

;; -----------------------------------------------------------------------------
;; org-agenda
;;
(use-package! org-agenda
  :after org

  :custom
  (org-agenda-diary-file (concat medivhok/org-directory "diary.org"))
  (org-agenda-block-separator nil)
  (org-agenda-start-with-log-mode t)
  (org-agenda-custom-commands
   `((" " "Agenda"
      ((agenda ""
               ((org-agenda-span 'week)
                (org-deadline-warning-days 14)))
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile")
              (org-agenda-files '(,(concat medivhok/org-agenda-directory "inbox.org")))))
       (todo "TODO"
             ((org-agenda-overriding-header "Emails")
              (org-agenda-files '(,(concat medivhok/org-agenda-directory "emails.org")))))
       (todo "NEXT"
             ((org-agenda-overriding-header "In Progress")
              (org-agenda-files '(,(concat medivhok/org-agenda-directory "someday.org")
                                  ,(concat medivhok/org-agenda-directory "projects.org")
                                  ,(concat medivhok/org-agenda-directory "next.org")))
              ))
       (todo "TODO"
             ((org-agenda-overriding-header "Projects")
              (org-agenda-files '(,(concat medivhok/org-agenda-directory "projects.org")))
              ))
       (todo "TODO"
             ((org-agenda-overriding-header "One-off Tasks")
              (org-agenda-files '(,(concat medivhok/org-agenda-directory "next.org")))
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
        "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"))

;; -----------------------------------------------------------------------------
;; org-clock-convenience
;;
(use-package! org-clock-convenience
  :after org-agenda

  :bind (:map org-agenda-mode-map
          ("<S-up>" . org-clock-convenience-timestamp-up)
          ("<S-down>" . org-clock-convenience-timestamp-down)
          ("o" . org-clock-convenience-fill-gap)
          ("e" . org-clock-convenience-fill-gap-both)))

;; -----------------------------------------------------------------------------
;; org-journal
;;
(use-package! org-journal
  :after org-roam

  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir org-roam-directory)
  (org-journal-date-format "%A, %d %B %Y"))

;; -----------------------------------------------------------------------------
;; org-capture
;;
(use-package! org-capture
  :after (org-protocol org-agenda)

  :custom
  (org-capture-templates
   `(("i" "inbox" entry (file ,(concat medivhok/org-agenda-directory "inbox.org"))
      "* TODO %?")
     ("e" "email" entry (file+headline ,(concat medivhok/org-agenda-directory "emails.org") "Emails")
      "* TODO [#A] Reply: %a :@home:@school:"
      :immediate-finish t)
     ("c" "org-protocol-capture" entry (file ,(concat medivhok/org-agenda-directory "inbox.org"))
      "* TODO [[%:link][%:description]]\n\n %i"
      :immediate-finish t)
     ("w" "Weekly Review" entry (file+olp+datetree ,(concat medivhok/org-agenda-directory "reviews.org"))
      (file ,(concat medivhok/org-agenda-directory "templates/weekly_review.org")))
     ("r" "Reading" todo ""
      ((org-agenda-files '(,(concat medivhok/org-agenda-directory "reading.org")))))))
  )

;; -----------------------------------------------------------------------------
;; ol-notmuch
;;
(use-package! ol-notmuch
  :after (notmuch org-capture)

  :init
  (map! :map notmuch-show-mode-map "C" #'medivhok/org-capture-email)
  (defun medivhok/org-capture-email ()
    (interactive)
    (org-capture nil "e")))

;; -----------------------------------------------------------------------------
;; deft
;;
(use-package! deft
  :after org-roam

  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; (use-package! bibtex-completion
;;   :custom
;;   (bibtex-completion-bibliography '("~/Documents/org/roam/bibliography/references.bib")))

(use-package! ox-latex
  :custom
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; -----------------------------------------------------------------------------
;; ox-icalendar
;;
(use-package! ox-icalendar
  :custom
  (org-icalendar-timezone "America/Montreal")
  (org-icalendar-store-UID t)
  (org-icalendar-include-todo 'all)
  (org-icalendar-use-scheduled '(event-if-todo-not-done event-if-not-todo todo-start))
  (org-icalendar-use-deadline '(event-if-todo-not-done event-if-not-todo todo-due)))

;; -----------------------------------------------------------------------------
;; org-caldav setup
;;
;; site: https://github.com/dengste/org-caldav
;; -----------------------------------------------------------------------------
(use-package! org-caldav
  :init
  (setq org-caldav-sync-todo t)

  :custom
  (org-caldav-save-directory (concat medivhok/org-agenda-directory "caldav-sync/"))
  (org-caldav-url "https://medivhok.hopto.org/remote.php/dav/calendars/medivhok")
  (org-caldav-calendars
   `((:calendar-id "projects"
      :files (,medivhok/agenda-file-projects)
      :inbox ,medivhok/agenda-file-inbox)
     (:calendar-id "teluq"
      :files (,medivhok/agenda-file-teluq)
      :inbox ,medivhok/agenda-file-inbox))))
