;;; app/gtd/config.el -*- lexical-binding: t; -*-

(setq org-directory "~/Documents/org/")
(setq medivhok/gtd-directory (concat org-directory "gtd/"))
(setq org-agenda-files (list medivhok/gtd-directory))
(setq org-agenda-diary-file (concat org-directory "diary.org"))
;; (setq org-agenda-custom-commands (list))

;; ---------------------------------------------------------------------------
(defvar medivhok/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

;; -----------------------------------------------------------------------------
(defvar medivhok/org-current-effort "1:00"
  "Current effort for agenda items.")

;; -----------------------------------------------------------------------------
(defun medivhok/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;; -----------------------------------------------------------------------------
(defun medivhok/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (medivhok/bulk-process-entries))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(defun medivhok/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'medivhok/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(defun medivhok/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

;; -----------------------------------------------------------------------------
(setq org-agenda-bulk-custom-functions `((,medivhok/org-agenda-bulk-process-key medivhok/org-agenda-process-inbox-item)))

;; -----------------------------------------------------------------------------
(defun medivhok/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

;; -----------------------------------------------------------------------------
(defun medivhok/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
                (not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

;; -----------------------------------------------------------------------------
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

;; -----------------------------------------------------------------------------
(defun medivhok/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

;; -----------------------------------------------------------------------------
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)
  (setq org-fast-tag-selection-single-key nil)
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 0)
                             ("reading.org" :level . 1)
                             ("projects.org" :maxlevel . 1))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-agenda-block-separator nil)
  (setq org-agenda-start-with-log-mode t)

  (setq org-tag-alist '(("@errand" . ?e)
                        ("@home" . ?h)
                        ("@school" . ?s)
                        (:newline)
                        ("WAITING" . ?w)
                        ("HOLD" . ?H)
                        ("CANCELLED" . ?c)))

  (require 'ol-notmuch)

  (add-hook 'org-clock-in-hook 'medivhok/set-todo-state-next 'append)

  ;; ---------------------------------------------------------------------------
  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 14)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files `(,(concat medivhok/gtd-directory "inbox.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Emails")
                   (org-agenda-files `(,(concat medivhok/gtd-directory "emails.org")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files `(,(concat medivhok/gtd-directory "someday.org")
                                       ,(concat medivhok/gtd-directory "projects.org")
                                       ,(concat medivhok/gtd-directory "next.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files `(,(concat medivhok/gtd-directory "projects.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files `(,(concat medivhok/gtd-directory "next.org")))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
            nil))

          ("r" "Reading" todo ""
           ((org-agenda-files `(,(concat medivhok/gtd-directory "reading.org")))))))

  ;; ---------------------------------------------------------------------------
  (setq org-capture-templates
        `(("i" "inbox" entry (file ,(concat medivhok/gtd-directory "inbox.org")) "* TODO %?")

          ("e" "email" entry (file+headline ,(concat medivhok/gtd-directory
                                                   "emails.org") "Emails") "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
          ("l" "link" entry (file ,(concat medivhok/gtd-directory "inbox.org")) "* TODO %(org-cliplink-capture)" :immediate-finish t)

          ("c" "org-protocol-capture" entry (file ,(concat medivhok/gtd-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)

          ("w" "Weekly Review" entry (file+olp+datetree ,(concat medivhok/gtd-directory "reviews.org"))
           (file ,(concat medivhok/gtd-directory "templates/weekly_review.org"))))))

(use-package! org-clock-convenience
  :after org-agenda
  :bind (:map org-agenda-mode-map
          ("<S-up>" . org-clock-convenience-timestamp-up)
          ("<S-down>" . org-clock-convenience-timestamp-down)
          ("o" . org-clock-convenience-fill-gap)
          ("e" . org-clock-convenience-fill-gap-both)))

;; -----------------------------------------------------------------------------
(use-package! org-roam
  ;;
  ;; https://github.com/jethrokuan/org-roam
  ;;
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Documents/org/roam/")
  :config
  (map!
   :leader
   (:map org-roam-mode-map
     (:desc "Org roam panel" "np" #'org-roam)
     ;;"rt" #'org-roam-today
     "nf" nil
     (:desc "Find note file" "nf" #'org-roam-find-file)
     (:desc "Show roam graph" "ng" #'org-roam-show-graph))
   (:map org-mode-map
    (:desc "Insert link" "ni" #'org-roam-insert))))

(map!
 :after evil-org-agenda
 :map evil-org-agenda-mode-map
 :m "i" nil)

(map!
 :after evil-org-agenda
 :map evil-org-agenda-mode-map
 :m
  "r" 'medivhok/org-process-inbox
  "R" 'org-agenda-refile
  "i" 'medivhok/org-inbox-capture)
