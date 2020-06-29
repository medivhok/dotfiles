;;; app/gtd/config.el -*- lexical-binding: t; -*-

(setq medivhok/org-directory "~/Documents/org/")
(setq medivhok/org-agenda-directory (concat medivhok/org-directory "agenda/"))
(setq reftex-default-bibliography '("~/Documents/org/roam/bibliography/references.bib"))

(defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
  (unless pub-dir
    (setq pub-dir "exports")
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))
(advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

;; -----------------------------------------------------------------------------
;; org
;;
(use-package! org
  :mode ("\\.org\\'" . org-mode)

  :custom
  (org-directory medivhok/org-directory)
  (org-agenda-files (list medivhok/org-agenda-directory))
  (org-return-follows-link t)
  (org-catch-invisible-edits 'show)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (org-tag-alist '(("@commission" . ?e)
                   ("@maison" . ?h)
                   ("@teluq" . ?s)
                   (:newline)
                   ("WAITING" . ?w)
                   ("HOLD" . ?H)
                   ("CANCELLED" . ?c)))

  (org-refile-targets '(("next.org" :level . 0)
                        ("someday.org" :level . 0)
                        ("reading.org" :level . 1)
                        ("projects.org" :maxlevel . 1)))

  :custom-face
  (org-link ((t (:inherit link :underline nil))))

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
;; org-roam
;;
(use-package! org-roam
  :after org

  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-graph-exclude-matcher "private")

  :custom
  (org-roam-directory (concat medivhok/org-directory "roam"))
  (org-roam-tag-sources '(prop))

  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("m" "mathématique" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head
           "#+SETUPFILE: ./setup-files/math_setup.org\n#+TITLE: ${title}\n#+ROAM_TAGS: mathématique\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
           :unnarrowed t))))

;; -----------------------------------------------------------------------------
;; company-org-roam
;;
(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

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
;; org-protocol
;;
(use-package! org-protocol)

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
