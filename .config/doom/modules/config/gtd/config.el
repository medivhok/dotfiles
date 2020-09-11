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
;; deft
;;
(use-package! deft
  :after org-roam

  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))


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
