;;; ~/.dotfiles/src/.config/doom/+org.el -*- lexical-binding: t; -*-

;;
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.
;;
;;   https://orgmode.org
;;
(use-package! org
  :custom
  (org-directory medivhok/org-directory)
  (org-agenda-files medivhok/agenda-files)
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
  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "exports")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified))
