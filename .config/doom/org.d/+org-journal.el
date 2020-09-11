;;; ~/.dotfiles/src/.config/doom/org.d/+org-journal.el -*- lexical-binding: t; -*-

(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-dir (concat org-directory "journal/")
        org-journal-carryover-items nil))
