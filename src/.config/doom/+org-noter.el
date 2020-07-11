;;; ~/.dotfiles/src/.config/doom/+org-noter.el -*- lexical-binding: t; -*-

(use-package! org-noter
  :after
  (org-pdftools)

  :custom
  ;;(org-noter-notes-window-location 'other-frame)
  (org-noter-always-create-frame nil)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list medivhok/roam-directory)))
