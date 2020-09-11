;;; ~/.dotfiles/src/.config/doom/+org-pdftools.el -*- lexical-binding: t; -*-

(use-package! org-pdf-tools
  :hook (org-load . org-pdftools-setup-link))
