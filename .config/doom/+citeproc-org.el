;;; ~/.dotfiles/src/.config/doom/+citeprog-org.el -*- lexical-binding: t; -*-

(use-package! citeproc-org
  :after
  (org)

  :config
  (citeproc-org-setup))
