;;; ../../.dotfiles/src/.config/doom/org.d/+org-refile.el -*- lexical-binding: t; -*-

(use-package! org-refile
  :after
  (org)

  :custom
  (org-refile-targets '((nil :tag . "@tâches")
                        (nil :tag . "@cours")
                        (nil :tag . "@projet"))))
