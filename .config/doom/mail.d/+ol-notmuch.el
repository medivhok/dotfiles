;;; ~/.dotfiles/src/.config/doom/mail.d/+ol-notmuch.el -*- lexical-binding: t; -*-

(use-package! ol-notmuch
  :after
  (notmuch org)

  :config
  (map!
   :map notmuch-show-mode-map
   :localleader "C" #'medivhok/org-capture-email)

  (defun medivhok/org-capture-email ()
    "Capture an email in notmuch"
    (interactive)
    (org-capture nil "e")))
