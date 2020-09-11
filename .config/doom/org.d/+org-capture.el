;;; ~/.dotfiles/src/.config/doom/org.d/+org-capture.el -*- lexical-binding: t; -*-

(use-package! org-capture
  :commands
  (org-capture)

  :config
  (setq org-capture-templates
        `(("i" "inbox" entry
           (file+headline ,medivhok/gtd-file "Inbox")
           "* TODO [#C] %?\n:PROPERTIES:\n:Effort: 1\n:END:\n")

          ("e" "email" entry
           (file+headline ,medivhok/gtd-file "Emails")
           "* TODO [#A] Reply: %a"
           :immediate-finish t)

          ("w" "Weekly Review" entry
           (file+olp+datetree ,medivhok/agenda-reviews-file)
           (file ,medivhok/agenda-review-template-file))))

  (defun medivhok/inbox-capture ()
    "Capture a task for the inbox"
    (interactive)
    (org-capture nil "i"))

  (map! "<f4>" #'medivhok/inbox-capture))
