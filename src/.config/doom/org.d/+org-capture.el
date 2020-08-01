;;; ~/.dotfiles/src/.config/doom/org.d/+org-capture.el -*- lexical-binding: t; -*-

(use-package! org-capture
  :commands
  (org-capture)

  :config
  (setq org-capture-templates
        `(("i" "inbox" entry
           (file ,medivhok/agenda-inbox-file)
           "* TODO %?")

          ("e" "email" entry
           (file+headline ,medivhok/agenda-emails-file "Emails")
           "* TODO [#A] Reply: %a :@home:@school:"
           :immediate-finish t)

          ("c" "org-protocol-capture" entry
           (file ,medivhok/agenda-inbox-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)

          ("w" "Weekly Review" entry
           (file+olp+datetree ,medivhok/agenda-reviews-file)
           (file ,medivhok/agenda-review-template-file))

          ("r" "Reading" todo
           ""
           ((org-agenda-files '(,medivhok/agenda-reading-file))))))

  (defun medivhok/inbox-capture ()
    "Capture a task for the inbox"
    (org-capture nil "i"))

  (map! "<f4>" #'medivhok/inbox-capture))
