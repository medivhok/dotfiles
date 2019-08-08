;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(set-email-account! "gregory.verret@gmail.com"
                    '((mu4e-sent-folder . "/gregory.verret@gmail.com/[Gmail]/Sent Mail")
                      (mu4e-maildir . "~/.mail")
                      (mu4e-drafts-folder . "/gregory.verret@gmail.com/[Gmail]/Drafts")
                      (mu4e-trash-folder . "/gregory.verret@gmail.com/[Gmail]/Trash")
                      (mu4e-refile-folder . "/gregory.verret@gmail.com/[Gmail]/All Mail")
                      (smtpmail-smtp-user . "gregory.verret@gmail.com")
                      (user-mail-address . "gregory.verret@gmail.com")
                      ))

(after! org
  (setq org-directory "~/pCloudDrive/My Agenda")
  (setq org-agenda-files (list org-directory)))

;; Place your private configuration here
