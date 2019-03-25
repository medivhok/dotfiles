;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(def-package! org
  :config
  (setq diary-file "~/pCloudDrive/My Documents/Agenda/diary"
        view-diary-entries-initialy t
        calendar-holidays holiday-local-holidays
        org-agenda-files (list "~/pCloudDrive/My Documents/Agenda/Intelia.org"
                               "~/pCloudDrive/My Documents/Agenda/Personal.org")))

(def-package! excorporate
  :config
  (setq-default excorporate-configuration '("gregory.verret@intelia.com" . "https://outlook.office365.com/EWS/Exchange.asmx")
                org-agenda-include-diary t)
  (excorporate)
  (excorporate-diary-enable)

  (defun ab/agenda-update-diary ()
    "call excorporate to update the diary for today"
    (exco-diary-diary-advice (calendar-current-date) (calendar-current-date) #'message "diary updated"))

  (add-hook 'org-agenda-cleanup-fancy-diary-hook 'ab/agenda-update-diary))

;; Place your private configuration here
