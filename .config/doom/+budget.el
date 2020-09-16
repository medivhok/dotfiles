;;; +budget.el -*- lexical-binding: t; -*-

(setq medivhok/main-ledger-file "~/Documents/Budget/ledger/main.ledger")

(defun medivhok/ledger-command ()
  "Returns the ledger command with the main ledger file."
  (interactive)
  (concat "%(binary) -f " medivhok/main-ledger-file))

(defun medivhok/ledger-budget-balance-command ()
  "Returns the ledger command for the budget balance report."
  (interactive)
  (concat (medivhok/ledger-command) " -c bal ^Budget"))

(defun medivhok/ledger-budget-forcasting-command ()
  "Returns the ledger command to the budget register for 60 days."
  (interactive)
  (concat (medivhok/ledger-command)
          " -b "
          (format-time-string "%Y/%m/%d")
          " -e "
          (format-time-string "%Y/%m/%d" (time-add (current-time) (* 60 60 24 60)))
          " reg ^Budget"))

(after! evil-ledger
  (defun medivhok/ledger-report ()
    "Wrapper around LEDGER-REPORT to redefine LEDGER-REPORTS with current dates."
    (interactive)
    (if (not (get-buffer ledger-report-buffer-name))
        (let ((report-window (split-window-right))
              (report-buffer (generate-new-buffer ledger-report-buffer-name)))
          (set-window-buffer report-window report-buffer)))
    (custom-set-variables
     '(ledger-reports `(("budget balance" ,(medivhok/ledger-budget-balance-command))
                        ("budget forcasting" ,(medivhok/ledger-budget-forcasting-command))
                        ("register" "%(binary) -f %(ledger-file) reg ^assets")
                        ("current balance" "%(binary) -f ~/Documents/Budget/ledger/main.ledger -c bal"))))
    (call-interactively 'ledger-report))

  (map! :localleader
        :map ledger-mode-map
        "b" #'(lambda ()
                (interactive)
                (medivhok/ledger-report "current balance"))))
