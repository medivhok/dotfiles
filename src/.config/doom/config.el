;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; test
;; (setq doom-font (font-spec :family "monospace" :size 14)
;;      doom-variable-pitch-font (font-spec :family "sans"))

(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package! el-patch
  :config
  (eval-when-compile
    (require 'el-patch)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package! dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-show-shortcuts nil)
  (dashboard-set-navigator t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (show-week-agenda-p t)
  (dashboard-items '((recents . 5)
		                 (projects . 3)
		                 (agenda . 5)))

  :config
  ;; (defun medivhok/dashboard-insert-custom (list-size)
  ;;   "Add the list of LIST-SIZE items of menu."
  ;;   (dashboard-insert-section
  ;;    "Menu:"
  ;;    '("Budget" "Org")
  ;;    2
  ;;    "l"
  ;;    `(lambda (&rest ignore))))
  ;; (add-to-list 'dashboard-item-generators '(custom . medivhok/dashboard-insert-custom))
  ;; (add-to-list 'dashboard-items '(custom) t)
  (dashboard-setup-startup-hook))

;; Display lambda as symbols
(global-prettify-symbols-mode 1)

(defun medivhok/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en") "fr" "en")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'medivhok/switch-dictionary)
(setq ispell-dictionary "fr")

;;-----------------------------------------------------------------------------
;;
;; Ledger configuration
;;
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

;; -----------------------------------------------------------------------------
;;
;; My c/c++ programming style.
;;
(defconst medivhok-c-style
  '((c-offsets-alist . ((access-label . /)
                        (innamespace . [0])
                        (member-init-intro . *)))) "Medivhok's programming style")

(defun medivhok/c-mode-common-hook ()
  (c-add-style "Medivhok" medivhok-c-style t)
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c-mode-common-hook 'medivhok/c-mode-common-hook)

(use-package! flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  :custom
  (flycheck-clang-tidy-extra-options "-checks=-cppcoreguidelines"))

;; -----------------------------------------------------------------------------
;;
;; Email configurations.
;;
(use-package! notmuch
  :commands (notmuch)
  :init
  (setq +notmuch-sync-backend nil)
  (defun +notmuch/toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))
  :config
  (setq message-auto-save-directory "~/.mail/drafts/"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t
        message-sendmail-f-is-evil nil
        message-kill-buffer-on-exit t
        notmuch-always-prompt-for-sender t
        notmuch-crypto-process-mime t
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-labeler-hide-known-labels t
        notmuch-search-oldest-first nil
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-message-headers '("To" "Cc" "Subject" "Bcc")
        notmuch-saved-searches '((:name "unread" :query "tag:inbox and tag:unread")
                                 (:name "org-roam" :query "tag:inbox and tag:roam")
                                 (:name "personal" :query "tag:inbox and tag:personal")
                                 (:name "drafts" :query "tag:draft"))))

;; -----------------------------------------------------------------------------
;;
;; Org configurations.
;;
(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package! easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package! emr
  :bind (:map prog-mode-map
          (("M-RET" . emr-show-refactor-menu))))
