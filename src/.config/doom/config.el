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

(smartparens-strict-mode 1)

;; -----------------------------------------------------------------------------
;; Future-proof your Emacs Lisp customizations!
;;   https://github.com/raxod502/el-patch
;;
(use-package! el-patch
  :ensure t
  :config
  (eval-when-compile
    (require 'el-patch)))


(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; -----------------------------------------------------------------------------
;; https://github.com/emacs-dashboard/emacs-dashboard
;;
(use-package! dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-show-shortcuts nil)
  (dashboard-items '((recents . 5)
		     (projects . 3)
		     (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Display lambda as symbols
(global-prettify-symbols-mode 1)


;; Languagetool configuration
(setq langtool-bin "/usr/bin/languagetool"
      langtool-language-tool-jar "/usr/share/languagetool/lib/languagetool-commandline.jar"
      langtool-mother-tongue "fr"
      langtool-default-language "fr")



(defun medivhok/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "english") "francais" "english")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'medivhok/switch-dictionary)
;;(setq ispell-dictionary "francais")

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

;;-----------------------------------------------------------------------------
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
;; My c/c++ programming style.
;; -----------------------------------------------------------------------------
(defconst medivhok-c-style
  '((c-offsets-alist . ((access-label . /)
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

(use-package! csv-mode
  :mode "\\.csv$")

;; Org configurations.
(custom-set-variables
 '(org-directory "~/Documents/org/")
 '(org-roam-directory "~/Documents/org/roam")
 '(org-agenda-files '("~/Documents/org/gtd/"))
 '(org-agenda-diary-file "~/Documents/org/diary.org"))
