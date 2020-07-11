;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;; Global settings
;;
(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com"
      doom-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font")
      doom-theme 'doom-one
      display-line-numbers-type 'relative)

;; Directories
(setq medivhok/org-directory "~/org/"
      medivhok/agenda-directory (concat medivhok/org-directory "agenda/")
      medivhok/roam-directory (concat medivhok/org-directory "roam/")
      medivhok/bibliography-directory (concat medivhok/org-directory "bibliography/")
      medivhok/pdfs-directory "~/pCloudDrive/books/")

;; Files
(setq medivhok/diary-file (concat medivhok/org-directory "diary.org")
      medivhok/gtd-emails-file (concat medivhok/agenda-directory "emails.org")
      medivhok/gtd-inbox-file (concat medivhok/agenda-directory "inbox.org")
      medivhok/gtd-next-file (concat medivhok/agenda-directory "next.org")
      medivhok/gtd-projects-file (concat medivhok/agenda-directory "projects.org")
      medivhok/gtd-someday-file (concat medivhok/agenda-directory "someday.org")
      medivhok/gtd-teluq-file (concat medivhok/agenda-directory "teluq.org")
      medivhok/agenda-files (list medivhok/gtd-emails-file
                                  medivhok/gtd-inbox-file
                                  medivhok/gtd-next-file
                                  medivhok/gtd-projects-file
                                  medivhok/gtd-someday-file
                                  medivhok/gtd-teluq-file)

      ;; Bibliography files
      medivhok/bibliography-file (concat medivhok/bibliography-directory "zotero.bib")
      medivhok/bibliography-notes-file (concat medivhok/roam-directory "bibnotes.org")
      reftex-default-bibliography (list medivhok/bibliography-file))

;; Display lambda as symbols
(global-prettify-symbols-mode 1)

;;(load! "+dashboard")
;;(load! "+ledger")
(load! "+bibtex-completion")
(load! "+notmuch")
(load! "+org")
(load! "+org-agenda")
(load! "+org-noter")
(load! "+org-ref")
(load! "+org-roam")
(load! "+org-roam-bibtex")


(defun medivhok/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en") "fr" "en")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'medivhok/switch-dictionary)
(setq ispell-dictionary "fr")

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

