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

;; Custom global variables
(setq medivhok/root-directory "~/org/"

      ;; Agenda directory and files.
      medivhok/agenda-directory (concat medivhok/root-directory "agenda/")
      medivhok/org-agenda-gtd-file (concat medivhok/agenda-directory "gtd.org")
      medivhok/agenda-emails-file (concat medivhok/agenda-directory "emails.org")
      medivhok/agenda-inbox-file (concat medivhok/agenda-directory "inbox.org")
      medivhok/agenda-tasks-file (concat medivhok/agenda-directory "taches.org")
      medivhok/agenda-projects-file (concat medivhok/agenda-directory "projects.org")
      medivhok/agenda-reading-file (concat medivhok/agenda-directory "reading.org")
      medivhok/agenda-reviews-file (concat medivhok/agenda-directory "reviews.org")
      medivhok/agenda-review-template-file (concat medivhok/agenda-directory "templates/weekly_review.org")
      medivhok/agenda-someday-file (concat medivhok/agenda-directory "someday.org")
      medivhok/agenda-teluq-file (concat medivhok/agenda-directory "teluq.org")

      ;; Bibtex directory and files.
      medivhok/bibtex-directory (concat medivhok/root-directory "bibtex/")
      medivhok/bibtex-file (concat medivhok/bibtex-directory "zotero.bib")
      reftex-default-bibliography (list medivhok/bibtex-file)

      ;; Roam directory and files.
      medivhok/roam-directory (concat medivhok/root-directory "roam/"))

;; Display lambda as symbols
(global-prettify-symbols-mode 1)

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(load! "bibtex.d/+config")
(load! "latex.d/+config")
(load! "mail.d/+config")
(load! "org.d/+config")
(load! "pdf.d/+config")

;;
;; Dictionnary setup
;;
(setq ispell-dictionary "fr")

(defun medivhok/switch-dictionary()
  "Switch between different dictionnary languages"
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "en") "fr" "en")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>") 'medivhok/switch-dictionary)


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((org-tag-alist quote
                    (("@statistiques" "@mathématiques" "@programmation")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-link ((t (:inherit link :underline nil)))))
