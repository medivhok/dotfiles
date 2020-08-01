;;; ~/.dotfiles/src/.config/doom/org.d/+org-ref.el -*- lexical-binding: t; -*-

;;
;; citations, cross-references, indexes, glossaries and bibtex utilities for
;; org-mode
;;
;; https://github.com/jkitchin/org-ref
;;
(use-package! org-ref
  :after
  (org-roam-bibtex)

  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-filename-helm-bibtex
        org-ref-default-bibliography bibtex-completion-bibliography
        ;;org-ref-bibliography-notes medivhok/bibliography-notes-file
        org-ref-note-title-format (concat "* TODO %y - %t\n"
                                     "  :PROPERTIES:\n"
                                     "  :Custom_ID: %k\n"
                                     "  :NOTER_DOCUMENT: %F\n"
                                     "  :ROAM_KEY: cite:%k\n"
                                     "  :JOURNAL: %j\n"
                                     "  :YEAR: %y\n"
                                     "  :VOLUME: %v\n"
                                     "  :PAGES: %p\n"
                                     "  :DOI: %D\n"
                                     "  :URL: %U\n"
                                     "  :END:\n\n")
        org-ref-notes-directory medivhok/roam-directory
        org-ref-notes-function 'orb-edit-notes))
