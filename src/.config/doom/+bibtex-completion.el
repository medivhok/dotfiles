;;; ~/.dotfiles/src/.config/doom/+bibtex-completion.el -*- lexical-binding: t; -*-

;;
;; Search and manage bibliographies in Emacs.
;;
;;   https://github.com/tmalsburg/helm-bibtex
;;
(use-package! bibtex-completion
  :custom
  (bibtex-completion-notes-path medivhok/roam-directory)
  (bibtex-completion-bibliography medivhok/bibliography-file)
  (bibtex-completion-library-path medivhok/pdfs-directory)
  (bibtex-completion-pdf-field "file"))
  ;; (bibtex-completion-notes-template-multiple-files
  ;;  (concat "#+TITLE: ${title}\n"
  ;;          "#+ROAM_KEY: cite:${=key=}\n"
  ;;          "* TODO Notes\n"
  ;;          ":PROPERTIES:\n"
  ;;          ":Custom_ID: ${=key=}\n"
  ;;          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ;;          ":AUTHOR: ${author-abbrev}\n"
  ;;          ":JOURNAL: ${journaltitle}\n"
  ;;          ":DATE: ${date}\n"
  ;;          ":YEAR: ${year}\n"
  ;;          ":DOI: ${doi}\n"
  ;;          ":URL: ${url}\n"
  ;;          ":END:\n\n")))
