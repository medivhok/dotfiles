;;; my/notes/config.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :after (org)

  :commands
  (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :config
  (setq org-roam-directory (concat org-directory "roam/")
        org-roam-title-sources '(title alias)
        org-roam-tag-sources '(first-directory prop)
        org-roam-graph-exclude-matcher '("setup")
        org-roam-index-file "index_file.org")

  (setq org-noter-notes-window-location 'vertical-split
        org-noter-always-create-frame nil
        org-noter-hide-other nil
        org-noter-doc-split-fraction '(0.5 . 0.75)
        org-noter-notes-search-path (list org-roam-directory))

  (setq org-roam-capture-templates
        `(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("b" "blog" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+DATE: %t\n#+SETUPFILE: ./blog-setup.org\n#+HUGO_TAGS:\n#+ROAM_TAGS: blog\n\n* ${title}\n"
           :unnarrowed t)
          ("s" "synapses" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"

           :head
           "#+TITLE: ${title}
#+SETUPFILE: ./synapses-setup.org
#+FILETAGS:
#+ROAM_TAGS

* ${title}
:PROPERTIES:
:ID: %(replace-regexp-in-string \"\n$\" \"\"
              (shell-command-to-string \"uuidgen\"))
:EXPORT_FILE_NAME: ${slug}
:EXPORT_DATE: %t
:END:\n"

           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))

  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
           :unnarrowed t)))

  (use-package! bibtex-completion
    :config
    (setq bibtex-completion-notes-path org-roam-directory
          bibtex-completion-bibliography reftex-default-bibliography)))

;;
;; Connector between Org-roam, BibTeX-completion, and Org-ref
;;
;; https://github.com/org-roam/org-roam-bibtex
;;
(use-package! org-roam-bibtex
  :after
  (org-roam)

  :hook
  (org-roam-mode . org-roam-bibtex-mode)

  :config
  (setq orb-preformat-keywords '(("citekey" . "=key=")
                                 "title"
                                 "url"
                                 "file"
                                 "author-or-editor"
                                 "keywords")
        orb-templates `(("r" "ref" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
                                  "#+ROAM_TAGS: ${keywords}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":AUTHOR: ${author-or-editor}\n"
                                  ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
                                  ":END:\n\n")
                         :unnarrowed t)

                        ("w" "webpage" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${url}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":END:\n\n")
                         :unnarrowed t))))
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
        org-ref-notes-directory org-roam-directory
        org-ref-notes-function 'orb-edit-notes))

;;
;; Search and manage bibliographies in Emacs.
;;
;; https://github.com/tmalsburg/helm-bibtex
;;
(after! helm-bibtex
  (map! :desc "helm-bibtex" "<f3>" #'helm-bibtex))

(use-package! citeproc-org
  :after
  (org)

  :config
  (citeproc-org-setup))

(use-package! org-pdf-tools
  :hook (org-load . org-pdftools-setup-link))

