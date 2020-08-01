;;; ~/.dotfiles/src/.config/doom/bibtex.d/+org-roam-bibtex.el -*- lexical-binding: t; -*-

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
                         ,(concat "#+TITLE: ${citekey}: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n\n"
                                  "- tags ::\n"
                                  "- keywords :: ${keywords}\n\n"
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
                                  "- tags ::\n"
                                  "- keywords :: ${keywords}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":URL: ${url}\n"
                                  ":END:\n\n")
                         :unnarrowed t))))
