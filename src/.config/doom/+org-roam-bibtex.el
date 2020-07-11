;;; ~/.dotfiles/src/.config/doom/+org-roam-bibtex.el -*- lexical-binding: t; -*-

(use-package! org-roam-bibtex
  :after
  (org-roam bibtex-completion)

  :hook
  (org-roam-mode . org-roam-bibtex-mode)

  :bind
  (:map org-mode-map
   (("C-c n a" . orb-not-actions)))

  :custom
  (orb-preformat-keywords '(("citekey" . "=key=")
                            "title"
                            "url"
                            "file"
                            "author-or-editor"
                            "keywords"))
  (orb-templates '(("r" "ref" plain (function org-roam-capture--get-point)
                    ""
                    :file-name "${citekey}"
                    :head
                    "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* Notes
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:END:\n\n"
                    :unnarrowed t))))
