;;; ~/.dotfiles/src/.config/doom/org.d/roam.d/+org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :after (org)

  :commands
  (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :config
  (setq org-roam-directory medivhok/roam-directory
        org-roam-title-sources '(title)
        org-roam-tag-sources '(prop)
        org-roam-graph-exclude-matcher '("setup")
        org-roam-index-file "index_file.org")

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
           :unnarrowed t))))
