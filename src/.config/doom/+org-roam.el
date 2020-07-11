;;; config/gtd/+org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :after (org)

  :commands
  (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :custom
  (org-roam-directory (concat org-directory "roam/"))
  (org-roam-title-sources '(title))
  (org-roam-tag-sources '(prop))
  (org-roam-graph-exclude-matcher '("setup"))

  :config
  ;;(require 'org-roam-protocol)
  (setq org-roam-capture-templates
        `(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
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

  (defun medivhok/add-teluq-course (course-name)
    "Create a subdirectory structure in the teluq roam directory."
    (interactive "sSigle du cours : ")
    (if (string-match-p "^\\w\\{3\\}[0-9]\\{4\\}$" course-name)
        (let ((course-directory (concat org-roam-directory
                                        "teluq/"
                                        (downcase course-name)
                                        "/")))
          (make-directory (concat course-directory "travaux/") :parents)
          (make-directory (concat course-directory "notes/") :parents)
          (make-directory (concat course-directory "examen/") :parents)
          (make-directory (concat course-directory "references/") :parents))
      (error "Sigle de cours invalide '%s'!" course-name))))
