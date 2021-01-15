(defconst medivhok:app-directory
  (file-name-as-directory "~/org")
  "The root directory of the org ecosystem.")

(defconst medivhok:agenda-directory
  (file-name-as-directory
   (expand-file-name "agenda" medivhok:app-directory))
  "The directory of my agenda files.")

(defconst medivhok:pdf-directory
  (file-name-as-directory
   (expand-file-name "readings" medivhok:app-directory))
  "The directory of my pdf files.")

(defconst medivhok:roam-directory
  (file-name-as-directory
   (expand-file-name "roam" medivhok:app-directory))
  "The directory of my roam files.")

(defconst medivhok:bibtex-file
  (expand-file-name "zotero.bib" medivhok:pdf-directory)
  "My default bibliography file.")

(use-package org
  :init
  (setq org-directory medivhok:app-directory)

  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . visual-line-mode)

  :custom-face
  (org-block ((t (:inherit fixed-pitch))))
  (org-document-title ((t (:height 1.5 :weight light))))
  (org-level-1 ((t (:inherit outline-1 :height 1.3 :weight light))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2 :weight light))))
  (org-level-3 ((t (:inherit outline-3 :height 1.1 :weight light))))
  (org-link ((t (:inherit link :underline nil :weight regular))))
  (org-meta-line ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit org-default :family "Roboto Mono"))))
  (org-table-header ((t (:inherit org-table :family "Roboto Mono"))))

  :general
  (medivhok:local-mode-menu 'normal
    org-mode-map
    nil '(:ignore t :which-key "org")
    "e" 'org-export-dispatch
    "t" '(:ignore t :which-key "toggle")
    "tl" 'org-toggle-link-display)

  :custom
  (org-fontify-quote-and-verse-blocks t)

  :config
  (setq org-agenda-files (list medivhok:agenda-directory)
        org-catch-invisible-edits 'show
        org-cycle-separator-lines 2
        org-directory medivhok:app-directory
        org-edit-src-content-indentation 0
        org-ellipsis " ▼"
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-startup-folded t)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 4.0))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (ledger . t)
     (R . t)))

  (add-hook 'org-src-mode-hook
            (lambda ()
              (when (eq major-mode 'emacs-lisp-mode)
                (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

  ;; Replace list hyphen with dot.
  (require 'org-indent)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package ob-core
  :defer t
  :straight org
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ob-tangle
  :defer t

  :straight org

  :preface
  (defun medivhok/tangle-on-save ()
    "Disable confirmation before tangling."
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

  :hook
  (org-mode . (lambda ()
                (add-hook 'after-save-hook
        	                #'medivhok/tangle-on-save
                          'run-at-end
                          'only-in-org-mode))))

(use-package org-superstar
  :after org

  :hook
  (org-mode . org-superstar-mode)

  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-make-toc
  :hook
  (org-mode . org-make-toc-mode))

(use-package ox-odt
  :after org

  :straight org

  :custom
  (org-odt-convert-process "unoconv")
  (org-odt-convert-processes '(("unoconv"
                                "unoconv -f %f %i")))
  (org-odt-preferred-output-format "docx")
  (org-odt-prettify-xml t)
  (org-odt-content-template-file (expand-file-name
                                  "OrgOdtContentTemplate.xml"
                                  (concat straight-base-dir
                                          "straight/repos/org/etc/styles/")))
  (org-odt-styles-file (expand-file-name
                        "OrgOdtStyles.xml"
                        (concat straight-base-dir
                                "straight/repos/org/etc/styles/"))))

  ;; :config
  ;; (org-odt-add-automatic-style "TNOrgTitle"
  ;;                             '(("style:family" "paragraph")
  ;;                               ("style:parent-style-name" "OrgTitle")
  ;;                               ("style:master-page-name" "OrgTitlePage"))))

(use-package evil-org
  :hook
  ((org-mode . evil-org-mode)
   (org-agenda-mode . evil-org-mode)
   (evil-org-mode . (lambda ()
	                    (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package helm-bibtex
  :after (helm org)
  :init
  (setq bibtex-completion-bibliography medivhok:bibtex-file
        bibtex-completion-notes-path medivhok:roam-directory
        bibtex-completion-pdf-field "File"))

(use-package org-ref
  :after
  (org helm-bibtex)

  :init
  (setq org-ref-completion-library 'org-ref-helm-cite
        org-ref-default-bibliography (list medivhok:bibtex-file)))

(load-file (expand-file-name "+agenda.el" medivhok:app-module-directory))

(use-package org-roam
  :after
  (org helm)

  :straight
  (org-roam :host github :repo "org-roam/org-roam")

  :hook
  (after-init . org-roam-mode)

  :commands
  (org-roam-db-query)

  :general
  (medivhok:notes-menu
    :states 'normal
    "f" 'org-roam-find-file
    "G" 'org-roam-graph
    "i" 'org-roam-insert
    "r" 'org-roam-buffer-toggle-display)

  :init
  (defconst medivhok:roam-templates-directory
    (file-name-as-directory
     (expand-file-name "templates" medivhok:roam-directory))
    "The slip box cards templates directory.")

  (setq org-roam-completion-system 'helm
        org-roam-directory medivhok:roam-directory
        org-roam-file-exclude-regexp "setupfiles\\|templates"
        org-roam-index-file "index_file.org"
        org-roam-tag-sources '(prop)
        org-roam-title-sources '(title alias)
        org-roam-capture-templates
        `(("n" "note card" plain
           (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}
#+CREATED: %T
#+LAST_MODIFIED: %T

- tags ::"
           :unnarrowed t))

        org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
           :unnarrowed t)))

  (setq time-stamp-active t
        time-stamp-pattern "-10/^#\\+LAST_MODIFIED: <%Y-%02m-%02d %a %02H:%02M>$"
        time-stamp-format "%Y-%02m-%02d %a %02H:%02M")

  (defun medivhok/sanitize-roam-tags (item &optional item-list)
    (let ((current-list (or item-list (list))))
      (if (stringp item)
          (unless (member item current-list)
            (push item current-list))
        (dolist (current-item item)
          (setq current-list (medivhok/sanitize-roam-tags current-item current-list))))
      current-list))

  (defun medivhok/get-roam-tags ()
    "Return a list of the tags in my roam files."
    (medivhok/sanitize-roam-tags (org-roam-db-query [:select tags :from tags])))

  (defun medivhok/get-roam-tag-alist ()
    (let ((tag-list (medivhok/get-roam-tags))
          (tag-alist))
      (dolist (tag tag-list tag-alist)
        (push (cons tag nil) tag-alist))))

  (defun medivhok/select-roam-tags ()
    "Return a list of selected tags."
    (let ((available-tag-list (medivhok/get-roam-tags))
          (selected-tag-list)
          (selected-tag)
          (done-selection "[done]")
          (finished nil))
      (add-to-list 'available-tag-list done-selection)
      (while (not finished)
        (let ((selected-tag
               (completing-read
                (format "Tags %s: " (or selected-tag-list ""))
                available-tag-list)))
          (cond ((or (string= "" selected-tag)
                     (string= done-selection selected-tag))
                 (setq finished t))
                ((not selected-tag)
                 (message "Return is nil"))
                (t
                 (delete selected-tag available-tag-list)
                 (add-to-list 'selected-tag-list selected-tag t)))))
      selected-tag-list)))

(use-package org-roam-bibtex
  :straight
  (org-roam-bibtex :host github :repo "org-roam/org-roam-bibtex")

  :after
  (org-roam helm-bibtex org-ref)

  :hook
  (org-roam-mode . org-roam-bibtex-mode)

  :init
  (setq org-ref-notes-function 'orb-edit-notes
        orb-preformat-keywords '(("citekey" . "=key=")
                                 "title"
                                 "url"
                                 "file"
                                 "author-or-editor"
                                 "keywords")
        orb-templates `(("r" "ref" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "%<%Y%m%d%H%M%S>-${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
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
                         :file-name "%<%Y%m%d%H%M%S>-${slug}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${url}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":END:\n\n")
                         :unnarrowed t))))

(use-package ledger-mode
  :defer t)

(use-package flycheck-ledger
  :after flycheck)

(use-package evil-ledger
  :hook
  (ledger-mode . evil-ledger-mode)

  :general
  (medivhok:local-mode-menu 'normal
    "s" 'evil-ledger-sort))

(use-package ess
  :defer t)

(use-package ess-R-data-view
  :defer t)

(use-package polymode
  :defer t)

(use-package poly-R
  :defer t)
