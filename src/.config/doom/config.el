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

;; Root directory of all my org files.
(setq org-directory "~/org/")

;; Directory of the org-agenda files.
(setq medivhok/agenda-directory (concat org-directory "agenda/"))

;; Directory of the org-roam files.
(setq org-roam-directory (concat org-directory "roam/"))

;; Directory of the bibtex-completion notes.
(setq bibtex-completion-notes-path org-roam-directory)

(setq medivhok/diary-file (concat medivhok/agenda-directory "diary.org")
      medivhok/emails-file (concat medivhok/agenda-directory "emails.org")
      medivhok/inbox-file (concat medivhok/agenda-directory "emails.org")
      medivhok/next-file (concat medivhok/agenda-directory "next.org")
      medivhok/projects-file (concat medivhok/agenda-directory "projects.org")
      medivhok/reading-file (concat medivhok/agenda-directory "reading.org")
      medivhok/reviews-file (concat medivhok/agenda-directory "reviews.org")
      medivhok/review-template-file (concat medivhok/agenda-directory "templates/weekly_review.org")
      medivhok/someday-file (concat medivhok/agenda-directory "someday.org")
      medivhok/teluq-file (concat medivhok/agenda-directory "teluq.org"))

;; Display lambda as symbols
(global-prettify-symbols-mode 1)


;;
;; 2. Emails
;;
(setq message-auto-save-directory "~/.mail/drafts/"
      sendmail-program (executable-find "msmtp")
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      mail-specify-envelope-from t
      message-kill-buffer-on-exit t
      notmuch-always-prompt-for-sender t
      notmuch-crypto-process-mime t
      notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
      notmuch-labeler-hide-known-labels t
      notmuch-search-oldest-first nil
      notmuch-archive-tags '("-inbox" "-unread")
      notmuch-message-headers '("To" "Cc" "Subject" "Bcc")
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox")
                               (:name "unread" :query "tag:inbox and tag:unread")
                               (:name "teluq" :query "tag:inbox and tag:teluq")
                               (:name "personal" :query "tag:inbox and tag:personal")
                               (:name "drafts" :query "tag:draft")))

;;
;; https://notmuchmail.org/notmuch-emacs/
;;
(use-package! notmuch
  :commands
  (notmuch)

  :init
  (setq +notmuch-sync-backend 'mbsync)

  :config
  (defun medivhok/notmuch-toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))

  (defun medivhok/notmuch-archive-all ()
    "Archive all the emails in the current view."
    (interactive)
    (notmuch-search-archive-thread nil (point-min) (point-max)))

  (defun medivhok/notmuch-delete-all ()
    "Archive all the emails in the current view. Mark them for deletion by cron job."
    (interactive)
    (notmuch-search-tag-all '("+deleted"))
    (+notmuch-archive-all))

  (map! :desc "notmuch" "<f2>" #'notmuch)

  (map! :map notmuch-search-mode-map
        :desc "toggle read" "t" #'medivhok/notmuch-toggle-read
        :desc "Reply to thread" "r" #'notmuch-search-reply-to-thread
        :desc "Reply to thread sender" "R" #'notmuch-search-reply-to-thread-sender
        :desc "Filter" "/" #'notmuch-search-filter
        :desc "Archive All" "A" #'medivhok/notmuch-archive-all
        :desc "Delete All" "D" #'medivhok/notmuch-delete-all)

  (map! :map notmuch-show-mode-map
        :desc "Next link" "<tab>" #'org-next-link
        :desc "Previous link" "<backtab>" #'org-previous-link
        :desc "URL at point" "C-<return>" #'browse-url-at-point))

(use-package! ol-notmuch
  :config
  (map!
   :map notmuch-show-mode-map
   :localleader "C" #'medivhok/org-capture-email)

  (defun medivhok/org-capture-email ()
    "Capture an email in notmuch"
    (interactive)
    (org-capture nil "e")))


;;
;; 3. Org
;;

;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.
;;  https://orgmode.org
(use-package! org
  :init
  (setq org-return-follows-link t
        org-catch-invisible-edits 'show
        org-log-done 'time
        org-log-into-drawer t
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))

        org-tag-alist '(("@commissions" . ?e)
                        ("@maison" . ?h)
                        ("@teluq" . ?s)
                        (:newline)
                        ("WAITING" . ?w)
                        ("HOLD" . ?H)
                        ("CANCELLED" . ?c)))

  :custom-face
  (org-link ((t (:inherit link :underline nil))))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (R . t)))

  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "exports")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

  ;; Recompute effort of a parent headline from the efforts of the children if
  ;; they sum to a higher value.
  ;;
  ;;   https://github.com/telotortium/emacs.d/blob/master/init.el
  ;;
  (defun medivhok/org-update-heading-effort-from-children (marker)
    "Compute the sum of efforts for each child of the heading at MARKER.
If the sum is greater than the current effort for this heading, offer to update
it.  This function is called recursively on each child, so the entire tree's
efforts may be updated by this function."
    (require 'call-log)                   ; For clog/msg
    (let*
        ((abort-at-marker)
         (ret
          (catch
              'break
            (org-with-point-at marker
              (clog/msg "At %S (%s)" (point-marker) (org-get-heading))
              (org-narrow-to-subtree)
              (outline-show-all)
              (let*
                  ((current-effort
                    (org-duration-to-minutes
                     (or (org-entry-get marker org-effort-property) 0)))
                   (children-effort 0))
                (save-excursion
                  (save-restriction
                    (when (org-goto-first-child)
                      ;; Use while loop with empty body to simulate a C do-while
                      ;; loop - in other words, we test at the end of the loop
                      ;; "body" whether a next sibling exists.
                      (while
                          (let ((x (medivhok/org-update-heading-effort-from-children (point-marker))))
                            (clog/msg "x = %S" x)
                            (setq children-effort (+ children-effort (nth 0 x)))
                            (org-get-next-sibling))))))
                (let ((children-effort-duration
                       (org-duration-from-minutes children-effort)))
                  (when (< current-effort children-effort)
                    (pcase (read-char-choice
                            (format
                             "Update effort in \"%s\" to children's sum (%s)? (y,n,j) "
                             (org-get-heading 'no-tags 'no-todo 'no-priority 'no-comment)
                             children-effort-duration)
                            '(?y ?n ?j))
                      (?n nil)
                      (?y
                       (org-entry-put
                        marker org-effort-property children-effort-duration)
                       (setq current-effort children-effort))
                      (?j
                       (setq abort-at-marker marker)
                       (throw 'break 'abort-at-marker)))))
                (list current-effort (point-max-marker)))))))
      (pcase ret
        ('abort-at-marker
         (clog/msg "%S" abort-at-marker)
         (pop-to-buffer-same-window (marker-buffer abort-at-marker))
         (set-buffer (marker-buffer abort-at-marker))
         (goto-char (marker-position abort-at-marker))
         'abort)
        ('abort 'abort)
        (_ ret))))

  (defun medivhok/org-effort-from-children-hook ()
    "Update effort of a heading from its children before clocking in."
    (pcase (medivhok/org-update-heading-effort-from-children (point-marker))
      ('abort 'abort)
      (_ nil)))

  (add-hook 'org-clock-in-prepare-hook 'medivhok/org-effort-from-children-hook)

  (defun medivhok/org-update-heading-effort-from-children-all ()
    "Run over all projects, updating their efforts from their children.
Pressing ‘j’ will abort the run, leaving the point at the heading we were at
when ‘j’ was pressed."
    (interactive)
    (require 'call-log)
    (org-map-entries
     (lambda ()
       (display-buffer (current-buffer) '(display-buffer-same-window))
       (recenter nil)
       (pcase (medivhok/org-effort-from-children-hook)
         ('abort
          (clog/msg "'abort")
          (setq org-map-continue-from (point))
          (let ((debug-on-quit nil))
            (signal 'quit nil)))
         (x x)))
     nil 'agenda #'bh/skip-tasks)
    (clog/msg "Updating efforts complete.")))


;;
;; 4. Agenda
;;
(use-package! org-agenda
  :after
  (org)

  :commands
  (org-agenda)

  :custom
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)

  :init
  (defun medivhok/open-agenda ()
    "Opens my gtd agenda."
    (interactive)
    (org-agenda nil " "))

  (map! "<f1>" #'medivhok/open-agenda)

  :config
  (setq org-agenda-files (list medivhok/agenda-directory)
        org-agenda-diary-file medivhok/diary-file
        org-agenda-block-separator nil
        org-agenda-start-with-log-mode t

        org-refile-targets `((,medivhok/next-file :level . 0)
                             (,medivhok/someday-file :level . 0)
                             (,medivhok/reading-file :level . 1)
                             (,medivhok/projects-file :maxlevel . 1))

        org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-deadline-warning-days 14)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files (list medivhok/inbox-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Emails")
                   (org-agenda-files (list medivhok/emails-file))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files (list medivhok/next-file
                                           medivhok/projects-file
                                           medivhok/someday-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files (list medivhok/projects-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Teluq")
                   (org-agenda-files (list medivhok/teluq-file))))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off Tasks")
                   (org-agenda-files (list medivhok/next-file))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package! org-capture
  :commands
  (org-capture)

  :config
  (setq org-capture-templates
        `(("i" "inbox" entry
           (file ,medivhok/inbox-file)
           "* TODO %?")

          ("e" "email" entry
           (file+headline ,medivhok/emails-file "Emails")
           "* TODO [#A] Reply: %a :@home:@school:"
           :immediate-finish t)

          ("c" "org-protocol-capture" entry
           (file ,medivhok/inbox-file)
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)

          ("w" "Weekly Review" entry
           (file+olp+datetree ,medivhok/reviews-file)
           (file ,medivhok/review-template-file))

          ("r" "Reading" todo
           ""
           ((org-agenda-files '(,medivhok/reading-file))))))

  (defun medivhok/inbox-capture ()
    "Capture a task for the inbox"
    (org-capture nil "i"))

  (map! "<f4>" #'medivhok/inbox-capture))


;;
;; 5. Roam
;;
(use-package! org-roam
  :after (org)

  :commands
  (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)

  :hook
  (after-init . org-roam-mode)

  :init
  (setq org-roam-title-sources '(title)
        org-roam-tag-sources '(prop)
        org-roam-graph-exclude-matcher '("setup"))


  :config
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
           :unnarrowed t))))

(use-package! org-roam-server
  :after
  (org-roam)

  :config
  (org-roam-server-mode 1))

(setq bibtex-completion-notes-path org-roam-directory
      bibtex-completion-bibliography (list (concat org-directory "bibliography/zotero.bib"))
      bibtex-completion-pdf-field "file"
      reftex-default-bibliography bibtex-completion-bibliography)

;; Search and manage bibliographies in Emacs.
;;  https://github.com/tmalsburg/helm-bibtex
(after! bibtex-completion
  (map! :desc "helm-bibtex" "<f3>" #'helm-bibtex))

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

(setq orb-preformat-keywords '(("citekey" . "=key=")
                               "title"
                               "url"
                               "file"
                               ;;"author-or-editor"
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
                       :unnarrowed t)))

(use-package! org-roam-bibtex
  :after
  (org-roam)

  :hook
  (org-roam-mode . org-roam-bibtex-mode))

;; citations, cross-references, indexes, glossaries and bibtex utilities for
;; org-mode
;;  https://github.com/jkitchin/org-ref
(use-package! org-ref
  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-filename-helm-bibtex
        org-ref-default-bibliography (list bibtex-completion-bibliography)
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

(use-package! org-noter
  :after
  (org-pdftools)

  :custom
  ;;(org-noter-notes-window-location 'other-frame)
  (org-noter-always-create-frame nil)
  (org-noter-hide-other nil)
  (org-noter-notes-search-path (list medivhok/roam-directory)))

;;
;; 6. Latex
;;
(use-package! ox-latex
  :custom
  (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))


;;
;; 7. Dictionnary setup
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
