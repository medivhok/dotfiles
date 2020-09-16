;;; config.el -*- lexical-binding: t; -*-

;;
;; Global settings
;;
(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com"
      doom-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font")
      doom-theme 'doom-one
      display-line-numbers-type 'relative
      medivhok/root-directory "~/org/"
      reftex-default-bibliography (list (concat medivhok/root-directory
                                                "bibtex/zotero.bib")))


(load! "+agenda")
(load! "+budget")
(load! "+notes")

;; Display lambda as symbols
(global-prettify-symbols-mode 1)

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

;;
;; Org mode is for keeping notes, maintaining TODO lists, planning projects, and
;; authoring documents with a fast and effective plain-text system.
;;
;; https://orgmode.org
;;
(use-package! org
  :custom-face
  (org-link ((t (:inherit link :underline nil))))

  :config
  (setq org-directory medivhok/root-directory
        org-return-follows-link t
        org-catch-invisible-edits 'show
        org-log-done 'time
        org-log-into-drawer t
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id

        org-format-latex-options (plist-put org-format-latex-options :scale 3.5)

        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))

        org-tag-alist-for-agenda '(("@commissions" . ?e)
                                   ("@maison" . ?h)
                                   ("@teluq" . ?s)
                                   (:newline)
                                   ("WAITING" . ?w)
                                   ("HOLD" . ?H)
                                   ("CANCELLED" . ?c)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
                               (R . t))))

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

(defun medivhok/apply-carriage-return (start)
  ""
  (save-excursion
    (goto-char start)
    (while (< (point) (point-max))
      (if (eq ?\u000D (char-after))
          (delete-region (line-beginning-position) (1+ (point))))
      (goto-char (1+ (point))))))

(defun colorize-buttercup-buffer (start end size)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region start end)
    (medivhok/apply-carriage-return start)))

(add-hook 'special-mode-hook
          (lambda ()
            (if (string= (buffer-name) "*Buttercup*")
                (add-hook 'after-change-functions 'colorize-buttercup-buffer nil t))))

(use-package! ox-latex
  :custom
  (org-latex-listings 'minted)
  (org-latex-packages-alist '(("" "minted")))
  (org-latex-minted-langs '((R "r")))
  (org-latex-minted-options '(("style" "colorful")
                              ("frame" "single")
                              ("framesep" "8pt")
                              ("xleftmargin" "16pt")))
  (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;;
;; https://notmuchmail.org/notmuch-emacs/
;;
(use-package! notmuch
  :commands
  (notmuch)

  :init
  (setq +notmuch-sync-backend 'mbsync)

  :config
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
  :after
  (notmuch org)

  :config
  (map!
   :map notmuch-show-mode-map
   :localleader "C" #'medivhok/org-capture-email)

  (defun medivhok/org-capture-email ()
    "Capture an email in notmuch"
    (interactive)
    (org-capture nil "e")))
