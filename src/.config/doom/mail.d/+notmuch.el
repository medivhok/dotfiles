;;; ~/.dotfiles/src/.config/doom/mail.d/+notmuch.el -*- lexical-binding: t; -*-

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
