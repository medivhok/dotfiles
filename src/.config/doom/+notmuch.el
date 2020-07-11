;;; ~/.dotfiles/src/.config/doom/+notmuch.el -*- lexical-binding: t; -*-

;;
;; Email configurations.
;;
(use-package! notmuch
  :commands (notmuch)
  :init
  (setq +notmuch-sync-backend nil)
  (defun +notmuch/toggle-read ()
    "toggle read status of message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))
  :config
  (setq message-auto-save-directory "~/.mail/drafts/"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (executable-find "msmtp")
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-specify-envelope-from t
        message-sendmail-f-is-evil nil
        message-kill-buffer-on-exit t
        notmuch-always-prompt-for-sender t
        notmuch-crypto-process-mime t
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        notmuch-labeler-hide-known-labels t
        notmuch-search-oldest-first nil
        notmuch-archive-tags '("-inbox" "-unread")
        notmuch-message-headers '("To" "Cc" "Subject" "Bcc")
        notmuch-saved-searches '((:name "unread" :query "tag:inbox and tag:unread")
                                 (:name "org-roam" :query "tag:inbox and tag:roam")
                                 (:name "personal" :query "tag:inbox and tag:personal")
                                 (:name "drafts" :query "tag:draft"))))
