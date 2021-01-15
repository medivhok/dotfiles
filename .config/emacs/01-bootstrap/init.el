(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups"
      user-emacs-directory)))
        url-history-file (expand-file-name "url/history" user-emacs-directory)
        auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-"
                                                     user-emacs-directory)
        projectile-known-projects-file (expand-file-name
                                        "projectile-bookmarks.eld"
                                        user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!).
(setq custom-file
        (if (boundp 'server-socket-dir)
            (expand-file-name "custom.el" server-socket-dir)
          (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                            temporary-file-directory)))
(load custom-file t)

(setq user-full-name "Jean Gregory Verret"
      user-mail-address "gregory.verret@gmail.com")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-verbose t)
