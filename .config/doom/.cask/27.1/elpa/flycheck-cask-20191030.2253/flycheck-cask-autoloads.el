;;; flycheck-cask-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-cask" "flycheck-cask.el" (0 0 0 0))
;;; Generated autoloads from flycheck-cask.el

(autoload 'flycheck-cask-setup "flycheck-cask" "\
Setup Cask integration for Flycheck.

If the current file is part of a Cask project, as denoted by the
existence of a Cask file in the file's directory or any ancestor
thereof, configure Flycheck to initialze Cask packages while
syntax checking.

Set `flycheck-emacs-lisp-initialize-packages' and
`flycheck-emacs-lisp-package-user-dir' accordingly." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-cask" '("flycheck-cask-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-cask-autoloads.el ends here
