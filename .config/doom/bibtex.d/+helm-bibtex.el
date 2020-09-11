;;; ~/.dotfiles/src/.config/doom/bibtex.d/+helm-bibtex.el -*- lexical-binding: t; -*-

;;
;; Search and manage bibliographies in Emacs.
;;
;; https://github.com/tmalsburg/helm-bibtex
;;
(after! helm-bibtex
  (map! :desc "helm-bibtex" "<f3>" #'helm-bibtex))
