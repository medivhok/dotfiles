;;; ~/.dotfiles/src/.config/doom/bibtex.d/+bibtex-completion.el -*- lexical-binding: t; -*-

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path medivhok/roam-directory
        bibtex-completion-bibliography reftex-default-bibliography))
