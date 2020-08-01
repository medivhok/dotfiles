;;; ~/.dotfiles/src/.config/doom/latex.d/+ox-latex.el -*- lexical-binding: t; -*-

(use-package! ox-latex
  :custom
  (org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "bibtex %b"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))
