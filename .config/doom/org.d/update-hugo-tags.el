;;; ~/.dotfiles/src/.config/doom/org.d/update-hugo-tags.el -*- lexical-binding: t; -*-

(defun medivhok/hugo-tags-update ()
  "Update the hugo tags from the current org buffer"
  (interactive)
  (if (not (derived-mode-p 'org-mode))
      (error "This is NOT org-mode!"))
  ;; hugo-tags-get
  )

(defun medivhok/hugo-tags-get ()
  "Retrieve a list of current hugo tags"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1))))

(defun medivhok/hugo-tags-get-settings-lines ()
  "Returns a list of line numbers"
  (interactive)
  (let ((settings-lines ()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-text (buffer-substring (point) (line-end-position))))
          (if (string-match-p "^#\\+HUGO_TAGS:" line-text)
              (cons settings-lines (line-beginning-position)))
        (forward-line 1))))))
