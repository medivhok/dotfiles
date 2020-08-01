;;; ~/.dotfiles/src/.config/doom/org.d/+org.el -*- lexical-binding: t; -*-

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
