;;; slip-box-helpers.el --- slip box helper functions -*- lexical-binding: t; -*-

;;; code:
(defun medivhok/roam-assert-tag (tag)
  "Throw is TAG is not a valid tag.
A valid tag has the following properties :
- is a string;
- is non-empty; and
- cannot be '@'."
  (unless (and (stringp tag)
               (not (string= "" tag))
               (not (string= "@" tag)))
    (signal 'wrong-type-argument '(tag)))
  t)
(defun medivhok/roam-plain-tag-p (tag)
  "Predicate for plain tag.
Return t if TAG is a non-empty string and does not start with '@',
otherwise nil is returned."
  (medivhok/roam-assert-tag tag)
  (not (string= "@" (substring tag 0 1))))
(defun medivhok/roam-arroba-tag-p (tag)
  "Predicate for arroba tag.
Return t if TAG is a non-empty string starting with '@' followed by at
least one character, otherwise nil is returned."
  (medivhok/roam-assert-tag tag)
  (and (> (length tag) 1)
       (string= "@" (substring tag 0 1))))
(defun medivhok/roam-plain-tag (tag)
  ""
  (if (medivhok/roam-arroba-tag-p tag)
      (substring tag 1)
    tag))
(defun medivhok/roam-arroba-tag (tag)
  ""
  (if (medivhok/roam-plain-tag-p tag)
      (concat "@" tag)
    tag))

(provide 'slip-box-helpers)
;; slip-box-helpers.el ends here
