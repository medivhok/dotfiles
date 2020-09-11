;;; ~/.dotfiles/src/.config/doom/+dashboard.el -*- lexical-binding: t; -*-

(use-package! dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-show-shortcuts nil)
  (dashboard-set-navigator t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (show-week-agenda-p t)
  (dashboard-items '((recents . 5)
		                 (projects . 3)
		                 (agenda . 5)))

  :config
  ;; (defun medivhok/dashboard-insert-custom (list-size)
  ;;   "Add the list of LIST-SIZE items of menu."
  ;;   (dashboard-insert-section
  ;;    "Menu:"
  ;;    '("Budget" "Org")
  ;;    2
  ;;    "l"
  ;;    `(lambda (&rest ignore))))
  ;; (add-to-list 'dashboard-item-generators '(custom . medivhok/dashboard-insert-custom))
  ;; (add-to-list 'dashboard-items '(custom) t)
  (dashboard-setup-startup-hook))
