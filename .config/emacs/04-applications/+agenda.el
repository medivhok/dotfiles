(use-package org-agenda
  :preface
  (general-create-definer medivhok:agenda:menu-def
    :prefix (medivhok:menu/prepend-leader "a")
    :prefix (medivhok:menu/prepend-leader "a" t)
    nil '(:ignore t :which-key "agenda"))

  :defer t

  :after (org)

  :straight org

  :init
  (defconst medivhok:gtd-file
    (expand-file-name "gtd.org" medivhok:agenda-directory)
    "My 'getting things done' agenda file.")

  :custom
  (org-agenda-window-setup 'current-window)
  (org-agenda-block-separator nil)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-custom-commands
   `((" "
      "GTD Agenda"
      ((agenda ""
	             ((org-agenda-span 'week)
	              (org-deadline-warning-days 14)))
       (tags-todo "@inbox"
		              ((org-agenda-overriding-header "Inbox")))
       (tags-todo "@tâches"
		              ((org-agenda-overriding-header "Tâches")))
       (tags-todo "@teluq"
		              ((org-agenda-overriding-header "Teluq")))
       (tags-todo "@projets"
		              ((org-agenda-overriding-header "Projets"))))))))

(use-package org-capture
  :straight org

  :commands (org-capture)
  :bind
  (("<f4>" . (lambda () (interactive) (org-capture nil "i"))))

  :config
  (setq org-capture-templates
	`(("i" "inbox" entry
	   (file+headline ,medivhok:gtd-file "Inbox")
	   "* TODO [#C] %?\n:PROPERTIES:\n:Effort: 1\n:END:\n")

	  ("e" "email" entry
	   (file+headline ,medivhok:gtd-file "Emails")
	   "* TODO [#A] Reply: %a"
	   :immediate-finish t))))

(use-package org-refile
  :straight org

  :config
  (setq org-refile-allow-creating-parent-nodes 'confirm
	      org-refile-use-outline-path 'file
	      org-refile-targets '((nil :tag . "@tâches")
	                     	   (nil :tag . "@cours")
				   (nil :tag . "@projet")
				   (nil :tag . "@teluq"))))


