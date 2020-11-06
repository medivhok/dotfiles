;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-

;; Author: Jean Gregory Verret <gregory.verret@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my Emacs configuration.

;;; Code:

;; The default is 800k (mesured in bytes).
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup.
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

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)

  :config
  (evil-mode 1))

(use-package evil-collection
  :defer t
  :after evil

  :custom
  (evil-collection-outline-bind-tab-p nil))

(use-package hydra
  :defer t)

(use-package which-key
  :custom
  (which-key-idle-delay 0.3)

  :config
  (which-key-mode))

(use-package general
  :config
  (general-evil-setup t))

(defvar medivhok:leader-key "SPC"
  "The leader key.")

(defconst medivhok:non-normal-leader-key "M-SPC"
  "The 'non normal state' leader key.")

(defun medivhok/expand-menu-key (menu-key &optional non-normal-menu-key)
  "Returns a string of the MENU-KEY prefixed by `medivhok:leader-key' and
a space. If NON-NORMAL-MENU-KEY is non nil, `medivhok:non-local-leader-key'
is used instead."
  (if (null non-normal-menu-key)
      (concat medivhok:leader-key " " menu-key)
    (concat medivhok:non-normal-leader-key " " menu-key)))

(defhydra hydra-zoom ()
  "zoom"
  ("-" text-scale-decrease "out")
  ("=" text-scale-increase "in"))

(general-create-definer medivhok:main-menu
  :prefix medivhok:leader-key
  :non-normal-prefix medivhok:non-normal-leader-key
  :keymaps 'override)

(medivhok:main-menu
  :states 'normal
  ":" 'execute-extended-command
  "-" '(hydra-zoom/text-scale-decrease
        :which-key "text-scale-decrease")
  "=" '(hydra-zoom/text-scale-increase
        :which-key "text-scale-increase"))

(general-create-definer medivhok:local-mode-menu
  :prefix (medivhok/expand-menu-key "m")
  :non-normal-prefix (medivhok/expand-menu-key "m" t)
  :keymaps 'override)

(general-create-definer medivhok:applications-menu
  :prefix (medivhok/expand-menu-key "a")
  :non-normal-prefix (medivhok/expand-menu-key "a" t)
  :keymaps 'override
  nil '(:ignore t :which-key "applications"))

(general-create-definer medivhok:buffer-menu
  :prefix (medivhok/expand-menu-key "b")
  :non-normal-prefix (medivhok/expand-menu-key "b" t)
  :keymaps 'override
  nil '(:ignore t :which-key "buffer"))

(medivhok:buffer-menu
  :states 'normal
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "d" 'kill-current-buffer)

(general-create-definer medivhok:emacs-menu
  :prefix (medivhok/expand-menu-key "e")
  :non-normal-prefix (medivhok/expand-menu-key "e" t)
  :keymaps 'override
  nil '(:ignore t :which-key "emacs"))

(medivhok:emacs-menu
  :states 'normal
  "e" '((lambda ()
          (interactive)
          (find-file
           (expand-file-name "README.org"
                             (file-name-directory user-init-file))))
        :which-key "edit literate config")
  "E" '((lambda ()
          (interactive)
          (find-file
           (expand-file-name "init.el"
                             (file-name-directory user-init-file))))
        :which-key "edit config"))

(general-create-definer medivhok:file-menu
  :prefix (medivhok/expand-menu-key "f")
  :non-normal-prefix (medivhok/expand-menu-key "f" t)
  :keymaps 'override
  nil '(:ignore t :which-key "file"))

(medivhok:file-menu
  :states 'normal
  "f" 'find-file
  "r" 'counsel-recentf)

(general-create-definer medivhok:help-menu
  :prefix (medivhok/expand-menu-key "h")
  :non-normal-prefix (medivhok/expand-menu-key "h" t)
  :keymaps 'override
  nil '(:ignore t :which-key "help"))

(medivhok:help-menu
  :states 'normal
  "a" 'apropos-command
  "b" 'describe-bindings
  "f" 'describe-function
  "i" 'info
  "k" 'general-describe-keybindings
  "s" 'counsel-describe-symbol
  "v" 'describe-variable)

(general-create-definer medivhok:notes-menu
  :prefix (medivhok/expand-menu-key "n")
  :non-normal-prefix (medivhok/expand-menu-key "n" t)
  :keymaps 'override
  nil '(:ignore t :which-key "notes"))

(general-create-definer medivhok:window-menu
  :prefix (medivhok/expand-menu-key "w")
  :non-normal-prefix (medivhok/expand-menu-key "w" t)
  :keymaps 'override
  nil '(:ignore t :which-key "window"))

(medivhok:window-menu
  :states 'normal
  "q" 'delete-window
  "s" 'split-window-below
  "\\" 'split-window-right)

(general-create-definer medivhok:quit-menu
  :prefix (medivhok/expand-menu-key "q")
  :non-normal-prefix (medivhok/expand-menu-key "q" t)
  :keymaps 'override
  nil '(:ignore t :which-key "quit"))

(medivhok:quit-menu
  :states 'normal
  "q" 'save-buffers-kill-terminal)

(use-package all-the-icons

:preface
(global-prettify-symbols-mode 1)

)

(use-package amx
  :after ivy

  :custom
  (amx-backend 'ivy)

  :config
  (amx-mode))

(use-package counsel

:after evil-collection

:config
(evil-collection-init 'ivy)
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")
(ivy-mode 1)
(counsel-mode 1)

)

(use-package ivy-rich

:after ivy

:config
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

)

(use-package ivy-posframe
  :after ivy

  :custom
  (ivy-posframe-display-functions-alist
   '((t . ivy-posframe-display-at-frame-center)))

  :config
  (ivy-posframe-mode 1))

(set-face-attribute 'default nil
                    :font "Hack Nerd Font"
                    :height 130)
(set-face-attribute 'fixed-pitch nil
                    :font "DroidSansMono Nerd Font"
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :font "Hack Nerd Font"
                    :height 130
                    :weight 'regular)

(use-package dashboard
  :preface
  (setq inhibit-startup-message t)

  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (agenda . 5)))
  (evil-collection-init 'dashboard))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tooltip-mode -1)

;; We disable the tool and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(setq visible-bell t)

(global-hl-line-mode t)

;; One line at a time.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Don't accelerate scrolling.
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse.
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll one line at a time.
(setq scroll-step 1)

(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)
(global-display-line-numbers-mode t)
(column-number-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq-default fill-column 80)

(use-package doom-modeline
  :hook
  (window-setup . doom-modeline-mode)

  ;; :custom-face
  ;; (mode-line ((t (:height 0.85))))
  ;; (mode-line-inactive ((t (:height 0.85))))

  :init
  (setq doom-modeline-bar-width 6
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-buffer-state-icon t
        doom-modeline-github nil
        doom-modeline-height 15
        doom-modeline-icon (display-graphic-p)
        doom-modeline-irc nil
        doom-modeline-lsp t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes t
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-project-detection 'projectile))

(use-package minions
  :init
  (setq minions-mode-line-lighter " ")

  :config
  (minions-mode 1))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package helpful
  :after
  (counsel evil-collection)

  :config
  (evil-collection-init 'helpful)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package bibtex-completion
  :defer t
  :after org
  :custom
  (bibtex-completion-bibliography
   (expand-file-name
    "zotero.bib"
    (file-name-as-directory
     (expand-file-name "readings" org-directory))))
  (bibtex-completion-pdf-field "File"))

(use-package company
  :hook
  (after-init . global-company-mode)

  :config
  (setq company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))))

  (defun medivhok/company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
	      backends
	    (append (if (consp backends)
		              backends
		            (list backends))
		          '(:with company-yasnippet))))

  ;; add yasnippet to all backends
  (setq company-backends
        (mapcar #'medivhok/company-backend-with-yas company-backends)))

(use-package company-dict
  :after company)

(use-package company-box
  :after company

  :hook
  (company-mode . company-box-mode))

(use-package helm
  :config
  (require 'helm-config))

(use-package pdf-tools
  :defer t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (evil-collection-init 'pdf)
  (evil-collection-pdf-setup))

(setq-default tab-width 2)
(setq-default evil-shift-with tab-width)
(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)

(use-package hideshow)

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  (prog-mode . smartparens-strict-mode)

  :config
  (require 'smartparens-config))

(use-package evil-nerd-commenter
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package ws-butler
  :hook
  ((text-mode . ws-butler-mode)
   (prog-mode . ws-butler-mode)))

(use-package parinfer
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
      '(defaults       ; should be included.
        pretty-parens  ; different paren styles for different modes.
        evil           ; If you use Evil.
        smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
        smart-yank)))  ; Yank behavior depend on mode.

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :custom
  (yas-snippet-dirs
     (list
      (file-name-as-directory
       (expand-file-name "snippets"
                         (file-name-directory user-init-file)))))
  :general
  :config
  (yas-global-mode 1)
  )

(use-package magit
  :commands
  (magit-status magit-get-current-branch)

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-execpt-diff-v1))

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :hook
  ((text-mode . git-gutter-mode)
   (prog-mode . git-gutter-mode))

  :config
  (setq git-gutter:update-interval 2))

(use-package git-link
  :commands git-link

  :config
  (setq git-link-open-in-browser t))

(use-package magit-todos
  :after magit)

(use-package projectile
  :config
  (projectile-mode))

(use-package counsel-projectile
  :after projectile)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package org

:preface
(defun medivhok/org-babel-tangle-dont-ask ()
  "Disable confirmation before tangling."
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(defun medivhok/org-buffer-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)

  (add-hook 'after-save-hook
            #'medivhok/org-babel-tangle-dont-ask
            'run-at-end
            'only-in-org-mode))

:hook
(org-mode . medivhok/org-buffer-setup)

:init
(setq org-directory "~/org/")

(defconst medivhok:agenda-directory
  (file-name-as-directory
   (expand-file-name "agenda" org-directory))
  "The directory of my agenda files.")

(setq org-agenda-files (list medivhok:agenda-directory))

:custom
(org-catch-invisible-edits 'show)
(org-cycle-separator-lines 2)
(org-edit-src-content-indentation 0)
(org-ellipsis " ▼")
(org-hide-block-startup nil)
(org-hide-emphasis-markers t)
(org-log-done 'time)
(org-log-into-drawer t)
(org-outline-path-complete-in-steps nil)
(org-return-follows-link t)
(org-src-fontify-natively t)
(org-src-preserve-indentation nil)
(org-src-tab-acts-natively t)
(org-src-window-setup 'current-window)
(org-startup-folded t)

:custom-face
(org-link ((t (:inherit link :underline nil))))

:general
(medivhok:local-mode-menu 'normal
  org-mode-map
  nil '(:ignore t :which-key "org")
  "e" 'org-export-dispatch
  "t" '(:ignore t :which-key "toggle")
  "tl" 'org-toggle-link-display)

:config
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 4.0))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . t)
   (ledger . t)
   (R . t)))

(add-hook 'org-src-mode-hook
          (lambda ()
            (when (eq major-mode 'emacs-lisp-mode)
              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

;; Replace list hyphen with dot.
(require 'org-indent)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package evil-org
  :hook
  ((org-mode . evil-org-mode)
   (org-agenda-mode . evil-org-mode)
   (evil-org-mode . (lambda ()
  	(evil-org-set-key-theme))))
  )

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "▶" "☰" "▷" "○"))
  )

(use-package org-make-toc
  :hook
  (org-mode . org-make-toc-mode)
  )

(use-package ox-odt
  :after org

  :straight
  (org :type git :local-repo "org")

  :custom
  (org-odt-convert-process "unoconv")
  (org-odt-convert-processes '(("unoconv"
                                "unoconv -f %f %i")))
  (org-odt-preferred-output-format "docx")
  (org-odt-prettify-xml t)

  :config
  (org-odt-add-automatic-style "TNOrgTitle"
                               '(("style:family" "paragraph")
                                 ("style:parent-style-name" "OrgTitle")
                                 ("style:master-page-name" "OrgTitlePage"))))

(defun medivhok/open-agenda ()
  "Opens my GTD agenda."
  (interactive)
  (org-agenda nil " "))

(use-package org-agenda
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

(use-package org-roam
  :after (org)

  :straight
  (org-roam :host github :repo "org-roam/org-roam")

  :hook
  (after-init . org-roam-mode)

  :commands
  (org-roam-db-query)

  :preface
  (setq org-roam-directory
        (file-name-as-directory
         (expand-file-name "slip-boxes" org-directory)))

  (defconst medivhok:notes-directory
    (file-name-as-directory
     (expand-file-name "notes" org-roam-directory))
    "The slip box with my notes cards.")

  (defconst medivhok:annotated-bibliography-directory
    (file-name-as-directory
     (expand-file-name "annotated-bibliography" org-roam-directory))
    "The slip box with my annotated bibliography cards.")

  (defconst medivhok:glossaries-directory
    (file-name-as-directory
     (expand-file-name "glossaries" org-roam-directory))
    "The slip box with my glossaries cards.")

  (defconst medivhok:card-templates-directory
    (file-name-as-directory
     (expand-file-name "templates" org-roam-directory))
    "The directory containing the card templates for my slip boxes.")

  :custom
  (org-roam-completion-system 'helm)
  (org-roam-file-exclude-regexp "setupfiles\\|templates")
  (org-roam-index-file "index_file.org")
  (org-roam-tag-sources '(prop))
  (org-roam-title-sources '(title alias))
  (org-roam-capture-templates
   `(("n" "note card" plain
      (function org-roam--capture-get-point)
      "%?"
      :file-name "notes/${slug}"
      :head "#+TITLE: ${title}
#+CREATED: %T
#+LAST_MODIFIED: %T

- tags ::"
      :unnarrowed t)

     ("g" "glossaries card" plain
      (function org-roam--capture-get-point)
      (file ,(expand-file-name "glossary-card.org"
                             medivhok:card-templates-directory))
      :file-name "glossaries/${slug}"
      :head ""
      :unnarrowed t)))

  (org-roam-ref-capture-templates
   '(("r" "ref" plain (function org-roam-capture--get-point)
      "%?"
      :file-name "websites/${slug}"
      :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
      :unnarrowed t)))

  :general
  (medivhok:notes-menu
    :states 'normal
    "b" '(:ignore t :which-key "annotated bibliography")
    "bf" 'medivhok/find-annotated-bibliography-card
    "f" 'medivhok/find-note-card
    "g" '(:ignore t :which-key "glossaries")
    "gf" 'medivhok/find-glossary-card
    "G" 'org-roam-graph
    "i" 'org-roam-insert
    "r" 'org-roam-buffer-toggle-display)

  :init
  (setq time-stamp-active t
        time-stamp-pattern "-10/^#\\+LAST_MODIFIED: <%Y-%02m-%02d %a %02H:%02M>$"
        time-stamp-format "%Y-%02m-%02d %a %02H:%02M")

  (defun medivhok/sanitize-roam-tags (item &optional item-list)
    (let ((current-list (or item-list (list))))
      (if (stringp item)
          (unless (member item current-list)
            (push item current-list))
        (dolist (current-item item)
          (setq current-list (medivhok/sanitize-roam-tags current-item current-list))))
      current-list))

  (defun medivhok/get-roam-tags ()
    "Return a list of the tags in my roam files."
    (medivhok/sanitize-roam-tags (org-roam-db-query [:select tags :from tags])))

  (defun medivhok/get-roam-tag-alist ()
    (let ((tag-list (medivhok/get-roam-tags))
          (tag-alist))
      (dolist (tag tag-list tag-alist)
        (push (cons tag nil) tag-alist))))

  (defun medivhok/select-roam-tags ()
    "Return a list of selected tags."
    (let ((available-tag-list (medivhok/get-roam-tags))
          (selected-tag-list)
          (selected-tag)
          (done-selection "[done]")
          (finished nil))
      (add-to-list 'available-tag-list done-selection)
      (while (not finished)
        (let ((selected-tag
               (completing-read
                (format "Tags %s: " (or selected-tag-list ""))
                available-tag-list)))
          (cond ((or (string= "" selected-tag)
                     (string= done-selection selected-tag))
                 (setq finished t))
                ((not selected-tag)
                 (message "Return is nil"))
                (t
                 (delete selected-tag available-tag-list)
                 (add-to-list 'selected-tag-list selected-tag t)))))
      selected-tag-list)))

(use-package org-ref
  :after
  (org ivy-bibtex)

  :custom
  (org-ref-completion-library 'org-ref-ivy-cite)
  (org-ref-default-bibliography bibtex-completion-bibliography)
  (org-ref-notes-directory medivhok:annotated-bibliography-directory))

(use-package org-roam-bibtex

:straight
(org-roam-bibtex :host github :repo "org-roam/org-roam-bibtex")

:after
(org-roam ivy-bibtex org-ref)

:hook
(org-roam-mode . org-roam-bibtex-mode)

:custom
(org-ref-notes-function 'orb-edit-notes)
(orb-preformat-keywords '(("citekey" . "=key=")
                          "title"
                          "url"
                          "file"
                          "author-or-editor"
                          "keywords"))
(orb-templates `(("r" "ref" plain (function org-roam-capture--get-point)
                  ""
                  :file-name "annotated-bibliography/${citekey}"
                  :head
                  ,(concat "#+TITLE: ${title}\n"
                           "#+ROAM_KEY: ${ref}\n"
                           "* Notes\n"
                           ":PROPERTIES:\n"
                           ":Custom_ID: ${citekey}\n"
                           ":URL: ${url}\n"
                           ":AUTHOR: ${author-or-editor}\n"
                           ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")\n"
                           ":END:\n\n")
                  :unnarrowed t)

                 ("w" "webpage" plain (function org-roam-capture--get-point)
                  ""
                  :file-name "annotated-bibliography/${citekey}"
                  :head
                  ,(concat "#+TITLE: ${title}\n"
                           "#+ROAM_KEY: ${url}\n\n"
                           "* Notes\n"
                           ":PROPERTIES:\n"
                           ":Custom_ID: ${citekey}\n"
                           ":URL: ${url}\n"
                           ":END:\n\n")
                  :unnarrowed t))))

(defconst medivhok:slip-boxes-directory
  (file-name-as-directory
   (expand-file-name "slip-boxes" org-directory))
  "The directory containing my slip boxes.")

(defconst medivhok:notes-directory
  (file-name-as-directory
   (expand-file-name "notes" medivhok:slip-boxes-directory))
  "The slip box with my notes cards.")

(defconst medivhok:annotated-bibliography-directory
  (file-name-as-directory
   (expand-file-name "annotated-bibliography" medivhok:slip-boxes-directory))
  "The slip box with my annotated bibliography cards.")

(defconst medivhok:glossaries-directory
  (file-name-as-directory
   (expand-file-name "glossaries" medivhok:slip-boxes-directory))
  "The slip box with my glossaries cards.")

(defconst medivhok:card-templates-directory
  (file-name-as-directory
   (expand-file-name "templates" medivhok:slip-boxes-directory))
  "The directory containing the card templates for my slip boxes.")

(defconst medivhok:pdf-root-directory
  (file-name-as-directory
   (expand-file-name "readings" org-directory))
  "The root directory of my PDF files.")

(defconst medivhok:bibtex-file
  (expand-file-name "zotero.bib" medivhok:pdf-root-directory)
  "My bibtex file, generated by 'zotero'.")

(defun medivhok/card-entry-< (entry-a entry-b)
  "Returns ENTRY-A < ENTRY-B."
  (string< (car entry-a) (car entry-b)))

(defun medivhok/note-card-entry-p (card-entry)
  "Check if CARD-ENTRY is a note card."
  (string-match medivhok:notes-directory
                (plist-get (cdr card-entry) :path)))

(defun medivhok/find-note-card ()
  (interactive)
  (org-roam-find-file
   ""
   nil
   (lambda (cards-entries)
     (interactive)
     (sort (seq-filter 'medivhok/note-card-entry-p
                       cards-entries)
           'medivhok/card-entry-<))))

(defun medivhok/annotated-bibliography-card-entry-p (card-entry)
  "Check if CARD-ENTRY is an annotated bibliography card."
  (string-match medivhok:annotated-bibliography-directory
                (plist-get (cdr card-entry) :path)))

(defun medivhok/find-annotated-bibliography-card ()
  (interactive)
  (org-roam-find-file
   ""
   nil
   (lambda (cards-entries)
     (interactive)
     (sort (seq-filter 'medivhok/annotated-bibliography-card-entry-p
                       cards-entries)
           'medivhok/card-entry-<))))

(defun medivhok/glossary-card-entry-p (card-entry)
  "Check if CARD-ENTRY is a glossary card."
  (string-match medivhok:glossaries-directory
                (plist-get (cdr card-entry) :path)))

(defun medivhok/find-glossary-card ()
  (interactive)
  (org-roam-find-file
   ""
   nil
   (lambda (cards-entries)
     (interactive)
     (sort (seq-filter 'medivhok/glossary-card-entry-p
                       cards-entries)
           'medivhok/card-entry-<))))

(use-package ledger-mode
  :defer t)

(use-package flycheck-ledger
  :after flycheck)

(use-package evil-ledger
  :hook
  (ledger-mode . evil-ledger-mode)

  :general
  (medivhok:local-mode-menu 'normal
    "s" 'evil-ledger-sort))

(use-package cask)

(use-package cask-mode
  :defer t)

(use-package buttercup
  :defer t)

(use-package haskell-mode

:general

:init

:config

)

(use-package dante

:general

:init

:config

)

(use-package attrap

:general

:init

:config

)

(use-package json-mode
  :custom
  (json-reformat:indent-width 2)

  :general
  (medivhok/local-leader-def 'normal
    json-mode-map
    nil '(:ignore t :which-key "json")
    "d" 'json-decrement-number-at-point
    "f" 'json-mode-beautify
    "i" 'json-increment-number-at-point
    "k" 'json-nullify-sexp
    "p" 'json-mode-show-path
    "P" 'json-mode-kill-path
    "t" 'json-toggle-boolean))

(use-package auctex
  :defer t)

(use-package company-auctex
  :defer t
  :after company)

(use-package company-reftex
  :defer t
  :after company)

(use-package company-math
  :defer t
  :after company)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package nxml-mode
  :straight nxml

  :hook
  (nxml-mode . hs-minor-mode)

  :general
  (normal nxml-mode-map "TAB" 'hs-toggle-hiding)
  (medivhok/local-leader-def 'normal
    nxml-mode-map
    nil '(:ignore t :which-key "xml")
    "t" 'hs-toggle-hiding)

  :config
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                "<!--\\|<[^/>]*[^/]>"
                "-->\\|</[^/>]*[^/]>"
                "<!--"
                nxml-forward-element
                nil)))
