;;; init.el --- My Emacs Config -*- lexical-binding: t; -*-

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

;; Keep the transient cruft out of the configuration directory.
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

(use-package diminish)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t)

  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil

  :commands
  (evil-collection-init)

  :custom
  (evil-collection-outline-bind-tab-p nil))

(use-package which-key
  :init
  (which-key-mode)

  :diminish
  which-key-mode

  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :after (evil which-key)

  :config
  (general-evil-setup t)

  (general-create-definer medivhok/local-leader-def
    :prefix "SPC m")

  (medivhok/local-leader-def
   :states 'normal
   :keymaps 'org-mode-map
   nil '(:ignore t :which-key "org")
    "i"  '(:ignore t :which-key "insert")
    "il" '(org-insert-link :which-key "insert link")
  
    "n"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
  
    "s"  '(dw/counsel-rg-org-files :which-key "search notes")
  
    "a"  '(org-agenda :which-key "status")
    "c"  '(org-capture t :which-key "capture")
    "x"  '(org-export-dispatch t :which-key "export"))

  (general-def 'normal
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :prefix-map 'medivhok/leader-map
    ":" 'execute-extended-command
    "b" '(:ignore t :which-key "buffer")
    "f" '(:ignore t :which-key "file")
    "h" '(:ignore t :which-key "help")
    "n" '(:ignore t :which-key "notes")
    "w" '(:ignore t :which-key "window")
    "q" '(:ignore t :which-key "quit"))

  (general-def 'medivhok/leader-map
    :prefix "b"
    :prefix-map 'medivhok/buffer-map
    "b" 'switch-to-buffer
    "k" 'kill-buffer
    "q" 'kill-current-buffer)

  (general-def 'medivhok/leader-map
    :prefix "n"
    :prefix-map 'medivhok/notes-map)

  (general-def 'medivhok/leader-map
    :prefix "f"
    :prefix-map 'medivhok/file-map
    "f" 'find-file
    "r" 'counsel-recentf)

  (general-def 'medivhok/leader-map
    :prefix "h"
    :prefix-map 'medivhok/help-map
    "a" 'apropos-command
    "b" 'describe-bindings
    "f" 'describe-function
    "s" 'counsel-describe-symbol
    "v" 'describe-variable)

  (general-def 'medivhok/leader-map
    :prefix "w"
    :prefix-map 'medivhok/window-map
    "q" 'delete-window
    "s" 'split-window-below)

  (general-def 'medivhok/leader-map
    :prefix "q"
    :prefix-map 'medivhok/quit-map
    "q" 'save-buffers-kill-terminal))

(use-package ivy
  :diminish
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package swiper
  :after ivy)

(use-package counsel
  :after swiper
  :diminish
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :after counsel

  :config
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :after ivy

  :diminish ivy-posframe-mode

  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

(defconst medivhok/root-directory "~/org/"
  "The root directory for my 'workflows' files.")

(defconst medivhok/agenda-directory
  (file-name-as-directory
   (expand-file-name "agenda" medivhok/root-directory))
  "The directory of my agenda files.")

(defconst medivhok/gtd-file
  (expand-file-name "gtd.org" medivhok/agenda-directory)
  "My 'getting things done' agenda file.")

(defconst medivhok/roam-directory
  (file-name-as-directory
   (expand-file-name "roam" medivhok/root-directory))
  "The directory of my roam files.")

(defconst medivhok/slip-box
  (file-name-as-directory
   (expand-file-name "slip-box" medivhok/roam-directory))
  "The directory containing the notes of my slip-box")

(defconst medivhok/pdf-root-directory
  (file-name-as-directory
   (expand-file-name "readings" medivhok/root-directory))
  "The root directory of my PDF files.")

(defconst medivhok/bibtex-file
  (expand-file-name "zotero.bib" medivhok/pdf-root-directory)
  "My bibtex file, generated by 'zotero'.")

(defconst medivhok/bibliographic-notes-directory
  (file-name-as-directory
   (expand-file-name "biblio-box" medivhok/roam-directory))
  "The directory of my bibliographic notes.")

(setq inhibit-startup-message t)
(global-prettify-symbols-mode 1)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(tooltip-mode -1)

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

(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq-default fill-column 80)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (agenda . 5)))
  (evil-collection-init 'dashboard))

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

(use-package all-the-icons)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package org
  :straight org-plus-contrib

  :hook
  (org-mode . medivhok/org-mode-setup)

  :custom-face
  (org-link ((t (:inherit link :underline nil))))

  :config
  (setq org-catch-invisible-edits 'show
        org-cycle-separator-lines 2
        org-edit-src-content-indentation 0
        org-ellipsis " ▾"
        org-hide-block-startup nil
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-outline-path-complete-in-steps nil
        org-return-follows-link t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-src-tab-acts-natively t
        org-startup-folded t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ledger . t)))

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
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun medivhok/org-babel-tangle-dont-ask ()
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(defun medivhok/org-mode-setup ()
  "Turn on indentation and auto-fill mode for Org files."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode)

  ;; Automatically tangle on save.
  (add-hook 'after-save-hook
            #'medivhok/org-babel-tangle-dont-ask
            'run-at-end
            'only-in-org-mode))

(use-package evil-org
  :after org

  :hook
  ((org-mode . evil-org-mode)
   (org-agenda-mode . evil-org-mode)
   (evil-org-mode . (lambda ()
                      (evil-org-set-key-theme '(navigation
                                                todo
                                                insert
                                                textobjects
                                                additional)))))

  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(medivhok/local-leader-def
 :states 'normal
 :keymaps 'org-mode-map
 nil '(:ignore t :which-key "org")
  "i"  '(:ignore t :which-key "insert")
  "il" '(org-insert-link :which-key "insert link")

  "n"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "s"  '(dw/counsel-rg-org-files :which-key "search notes")

  "a"  '(org-agenda :which-key "status")
  "c"  '(org-capture t :which-key "capture")
  "x"  '(org-export-dispatch t :which-key "export"))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-make-toc
  :hook
  (org-mode . org-make-toc-mode))

(use-package org-agenda
  :straight org-plus-contrib

  :commands (org-agenda)

  :bind
  (("<f1>" . (lambda () (interactive) (org-agenda nil " "))))

  :config
  (setq org-agenda-block-separator nil
        org-agenda-dim-blocked-tasks 'invisible
        org-agenda-files (list medivhok/gtd-file)
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-with-log-mode t

        org-agenda-custom-commands
        `((" " "GTD Agenda"
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
  :straight org-plus-contrib

  :commands (org-capture)
  :bind
  (("<f4>" . (lambda () (interactive) (org-capture nil "i"))))

  :config
  (setq org-capture-templates
        `(("i" "inbox" entry
           (file+headline ,medivhok/gtd-file "Inbox")
           "* TODO [#C] %?\n:PROPERTIES:\n:Effort: 1\n:END:\n")

          ("e" "email" entry
           (file+headline ,medivhok/gtd-file "Emails")
           "* TODO [#A] Reply: %a"
           :immediate-finish t)))))

(use-package org-refile
  :straight org-plus-contrib

  :config
  (setq org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-targets '((nil :tag . "@tâches")
                             (nil :tag . "@cours")
                             (nil :tag . "@projet")
                             (nil :tag . "@teluq"))))

(use-package org-roam
  :after org-capture

  :straight
  (org-roam :host github :repo "org-roam/org-roam")

  :commands
  (org-roam-buffer-toggle-display
   org-roam-capture
   org-roam-find-file
   org-roam-graph
   org-roam-insert)

  :bind
  (:map medivhok/notes-map
        ("f" . org-roam-find-file)
        ("g" . org-roam-graph)
        ("i" . org-roam-insert)
        ("r" . org-roam-buffer-toggle-display))

  :init
  (setq org-roam-directory medivhok/roam-directory)

  (defconst medivhok/roam-templates-directory
    (file-name-as-directory
     (expand-file-name "templates" medivhok/roam-directory))
    "The directory containing the templates for my notes.")

  :config
  (setq org-roam-graph-exclude-matcher '("setup")
        org-roam-index-file "index_file.org"
        org-roam-tag-sources '(prop)
        org-roam-title-sources '(title alias))

  (setq org-roam-capture-templates
        `(("c" "slip-box card" plain
           (function org-roam--capture-get-point)
           (file ,(expand-file-name "slip-box-card.org"
                                    medivhok/roam-templates-directory))
           :file-name "slip-box/${slug}"
           :head ""
           :unnarrowed t)

          ("b" "bibliographic notes" plain
           (function org-roam--capture-get-point)
           (file ,(expand-file-name "biblio-box-entry.org"
                                    medivhok/roam-templates-directory))
           :file-name "biblio-box/${slug}"
           :head ""
           :unnarrowed t)))

  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n- source :: ${ref}"
           :unnarrowed t))))

(setq-default tab-width 2)
(setq-default evil-shift-with tab-width)

(setq-default indent-tabs-mode nil)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

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

(use-package magit
  :commands
  (magit-status magit-get-current-branch)

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-execpt-diff-v1))

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :diminish

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
  :diminish projectile-mode

  :config
  (projectile-mode))

(use-package counsel-projectile
  :after projectile)

(use-package bibtex-completion
  :custom
  (bibtex-completion-additional-search-fields '(keywords))
  (bibtex-completion-bibliography (list medivhok/bibtex-file))
  (bibtex-completion-cite-default-as-initial-input t)
  (bibtex-completion-notes-path medivhok/bibliographic-notes-directory)
  (bibtex-completion-pdf-field "file"))

(use-package ivy-bibtex
  :after bibtex-completion

  :commands
  (ivy-bibtex)

  :bind
  ("<f3>" . ivy-bibtex))

(use-package org-ref
  :after
  (org ivy-bibtex)

  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-filename-helm-bibtex
        org-ref-default-bibliography bibtex-completion-bibliography
        org-ref-note-title-format (concat "* TODO %y - %t\n"
                                     "  :PROPERTIES:\n"
                                     "  :Custom_ID: %k\n"
                                     "  :NOTER_DOCUMENT: %F\n"
                                     "  :ROAM_KEY: cite:%k\n"
                                     "  :JOURNAL: %j\n"
                                     "  :YEAR: %y\n"
                                     "  :VOLUME: %v\n"
                                     "  :PAGES: %p\n"
                                     "  :DOI: %D\n"
                                     "  :URL: %U\n"
                                     "  :END:\n\n")
        org-ref-notes-directory medivhok/bibliographic-notes-directory))

(use-package org-roam-bibtex
  :after
  (org-roam ivy-bibtex)

  :straight
  (org-roam-bibtex :host github :repo "org-roam/org-roam-bibtex")

  :hook
  (org-roam-mode . org-roam-bibtex-mode)

  :config
  (setq org-ref-notes-function 'orb-edit-notes
        orb-preformat-keywords '(("citekey" . "=key=")
                                 "title"
                                 "url"
                                 "file"
                                 "author-or-editor"
                                 "keywords")
        orb-templates `(("r" "ref" plain (function org-roam-capture--get-point)
                         ""
                         :file-name "${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${ref}\n"
                                  "#+ROAM_TAGS: ${keywords}\n\n"
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
                         :file-name "${citekey}"
                         :head
                         ,(concat "#+TITLE: ${title}\n"
                                  "#+ROAM_KEY: ${url}\n\n"
                                  "* Notes\n"
                                  ":PROPERTIES:\n"
                                  ":Custom_ID: ${citekey}\n"
                                  ":URL: ${url}\n"
                                  ":END:\n\n")
                         :unnarrowed t))))

(use-package sqlite3)
