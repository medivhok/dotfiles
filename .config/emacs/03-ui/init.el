(load-file (expand-file-name "+keybindings.el" medivhok:ui-module-directory))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-theme-set-faces
 'user
 `(default ((t (:family "Roboto Mono" :height 140 :weight light))))
 `(fixed-pitch ((t (:family "FiraCode" :height 130 :weight light)))))

(use-package all-the-icons

:preface
(global-prettify-symbols-mode 1)

)

(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

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
;; (global-display-line-numbers-mode t)
(column-number-mode)

(setq visible-bell t)

(global-hl-line-mode t)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(setq-default fill-column 80)

(scroll-bar-mode -1)
(tooltip-mode -1)

;; We disable the tool and menu bar.
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package dashboard
  :preface
  (setq inhibit-startup-message t)

  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-items '((recents . 5)
                          (agenda . 5)))
  (evil-collection-init 'dashboard))

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
