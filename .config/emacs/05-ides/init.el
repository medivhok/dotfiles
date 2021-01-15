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

(use-package helm-lsp
  :after
  (helm lsp-mode))

(use-package lsp-mode
  :defer t
  :after which-key
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

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

(use-package css-mode
  :defer t

  :after
  (flycheck)

  :init
  (setq flycheck-css-csslint-executable "~/.yarn/bin/csslint"))

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
  :defer t
  :custom
  (TeX-engine 'luatex))

(use-package company-auctex
  :defer t
  :after company)

(use-package company-reftex
  :defer t
  :after company)

(use-package company-math
  :defer t
  :after company)

(use-package ox-latex
  :defer t
  :after org
  :straight org
  :config
  (setq org-latex-caption-above nil
        org-latex-compiler "lualatex"
        org-latex-listings 'minted
        org-latex-minted-options
        '(("frame" "none")
          ("baselinestretch" "1.2")
          ("style" "emacs")
          ("breaklines"))
        org-latex-packages-alist
        '(("" "fontspec" t ("lualatex"))
          ("AUTO" "babel" t ("pdflatex" "lualatex"))
          ("" "booktabs" t)
          ("" "fancyhdr" t)
          ("framemethod=tikz" "mdframed")
          ("" "minted")
          ("" "xcolor"))
        org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-minted-langs '(R "r")))

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
