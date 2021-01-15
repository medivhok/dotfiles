(defconst medivhok:menu:leader "SPC"
  "The leader key.")

(defconst medivhok:menu:nn-leader "M-SPC"
  "The 'non normal state' leader key.")

(defun medivhok:menu/prepend-leader (menu-key &optional use-non-normal-leader)
  "Returns a string of the MENU-KEY prefixed by `medivhok:menu:leader' and
a space. If USE-NON-NORMAL-LEADER is non nil, `medivhok:menu:nn-leader'
is used instead."
  (if use-non-normal-leader
      (concat medivhok:menu:nn-leader " " menu-key)
    (concat medivhok:menu:leader " " menu-key)))

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
  :defer t
  :after evil

  :config
  (setq evil-collection-outline-bind-tab-p nil))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package hydra)

(use-package general
  :config
  (general-evil-setup t))

(defhydra hydra-zoom ()
  "zoom"
  ("-" text-scale-decrease "out")
  ("=" text-scale-increase "in"))

(general-create-definer medivhok:menu:main
  :prefix medivhok:menu:leader
  :non-normal-prefix medivhok:menu:nn-leader
  :keymaps 'override)

(medivhok:menu:main
  :states 'normal
  ":" 'execute-extended-command
  "-" '(hydra-zoom/text-scale-decrease
        :which-key "text-scale-decrease")
  "=" '(hydra-zoom/text-scale-increase
        :which-key "text-scale-increase"))

(general-create-definer medivhok:menu:agenda
  :prefix (medivhok:menu/prepend-leader "a")
  :non-normal-prefix (medivhok:menu/prepend-leader "a" t)
  :keymaps 'override
  nil '(:ignore t :which-key "agenda"))

(general-create-definer medivhok:menu:buffer
  :prefix (medivhok:menu/prepend-leader "b")
  :non-normal-prefix (medivhok:menu/prepend-leader "b" t)
  :keymaps 'override
  nil '(:ignore t :which-key "buffer"))

(medivhok:menu:buffer
  :states 'normal
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "d" 'kill-current-buffer)

(general-create-definer medivhok:menu:emacs
  :prefix (medivhok:menu/prepend-leader "e")
  :non-normal-prefix (medivhok:menu/prepend-leader "e" t)
  :keymaps 'override
  nil '(:ignore t :which-key "emacs"))

(medivhok:menu:emacs
  :states 'normal
  "e" '((lambda ()
          (interactive)
          (find-file
           (expand-file-name "README.org"
                             (file-name-directory medivhok:cfg-directory))))
        :which-key "edit literate config"))

(general-create-definer medivhok:menu:file
  :prefix (medivhok:menu/prepend-leader "f")
  :non-normal-prefix (medivhok:menu/prepend-leader "f" t)
  :keymaps 'override
  nil '(:ignore t :which-key "file"))

(medivhok:menu:file
  :states 'normal
  "f" 'find-file
  "r" 'counsel-recentf)

(general-create-definer medivhok:menu:help
  :prefix (medivhok:menu/prepend-leader "h")
  :non-normal-prefix (medivhok:menu/prepend-leader "h" t)
  :keymaps 'override
  nil '(:ignore t :which-key "help"))

(medivhok:menu:help
  :states 'normal
  "a" 'apropos-command
  "b" 'describe-bindings
  "c" 'describe-face
  "f" 'describe-function
  "i" 'info
  "k" 'general-describe-keybindings
  "s" 'counsel-describe-symbol
  "v" 'describe-variable)

(general-create-definer medivhok:menu:local
  :prefix (medivhok:menu/prepend-leader "m")
  :non-normal-prefix (medivhok:menu/prepend-leader "m" t)
  :keymaps 'override)

(general-create-definer medivhok:menu:notes
  :prefix (medivhok:menu/prepend-leader "n")
  :non-normal-prefix (medivhok:menu/prepend-leader "n" t)
  :keymaps 'override
  nil '(:ignore t :which-key "notes"))

(general-create-definer medivhok:menu:window
  :prefix (medivhok:menu/prepend-leader "w")
  :non-normal-prefix (medivhok:menu/prepend-leader "w" t)
  :keymaps 'override
  nil '(:ignore t :which-key "window"))

(medivhok:menu:window
  :states 'normal
  "q" 'delete-window
  "s" 'split-window-below
  "\\" 'split-window-right)

(general-create-definer medivhok:menu:quit
  :prefix (medivhok:menu/prepend-leader "q")
  :non-normal-prefix (medivhok:menu/prepend-leader "q" t)
  :keymaps 'override
  nil '(:ignore t :which-key "quit"))

(medivhok:menu:quit
  :states 'normal
  "q" 'save-buffers-kill-terminal)
