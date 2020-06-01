;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:


;; All of Doom's packages are pnned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
;(package! pinned-package :pin nil)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Let company use child frame as its candidate menu.
;;   https://github.com/tumashu/company-posframe
(package! company-posframe)

;; An extensible emacs dashboard.
;;   https://github.com/emacs-dashboard/emacs-dashboard
(package! dashboard)

;; Fast, friendly searching with ripgrep and Emacs.
;;   https://github.com/Wilfred/deadgrep
(package! deadgrep)

;; Collection of useful dired additions (dired-hacks).
;;   https://github.com/Fuco1/dired-hacks#dired-narrow
(package! dired-narrow)

;; Kill & Mark Things Easily in Emacs.
;;   https://github.com/leoliu/easy-kill
(package! easy-kill)

;; Future-proof your Emacs Lisp customizations!
;;   https://github.com/raxod502/el-patch
(package! el-patch
  :recipe (:host github :repo "raxod502/el-patch" :branch "master"))

(package! org-pdftools
  :recipe (:host github :repo "fuxialexander/org-pdftools" :branch "master"))

;; Language-specific refactoring in Emacs.
;;   https://github.com/Wilfred/emacs-refactor
(package! emr)

;; Make Emacs use the $PATH set up by the user's shell.
;;   https://github.com/purcell/exec-path-from-shell
(package! exec-path-from-shell)

;; Flycheck syntax checker using clang-tidy.
;;   https://github.com/ch1bo/flycheck-clang-tidy
(package! flycheck-clang-tidy)

;; Colorize color names in buffers.
;;   http://elpa.gnu.org/packages/rainbow-mode.html
(package! rainbow-mode)
