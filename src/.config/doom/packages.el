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

;;
;; Make Emacs use the $PATH set up by the user's shell.
;;
;;   https://github.com/purcell/exec-path-from-shell
;;
(package! exec-path-from-shell)

(package! bibtex-completion
  :recipe (:host github :repo "tmalsburg/helm-bibtex"))

;;
;; Log messages in elisp with information about where the log call is located
;; in source.
;;
;;   https://github.com/jordonbiondo/call-log
;;
(package! call-log
  :recipe (:host github :repo "jordonbiondo/call-log"))

;;
;; Flycheck syntax checker using clang-tidy.
;;
;;   https://github.com/ch1bo/flycheck-clang-tidy
;;
(package! flycheck-clang-tidy)

;;
;; Colorize color names in buffers.
;;
;;   http://elpa.gnu.org/packages/rainbow-mode.html
;;  
(package! rainbow-mode)

;;
;; citations, cross-references, indexes, glossaries and bibtex utilities for
;; org-mode
;;
;;   https://github.com/jkitchin/org-ref
;;
(package! org-ref)

;;
;; Convenience functions to work with emacs org mode clocking.
;;
;;   https://github.com/dfeich/org-clock-convenience
;;  
(package! org-clock-convenience)

;;
;; Caldav sync for Emacs Orgmode
;;
;;   https://github.com/dengste/org-caldav
;;  
(package! org-caldav
  :recipe (:host github :repo "grauschnabel/org-caldav"))

;;
;; https://orgmode.org/worg/org-contrib/
;;
(package! org-plus-contrib)

;;
;; org-roam-bibtex is a library which offers a tighter integration between
;; org-roam, helm-bibtex, and org-ref. It allows users to access their
;; bibliographical notes in org-roam-directory via helm-bibtex, ivy-bibtex, or
;; by opening org-ref’s cite: links and running 3. Add notes. If the note does
;; not exist, it is created.
;;
;;  https://github.com/org-roam/org-roam-bibtex
;;
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(unpin! org-roam
        company-org-roam
        bibtex-completion
        helm-bibtex
        ivy-bibtex)

;;
;; A Web Application to Visualize the Org-Roam Database.
;;
;;  https://github.com/org-roam/org-roam-server
;; 
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
