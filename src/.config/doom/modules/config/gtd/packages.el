;; -*- no-byte-compile: t; -*-
;;; conf/gtd/packages.el

;; Convenience functions to work with emacs org mode clocking.
;;   https://github.com/dfeich/org-clock-convenience
(package! org-clock-convenience)

;; Org sync with Google Calendar.
;;   https://github.com/kidd/org-gcal.el
(package! org-gcal)

;; A simple org-mode based journaling mode.
;;   https://github.com/bastibe/org-journal
(package! org-journal)

;; https://orgmode.org/worg/org-contrib/
(package! org-plus-contrib)

;; Rudimentary Roam replica with Org-mode.
;;   https://github.com/org-roam/org-roam
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))

;; Company completion backend for Org-roam.
;;   https://github.com/jethrokuan/company-org-roam
(package! company-org-roam
  :recipe (:host github :repo "org-roam/company-org-roam"))
