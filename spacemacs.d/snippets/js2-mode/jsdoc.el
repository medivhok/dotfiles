# -*- mode: snippet -*- 
# key: jsdoc-module
# name: jsdoc module comment block
# group: jsdoc
# type: command
# contributor: medivhok <medivhok@zoho.com>
# --
(setq-local jsdoc-header-snippet "/**\n * ${1:default Description}\n$0")
(setq-local jsdoc-description-snippet "")
(setq-local jsdoc-footer-snippet " */\n$0")

(let ((yas-after-exit-snippet-hook
     (lambda () "footer snippet" (yas-expand-snippet jsdoc-footer-snippet))))
  yas-expand-snippet jsdoc-header-snippet)
