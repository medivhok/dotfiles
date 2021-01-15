(defconst medivhok:config-directory
          (file-name-as-directory user-emacs-directory)
    "The root directory containing the configuration files.")

(defconst medivhok:module-init-file-name "init.el"
  "The name of the configuration file of a module to be loaded.")

(defconst medivhok:bootstrap-module-directory
  (file-name-as-directory
   (expand-file-name "01-bootstrap" medivhok:config-directory))
  "The directory containing the bootstrap module configuration files.")

(load-file (expand-file-name medivhok:module-init-file-name
                             medivhok:bootstrap-module-directory))

(defconst medivhok:exwm-module-directory
  (file-name-as-directory
   (expand-file-name "02-exwm" medivhok:config-directory))
  "The exwm module configurations directory.")

;;(load-file (expand-file-name module-init-file-name
;;                             medivhok:exwm-module-directory))

(defconst medivhok:ui-module-directory
  (file-name-as-directory
   (expand-file-name "03-ui" medivhok:config-directory)))

(load-file (expand-file-name medivhok:module-init-file-name medivhok:ui-module-directory))

(defconst medivhok:app-module-directory
  (file-name-as-directory (expand-file-name "04-applications" medivhok:config-directory)))

(load-file (expand-file-name medivhok:module-init-file-name medivhok:app-module-directory))

(defconst medivhok:ides-module-directory
 (file-name-as-directory (expand-file-name "05-ides" medivhok:config-directory)))

(load-file (expand-file-name medivhok:module-init-file-name medivhok:ides-module-directory))
