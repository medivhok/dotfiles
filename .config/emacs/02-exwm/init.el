(defconst medivhok:exwm-enabled
  (and (eq window-system 'x)
       (seq-contains command-line-args "--use-exwm")))

(use-package exwm
  :if medivhok:exwm-enabled
  :init
  (setq exwm-layout-show-all-buffers t
        exwm-workspace-number 5
        exwm-workspace-show-all-buffers t
        exwm-workspace-warp-cursor t
        focus-follows-mouse t
        mouse-autoselect-window nil)

  :config
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DP-1"))
  (exwm-randr-enable)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
          ([?\s-w] . exwm-worspace-switch)
          ([?\s-p] . counsel-linux-app)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-enable))
