;;; exwm.el --- My exwm configurations -*- lexical-binding: t; -*-

;; Author: Jean Gregory Verret <gregory.verret@gmail.com>
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my exwm configuration.

;;; Code:

(use-package exwm
  :init
  (setq mouse-autoselect-window nil
        focus-follow-mouse t
        exwm-workspace-warp-cursor t)
  :config
  (use-package exwm-randr
    :after
    (exwm)
  
    :commands
    (exwm-randr-enable))
  (exwm-randr-enable)

  (use-package exwm-systemtray
    :after
    (exwm)
  
    :commands
    (exwm-systemtray-enable))
  (exwm-systemtray-enable)

  (exwm-enable))
