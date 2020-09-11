;;; gtd/teluq/config.el -*- lexical-binding: t; -*-

(defconst teluq--session-weeks-count 15
  "The number of weeks in a session.")

(defconst teluq--session-days-count (* teluq--session-weeks-count 7)
  "The number of days in a session.")

(define-widget 'teluq-session 'lazy
  ""
  :type '(cons string string string))

(defcustom teluq-session-list '(("Automne 2020" "A20" "2020-09-01"))
  "A list of the sessions."
  :type 'cons)

(defun teluq--session-dates (session)
  ""
  (let ((session-start (date-to-time (nth 2 session))))
    (cons session-start (time-add session-start
                                  (days-to-time teluq--session-days-count)))))

(defun teluq--get-session (&optional target-date)
  "Return the teluq session for TARGET-DATE."
  (let ((found-session))
    (dolist (session teluq-session-list found-session)
      (let ((target-date (date-to-time target-date))
            (session-dates (teluq--session-dates session)))
        (if (and (time-less-p (nth 0 session-dates) target-date)
                 (time-less-p target-date (nth 1 session-dates)))
            (progn
              (setq found-session session)
              (return)))))))

;; (defun +teluq/week-number (&optional target-date)
;;   "Returns the session week number of TARGET-DATE (current-date
;; is used if TARGET-DATE is nil) or nil if TARGET-DATE
;; is not in a session period."
;;   (let ((session-days-count (* 15 7)))
;;     ))

;; (defun +teluq--session-week-number-p (week-number)
;;   "Returns t if WEEK-NUMBER is an integer between 1 and 15."
;;   (and (integerp week-number)
;;        (> week-number 0)
;;        (< week-number 16)))

;; (defun +teluq--week-start-time (session-start-date week-number)
;;   "Returns the date of the first day of WEEK-NUMBER."
;;   (let ((week-multiplier (1- week-number)))
;;     (time-add (teluq--session-start-time)
;;             (days-to-time (* week-multiplier 7)))))


;; (defun teluq-session--date-in-week-p (date week)
;;   ""
;;   (let ((week-start (teluq-session--week-start-time)))
;;     (and (not (time-less-p date week-start))
;;          (time-less-p date (time-add week-start (days-to-time 7))))))

;; (defun teluq-session-current-week ()
;;   "Returns the current week of the session (1-15) or 0 if
;; currently not in a session."
;;   )

;; (defun teluq-session-get-first-day-of-week (session-week)
;;   ""
;;   (if (not (and (integerp session-week)
;;                   (or (< session-week 0)
;;                       (> session-week 16))))))

;;   (defun medivhok/is-time-in-session-week (session-day session-start target-week)
;;     "Returns t if TARGET-TIME is in TARGET-WEEK"
;;     (let ((target-week-start (time-add session-start
;;                                        (days-to-time (* (- target-week 1) 7)))))
;;       (and (not (time-less-p session-day target-week-start))
;;            (time-less-p session-day (time-add target-week-start (days-to-time 7))))))

;;   (medivhok/is-time-in-session-week (date-to-time "2020-08-01")
;;                                     (date-to-time "2020-09-01")
;;                                     1)

;;   (not (time-less-p (date-to-time "2020-08-01") (date-to-time "2020-09-01")))

;;   (defun medivhok/get-session-week (&optional first-session-day current-day)
;;     "Returns an integer representing the current session week (1-15)."
;;     (interactive)
;;     (if (null first-session-day)
;;         (setq first-session-day (date-to-time "2020-09-01")))
;;     (if (null current-day)
;;         (setq current-day (current-time)))
;;     (cond ((and (not (time-less-p target-time medivhok/session-week-01))
;;                 (time-less-p target-time medivhok/session-week-02)) "Semaine 01")
;;           ((and (not (time-less-p target-time medivhok/session-week-02))
;;                 (time-less-p target-time medivhok/session-week-03)) "Semaine 02")
;;           ((and (not (time-less-p target-time medivhok/session-week-03))
;;                 (time-less-p target-time medivhok/session-week-04)) "Semaine 03")
;;           ((and (not (time-less-p target-time medivhok/session-week-04))
;;                 (time-less-p target-time medivhok/session-week-05)) "Semaine 04")
;;           ((and (not (time-less-p target-time medivhok/session-week-05))
;;                 (time-less-p target-time medivhok/session-week-06)) "Semaine 05")
;;           ((and (not (time-less-p target-time medivhok/session-week-06))
;;                 (time-less-p target-time medivhok/session-week-07)) "Semaine 06")
;;           ((and (not (time-less-p target-time medivhok/session-week-07))
;;                 (time-less-p target-time medivhok/session-week-08)) "Semaine 07")
;;           ((and (not (time-less-p target-time medivhok/session-week-08))
;;                 (time-less-p target-time medivhok/session-week-09)) "Semaine 08")
;;           ((and (not (time-less-p target-time medivhok/session-week-09))
;;                 (time-less-p target-time medivhok/session-week-10)) "Semaine 09")
;;           ((and (not (time-less-p target-time medivhok/session-week-10))
;;                 (time-less-p target-time medivhok/session-week-11)) "Semaine 10")
;;           ((and (not (time-less-p target-time medivhok/session-week-11))
;;                 (time-less-p target-time medivhok/session-week-12)) "Semaine 11")
;;           ((and (not (time-less-p target-time medivhok/session-week-12))
;;                 (time-less-p target-time medivhok/session-week-13)) "Semaine 12")
;;           ((and (not (time-less-p target-time medivhok/session-week-13))
;;                 (time-less-p target-time medivhok/session-week-14)) "Semaine 13")
;;           ((and (not (time-less-p target-time medivhok/session-week-14))
;;                 (time-less-p target-time medivhok/session-week-15)) "Semaine 14")
;;           ((and (not (time-less-p target-time medivhok/session-week-15))
;;                 (time-less-p target-time (time-add medivhok/session-week-15
;;                                                    (days-to-time 7)))) "Semaine 15")
;;           (t "Hors-session")))

;;   (message "On est la semaine : %s" (medivhok/get-session-week-name (date-to-time "2020-09-03")))

;;; gtd/teluq.el ends here
