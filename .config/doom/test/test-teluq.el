;; -*- no-byte-compile: t; -*-
;;; gtd/teluq/test/test-teluq.el

;; (load! "../config")

;; (describe "gtd/teluq"

;;   (describe "teluq--get-session"
;;     (it "returns t for integer 1 to 15"
;;       (expect (teluq--get-session "2020-09-10")))))

(defun medivhok/my-test ()
  (interactive)
  (> 10 1))

(describe "medivhok/my-test"
  (it "returns true"
    (expect t :to-be t)))
