(require 'slip-box-helpers)

(describe "slip-box-helpers"

(describe "medivhok/roam-assert-tag"
  (it "should return t if TAG is a valid tag"
    (expect (medivhok/roam-assert-tag "test") :to-be t)
    (expect (medivhok/roam-assert-tag "@test") :to-be t))

  (it "should throw if TAG is not a valid tag"
    (expect (medivhok/roam-assert-tag nil) :to-throw 'wrong-type-argument)))
(describe "medivhok/roam-plain-tag-p"
  (it "should return t if tag does not start with '@'"
    (expect (medivhok/roam-plain-tag-p "test") :to-be t))

  (it "should return nil if tag starts with '@'"
    (expect (medivhok/roam-plain-tag-p "@test") :to-be nil))

  (it "should throw for an invalid tag"
    (expect (medivhok/roam-plain-tag-p nil) :to-throw 'wrong-type-argument)))
(describe "medivhok/roam-arroba-tag-p"
  (it "should return t if tag starts with '@' followed by at least one character"
    (expect (medivhok/roam-arroba-tag-p "@test") :to-be t))

  (it "should return nil if tag does not start with '@'"
    (expect (medivhok/roam-arroba-tag-p "test") :to-be nil))

  (it "should throw with an invalid tag"
    (expect (medivhok/roam-arroba-tag-p "@") :to-throw 'wrong-type-argument)))
(describe "medivhok/roam-plain-tag"
  (it "should return the 'plain' version of the tag"
    (expect (medivhok/roam-plain-tag "@test") :to-equal "test")
    (expect (medivhok/roam-plain-tag "test") :to-equal "test"))

  (it "should throw for an invalid tag"
    (expect (medivhok/roam-plain-tag nil) :to-throw 'wrong-type-argument)))
(describe "medivhok/roam-arroba-tag"
  (it "should return the arroba version of the tag"
    (expect (medivhok/roam-arroba-tag "test") :to-equal "@test")
    (expect (medivhok/roam-arroba-tag "@test") :to-equal "@test"))

  (it "should throw for an invalid tag"
    (expect (medivhok/roam-arroba-tag nil) :to-throw 'wrong-type-argument)))
)
