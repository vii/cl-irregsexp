(in-package #:cl-irregsexp)

(5am:def-suite helpers :in :cl-irregsexp)
(5am:in-suite helpers)

(5am:test literal
  (with-match ("hello")
    (5am:finishes 
      (literal "hell")))
  (5am:signals match-failed 
    (with-match ("goodbye")
      (literal "xoodbye")
      (5am:fail))))

(5am:test match-multiple
  (with-match ("llama")
    (5am:finishes
      (match-multiple () (literal "l"))
      (literal "ama")))
  (with-match ("ama")
    (5am:finishes
      (match-multiple () (literal "l"))
      (literal "ama")))
  (with-match ("lama")
    (5am:finishes
      (match-multiple () (literal "l"))
      (literal "ama")))
  (with-match ("lllllllllllllllama")
    (5am:finishes
      (match-multiple () (literal "l"))
      (literal "ama")))

  (with-match ("lllama")
    (5am:finishes
      (match-multiple (2 nil)
		      (literal "l"))
      (literal "lama")))
  (with-match ("fish fish fish fish frog")
    (5am:finishes
      (match-multiple (2 1)
		      (literal "fish "))
      (literal "fish frog")))

  (with-match ("xxxxx")
    (5am:finishes
      (match-multiple ()
		      (literal "xtreme"))
      (literal "xxxxx"))))

(5am:test match-end
  (with-match ("")
    (5am:finishes 
      (match-end)))
  (with-match ("boat")
    (5am:finishes
      (try-match 
	(match-end)
	(5am:fail))
      (literal "bo")
      (try-match 
	(match-end)
	(5am:fail))
      (literal "at")
      (match-end))))



(5am:test match-element-range
  (with-match ("word")
    (5am:finishes
      (match-multiple () (match-element-range #\a #\z))
      (match-end)))
  (5am:signals match-failed 
    (with-match ("Xword")
      (match-multiple (1) (match-element-range #\a #\z))
      (5am:fail))))


(5am:test match-not
  (with-match ("not")
    (5am:finishes
      (match-not (match-end))
      (match-not (literal "note"))
      (match-not (match-not (literal "n")))
      (literal "ot"))))

(5am:test match-integer
  (5am:signals match-failed 
    (with-match (" ")
      (match-integer)
      (5am:fail)))
  (5am:signals match-failed 
    (with-match ("- ")
      (match-integer)
      (5am:fail)))
  (with-match ("1")
    (5am:finishes
      (5am:is (= 1 (match-integer)))))
  (with-match ("-1")
    (5am:finishes
      (5am:is (= -1 (match-integer)))))
  (with-match ("10000")
    (5am:finishes
      (5am:is (= 10000 (match-integer)))))
  (with-match ("-1234567890")
    (5am:finishes
      (5am:is (= -1234567890 (match-integer)))))
  (with-match ("ffff")
    (5am:finishes
      (5am:is (= #xffff (match-integer 16)))))
  (with-match ("1234aBcDeFG")
    (5am:finishes
      (5am:is (= #x1234ABCDEF (match-integer 16)))
      (literal "G")
      (match-end))))
