(in-package #:cl-irregsexp)

(5am:def-suite split :in :cl-irregsexp)
(5am:in-suite split)

(5am:test split-char 
  (5am:is (equalp (list "ab" "c" "d" "e" "") (match-split #\- "ab-c-d-e--"))))
