(in-package #:cl-irregsexp)


(defmacro match-split (pattern string)
  (with-unique-names (before result)
  `(let (,result)
     (match-bind ((+ (or (last) (progn ,before (or (last) ,pattern) '(push ,before ,result)))))
	 ,string)
     (nreverse ,result))))
