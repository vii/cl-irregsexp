(in-package #:cl-irregsexp)

(defparameter *optimize-unsafe* 
  `(optimize speed (safety 0)))

#- (and)
(defparameter *optimize-unsafe* 
  `(optimize (safety 3) debug (speed 0)))


