(in-package #:cl-irregsexp)

(defun-speedy concatenate-sequences (prototype sequences)
  (etypecase prototype
    (string
     (let ((*print-pretty* nil))
       (with-output-to-string (out)
         (dolist (el sequences)
           (write-string el out)))))
    (byte-vector
     (cond
       ((rest sequences)
        (let ((len (loop
                      :for s :in sequences
                      :summing (length (the byte-vector s)))))
          (let ((ret (make-byte-vector len)) (i 0))
            (loop :for a :in sequences :do
               (let ((s (force-simple-byte-vector a)))
                 (replace ret s :start1 i)
                 (incf i (length s))))
            ret)))
       (t
        (force-byte-vector (first sequences)))))))

(defmacro match-replace-helper (string &body match-replacements)
  (with-unique-names (before replacement-text remaining)
    `(let (,replacement-text)
       (if-match-bind (,before (or ,@(loop
                                        :for (match replacement) :in match-replacements
                                        :collect `(progn
                                                    ,match
                                                    '(setf ,replacement-text (force-to-target-sequence ,replacement))))
                                   (last))
                               ,remaining)
                      ,string
                      (values ,before ,replacement-text ,remaining)
                      nil))))

(defmacro match-replace-one (string &body match-replacements)
  "As match-replace-all but at most one replacement is made"
  (once-only (string)
    `(concatenate-sequences ,string (multiple-value-list (match-replace-helper ,string ,@match-replacements)))))

(defmacro match-replace-all (string &body match-replacements)
  "For each (match replacment) in MATCH-REPLACEMENTS replace each value of match with the value of replacement in STRING"
  (with-unique-names (result)
    (once-only (string)
      `(concatenate-sequences
        ,string
        (let ((,result nil))
          (loop
             (multiple-value-bind (before replacement remaining)
                 (match-replace-helper ,string ,@match-replacements)
               (unless (zerop (length before))
                 (push before ,result))
               (unless (zerop (length replacement))
                 (push replacement ,result))
               (when (zerop (length remaining))
                 (return))
               (setf ,string remaining)))
          ;; OPTIMIZATION: this nreverse could be avoided, but it should be negligable compared to the rest
          (nreverse ,result))))))
