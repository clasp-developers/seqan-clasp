

(defpackage :seqan
  (:nicknames :sa)
  (:shadow find length make-string)
  (:use :common-lisp :seqan%)
  (:export
   #:get-absolute-path
   #:make-seq-file-in
   #:make-simple-score
   #:make-align
   #:append-value
   #:at-end
   #:to-string
   #:read-record
   #:make-string
   #:make-string-set
   #:make-finder
   #:make-pattern
   #:find
   #:get-score
   #:find-begin
   #:begin-position
   #:end-position
   #:infix
   #:length
   ))
