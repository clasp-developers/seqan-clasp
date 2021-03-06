

(defpackage :seqan
  (:nicknames :sa)
  (:shadow find length make-string close position)
  (:use :common-lisp :seqan%)
  (:export
   #:get-absolute-path
   #:make-seq-file-in
   #:make-seq-file-out
   #:write-record
   #:close
   #:make-simple-score
   #:make-align
   #:make-align-config
   #:rows&
   #:row&
   #:append-value
   #:at-end
   #:position
   #:to-string
   #:to-source-position
   #:to-view-position
   #:read-record
   #:make-string
   #:make-string-set
   #:make-finder
   #:make-pattern
   #:find
   #:get-score
   #:count-quality-value-less-than
   #:calculate-quality
   #:find-begin
   #:begin-position
   #:end-position
   #:infix
   #:get-prefix
   #:get-infix
   #:get-suffix
   #:length
   #:resize
   #:assign-source
   #:insert-gap
   #:insert-gaps
   #:set-begin-position
   #:global-alignment
   #:local-alignment
   #:assign
   ))
