(format t "About to load seqan-clasp lisp~%")
(require :asdf)
(asdf:load-asd (probe-file "source-dir:extensions;seqan-clasp;src;seqan.asd"))
(asdf:load-system :seqan)
(defparameter *loaded-seqan-again* 2345999)
(format t "Defined *loaded-seqan-again* again value: ~a~%" *loaded-seqan-again*)
;;;(load "source-dir:extensions;seqan-clasp;src;packages.lisp")
;;;(load "source-dir:extensions;seqan-clasp;src;seqan.lisp")
;;;(provide :seqan)
;;;(format t "Provided :seqan~%")
(format t "Done load seqan-clasp lisp~%")