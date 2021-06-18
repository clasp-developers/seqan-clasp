(format t "About to load seqan-clasp asd file~%")
(require :asdf)
(let ((asd-file (probe-file "source-dir:extensions;seqan-clasp;src;seqan.asd")))
  (if asd-file
      (asdf:load-asd asd-file)
      (error "Could not find source-dir:extensions;seqan-clasp;src;seqan.asd")))
;;; 
(push (lambda ()
        (asdf:load-system :seqan)
        (format t "Done eval seqan-clasp startup~%"))
      core:*extension-startup-evals*)
