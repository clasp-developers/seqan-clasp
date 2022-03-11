(k:sources :extension-load #~"seqan-startup.lisp")

(k:sources :iclasp #~"seqan.cc")

(k:sources :install-code
           #~"seqan.asd"
           #~"packages.lisp"
           #~"seqan.lisp")
