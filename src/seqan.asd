(in-package :asdf-user)

(defsystem "seqan"
  :description "Expose SeqAn2 sequence analysis library to clasp"
  :version "0.0.1"
  :author "Christian Schafmeister <chris.schaf@verizon.net>"
  :licence "LGPL-3.0"
  :depends-on ( )
  :serial t
  :components
  ((:file "packages")
   (:file "seqan")
   ))
