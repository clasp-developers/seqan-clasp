(require :asdf)
(asdf:load-asd (pathname "source-dir:extensions;seqan;src;seqan.asd"))
(asdf:load-system :seqan :force t)

(progn
  (defparameter seq1 (sa:make-string :char-string "CDFGDC"))
  (defparameter seq2 (sa:make-string :char-string "CDEFGAHGC"))
  (defparameter align (sa:make-align :char-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq1)
  (sa:assign-source (sa:row& align 1) seq2)
  )



(progn
  (defparameter seq1 (sa:make-string :dna5q-string "AGCTAGCTC"))
  (defparameter seq2 (sa:make-string :dna5q-string "AGCT"))
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq1)
  (sa:assign-source (sa:row& align 1) seq2)
  (defparameter score (sa::global-alignment align (sa:make-simple-score 3 -3 -2 -2)))
  (format t "Score: ~a~%" score)
  (format t "Align:~%~a~%" (sa:to-string align)))

(progn
  (defparameter seq1 (sa:make-string :char-string "aphilologicaltheorem"))
  (defparameter seq2 (sa:make-string :char-string "bizarreamphibology"))
  (defparameter align (sa:make-align :char-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq1)
  (sa:assign-source (sa:row& align 1) seq2)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 3 -3 -2 -2) (sa:make-align-config :t-nil-nil-t)))
  #+(or)(time (dotimes (i 1000000) (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t))))
  (format t "Score: ~a~%" score)
  (format t "Align:~%~a~%" (sa:to-string align)))

(progn
  (defparameter seq1 (sa:make-string :dna5q-string "GCCGCCACAGTCCT"))
  (defparameter seq2 (sa:make-string :dna5q-string "AGTGCCGCCTGCCC"))
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq1)
  (sa:assign-source (sa:row& align 1) seq2)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)))
  #+(or)(time (dotimes (i 1000000) (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t))))
  (format t "Score: ~a~%" score)
  (format t "~a~%" (sa:to-string align))
  )

(progn
  (defparameter short-file (sa:make-seq-file-in "/home/meister/Downloads/short.fastq"))
  (defparameter title (sa:make-string :char-string))
  (defparameter seq (sa:make-string :dna5q-string))
  )

(progn
  (sa:read-record title seq short-file)
  (defparameter needle (sa:make-string :dna5q-string "GCCGCCCAGTCCTGCTCGCTTCGCTACATGG")) ; forward primer
  #+(or)(defparameter needle (sa:make-string :dna5q-string "GCCGCCCATCCTGCTCGTTCGCTACATGG"))
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq)
  (sa:assign-source (sa:row& align 1) needle)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)))
  (format t "   seq: ~a~%" (sa:to-string seq))
  (format t "needle: ~a~%" (sa:to-string needle))
  (format t "score = ~d~%" score)
  (format t "align = ~%~s~%" (sa:to-string align))
  )

(progn
  (defparameter needle (sa:make-string :dna5q-string "TGGCCCAAGGATCA"))
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq)
  (sa:assign-source (sa:row& align 1) needle)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)
                                           38 39))
  (format t "   seq: ~a~%" (sa:to-string seq))
  (format t "needle: ~a~%" (sa:to-string needle))
  (format t "score = ~d~%" score)
  (format t "align = ~%~s~%" (sa:to-string align))
  )

    
(progn
  (sa:read-record title seq short-file)
  (defparameter needle (sa:make-string :dna5q-string "GTGGCACAACAACTG")) ;; "CAGTTGTTGTGCCAC")) ;; Deluge says this is reverse primer (reversed complement)
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq)
  (sa:assign-source (sa:row& align 1) needle)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)))
  (format t "   seq: ~a~%" (sa:to-string seq))
  (format t "needle: ~a~%" (sa:to-string needle))
  (format t "score = ~d~%" score)
  (format t "align = ~s~%" (sa:to-string align))
  )


(progn
  (sa:read-record title seq short-file)
  (defparameter needle (sa:make-string :dna5q-string "GTTTGCCCGCCAGTTGTTGTGCCACAGATCGGAAGAGCACA")) ;; Raw data constant region
  (defparameter align (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align) 2)
  (sa:assign-source (sa:row& align 0) seq)
  (sa:assign-source (sa:row& align 1) needle)
  (defparameter score (sa:global-alignment align (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)))
  (format t "   seq: ~a~%" (sa:to-string seq))
  (format t "needle: ~a~%" (sa:to-string needle))
  (format t "score = ~d~%" score)
  (format t "align = ~s~%" (sa:to-string align))
  )



(defun digit-needles ()
  (let ((digits-1x0i (list "AAGAGAGG"
                           "AGGGAGCA"
                           "ACAAAGAG"
                           "AAGGAGGT"
                           "AGAAAGCA"
                           "ATAAAGGT"
                           "ATAGAAGG"
                           "ATGGGAGT"
                           "GCAAAGGA"
                           "TTGAGGAT"))
        (digits-2x0i (list "AGTTTCAG"
                           "AACCTCAA"
                           "AATCCCAT"
                           "AACCCTAC"
                           "ATCCTCTC"
                           "ATTCTCCG"
                           "CGCCTTCA"
                           "CGTTCCTG"
                           "CTCTCCAC"
                           "TCCTCTTA")))
    (mapcar (lambda (x) (sa:make-string :dna5q-string x)) (append digits-1x0i digits-2x0i))))


(progn
  (defparameter needle-digit-1x01 (elt (digit-needles) 0))
  (defparameter align-digit (sa:make-align :dna5q-string))
  (sa:resize (sa:rows& align-digit) 2)
  (sa:assign-source (sa:row& align-digit 0) seq)
  (sa:assign-source (sa:row& align-digit 1) needle-digit-1x01)
  (defparameter score (sa:global-alignment align-digit (sa:make-simple-score 0 -1 -1) (sa:make-align-config :t-nil-nil-t)))
  (format t "   seq: ~a~%" (sa:to-string seq))
  (format t "needle: ~a~%" (sa:to-string needle-digit-1x01))
  (format t "score = ~d~%" score)
  (format t "align = ~s~%" (sa:to-string align-digit))
  )
  



(defparameter seqs (sa:make-seq-file-in "/home/meister/Downloads/487-4_S1_L001_R1_001.fastq"))
(defparameter seqs (sa:make-seq-file-in "/home/meister/Downloads/short.fastq"))


(defun build-needles ()
  (let ((needles (sa:make-string-set :dna5q-string))
        (sequences (list
                    "GCCGCCCAGTCCTGCTCGCTTCGCTAC"   ; 0001 fwd primer
                    #+(or)"GCCTGTTTGCCCGCCAGTTGTTGTGCCAC" ; 0901 rev primer
                    )))
    ;; Build a stringset of needles
    (mapc (lambda (seq)
            (sa:append-value needles (sa:make-string :dna5q-string seq)))
          sequences)
    needles))


(defun scan-file (seqs &optional (num 2))
  (let* ((meta (sa:make-string :char-string))
         (dna5q-seq (sa:make-string :dna5q-string))
         (needles (build-needles))
         (score (sa:make-simple-score 0 -2 -1))
         (pattern (sa:make-pattern needles score))
         )
    ;; Load each sequence and use it as a haystack
    (loop for x from 0 below num
          if (sa:at-end seqs)
            do (return-from scan-file x)
          else
            do (progn
                 (sa:read-record meta dna5q-seq seqs)
                 (progn
                   (format t "------ seq #~a~%" x)
                   (format t "Seq ~a~%" (sa:to-string dna5q-seq)))
                 (let* ((finder (sa:make-finder dna5q-seq)))
                   (loop for found = (sa:find finder pattern -2)
                         for the-score = (sa:get-score pattern)
                         while found
                         do (loop for found-begin = (sa:find-begin finder pattern -20 #+(or)the-score)
                                  while found-begin
                                  do (progn
                                       (format t "[~a ~a) ~s~%" (sa:begin-position finder) (sa:end-position finder) (sa:to-string (sa:infix finder)))))
                         ))))))
             

