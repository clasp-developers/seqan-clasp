(require :asdf)
(asdf:load-asd (pathname "source-dir:extensions;seqan;src;seqan.asd"))
(asdf:load-system :seqan)



(defparameter seqs (sa:make-seq-file-in "/home/meister/Downloads/487-4_S1_L001_R1_001.fastq"))


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
                 (format t "------ seq #~a~%" x)
                 (format t "Seq ~a~%" (sa:to-string dna5q-seq))
                 (let* ((finder (sa:make-finder dna5q-seq)))
                   (loop for found = (sa:find finder pattern -2)
                         for the-score = (sa:get-score pattern)
                         while found
                         do (loop for found-begin = (sa:find-begin finder pattern the-score)
                                  while found-begin
                                  do (progn
                                       (format t "[~a ~a) ~s~%" (sa:begin-position finder) (sa:end-position finder) (sa:to-string (sa:infix finder)))))
                         ))))))
             

