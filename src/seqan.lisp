(in-package :seqan)

(defun get-absolute-path (path) (|getAbsolutePath| path))
(defun make-seq-file-in (path) (|make-SeqFileIn| path))
  
(defgeneric make-string (kind &optional value)
  (:method ((kind (eql :char-string)) &optional (value ""))
    (|make-CharString| value))
  (:method ((kind (eql :dna-string)) &optional (value ""))
    (|make-DnaString| value))
  (:method ((kind (eql :dna5q-string)) &optional (value ""))
    (|make-Dna5QString| value)))

(defgeneric make-string-set (kind)
  (:method ((kind (eql :char-string)))
    (|make-StringSet<CharString>|))
  (:method ((kind (eql :dna-string)))
    (|make-StringSet<DnaString>|))
  (:method ((kind (eql :dna5q-string)))
    (|make-StringSet<Dna5QString>|)))

(defun make-simple-score (&optional (match 0) (mismatch -1) (gap -1) (gap-open gap))
  (|make-SimpleScore| match mismatch gap gap-open))
    
(defgeneric append-value (needles sequence)
  (:method ((needles |StringSet<CharString>|) (sequence |CharString|))
    (|appendValue<StringSet<CharString>,CharString>| needles sequence))
  (:method ((needles |StringSet<DnaString>|) (sequence |DnaString|))
    (|appendValue<StringSet<DnaString>,DnaString>| needles sequence))
  (:method ((needles |StringSet<Dna5QString>|) (sequence |Dna5QString|))
    (|appendValue<StringSet<Dna5QString>,Dna5QString>| needles sequence)))


(defgeneric at-end (file)
  (:method ((file |SeqFileIn|))
    (|atEnd<SeqFileIn>| file)))

(defgeneric read-record (metadata sequence file)
  (:method ((metadata |CharString|) (sequence |DnaString|) (file |SeqFileIn|))
    (|readRecord<CharString,DnaString,SeqFileIn>| metadata sequence file))
  (:method ((metadata |CharString|) (sequence |Dna5QString|) (file |SeqFileIn|))
    (|readRecord<CharString,Dna5QString,SeqFileIn>| metadata sequence file)))

(defgeneric to-string (str)
  (:documentation "Convert SeqAn String<xxx> to a Common Lisp string")

  (:method ((str |Segment<CharString,PrefixSegment>|)) (|to-string(Segment<CharString,PrefixSegment>)| str))
  (:method ((str |Segment<CharString,InfixSegment>|)) (|to-string(Segment<CharString,InfixSegment>)| str))
  (:method ((str |Segment<CharString,SuffixSegment>|)) (|to-string(Segment<CharString,SuffixSegment>)| str))
  (:method ((str |Segment<DnaString,PrefixSegment>|)) (|to-string(Segment<DnaString,PrefixSegment>)| str))
  (:method ((str |Segment<DnaString,InfixSegment>|)) (|to-string(Segment<DnaString,InfixSegment>)| str))
  (:method ((str |Segment<DnaString,SuffixSegment>|)) (|to-string(Segment<DnaString,SuffixSegment>)| str))
  (:method ((str |Segment<Dna5QString,PrefixSegment>|)) (|to-string(Segment<Dna5QString,PrefixSegment>)| str))
  (:method ((str |Segment<Dna5QString,InfixSegment>|)) (|to-string(Segment<Dna5QString,InfixSegment>)| str))
  (:method ((str |Segment<Dna5QString,SuffixSegment>|)) (|to-string(Segment<Dna5QString,SuffixSegment>)| str))
  
  (:method ((str |CharString|)) (|to-string(CharString)| str))
  (:method ((str |DnaString|)) (|to-string(DnaString)| str))
  (:method ((str |Dna5QString|)) (|to-string(Dna5QString)| str))

  (:method ((str |Align<CharString,ArrayGaps>|)) (|to-string(Align<CharString,ArrayGaps>)| str))
  (:method ((str |Align<DnaString,ArrayGaps>|)) (|to-string(Align<DnaString,ArrayGaps>)| str))
  (:method ((str |Align<Dna5QString,ArrayGaps>|)) (|to-string(Align<Dna5QString,ArrayGaps>)| str))
  )


(defgeneric make-finder (sequence)
  (:documentation "Create a finder of the appropriate type")
  (:method ((sequence |CharString|))
    (|make-Finder<CharString,void>| sequence))
  (:method ((sequence |DnaString|))
    (|make-Finder<DnaString,void>| sequence))
  (:method ((sequence |Dna5QString|))
    (|make-Finder<Dna5QString,void>| sequence)))



(defgeneric make-pattern (needles score)
  (:documentation "Make a pattern templated on the needles and score")
  (:method ((needles |CharString|) (score |SimpleScore|))
    (|make-Pattern<CharString,DPSearch<SimpleScore>>| needles score))
  (:method ((needles |DnaString|) (score |SimpleScore|))
    (|make-Pattern<DnaString,DPSearch<SimpleScore>>| needles score))
  (:method ((needles |Dna5QString|) (score |SimpleScore|))
    (|make-Pattern<Dna5QString,DPSearch<SimpleScore>>| needles score))
  (:method ((needles |StringSet<CharString>|) (score |SimpleScore|))
    (|make-Pattern<StringSet<CharString>,DPSearch<SimpleScore>>| needles score))
  (:method ((needles |StringSet<DnaString>|) (score |SimpleScore|))
    (|make-Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>| needles score))
  (:method ((needles |StringSet<Dna5QString>|) (score |SimpleScore|))
    (|make-Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>| needles score)))

(defgeneric find (finder pattern &optional minscore)
  (:documentation "Do a find")
  (:method ((finder |Finder<CharString,void>|)
            (pattern |Pattern<CharString,DPSearch<SimpleScore>>|)
            &optional (minscore 0))
    (|find<Finder<CharString,void>,Pattern<CharString,DPSearch<SimpleScore>>|
     finder
     pattern
     minscore))
  (:method ((finder |Finder<DnaString,void>|)
            (pattern |Pattern<DnaString,DPSearch<SimpleScore>>|)
            &optional (minscore 0))
    (|find<Finder<DnaString,void>,Pattern<DnaString,DPSearch<SimpleScore>>|
     finder
     pattern
     minscore))
  (:method ((finder |Finder<Dna5QString,void>|)
            (pattern |Pattern<Dna5QString,DPSearch<SimpleScore>>|)
            &optional (minscore 0))
    (|find<Finder<Dna5QString,void>,Pattern<Dna5QString>,DPSearch<SimpleScore>>|
     finder
     pattern
     minscore))

  #+(or)(:method ((finder |Finder<CharString,void>|)
                  (pattern |Pattern<StringSet<CharString>,DPSearch<SimpleScore>>|)
                  &optional (minscore 0))
          (|find<Finder<CharString,void>,Pattern<StringSet<CharString>,DPSearch<SimpleScore>>|
           finder
           pattern
           minscore))
  (:method ((finder |Finder<DnaString,void>|)
            (pattern |Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>|)
            &optional (minscore 0))
    (|find<Finder<DnaString,void>,Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>|
     finder
     pattern
     minscore))
  (:method ((finder |Finder<Dna5QString,void>|)
            (pattern |Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>|)
            &optional (minscore 0))
    (|find<Finder<Dna5QString,void>,Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>|
     finder
     pattern
     minscore))
  )

(defgeneric get-score (pattern)
  (:method ((pattern |Pattern<CharString,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<CharString,DPSearch<SimpleScore>>| pattern))
  (:method ((pattern |Pattern<DnaString,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<DnaString,DPSearch<SimpleScore>>| pattern))
  (:method ((pattern |Pattern<Dna5QString,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<Dna5QString,DPSearch<SimpleScore>>| pattern))
  (:method ((pattern |Pattern<StringSet<CharString>,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<StringSet<CharString>,DPSearch<SimpleScore>>| pattern))
  (:method ((pattern |Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>| pattern))
  (:method ((pattern |Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>|))
    (|getScore<Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>| pattern))
  )

(defgeneric find-begin (finder pattern score)
  (:method ((finder |Finder<CharString,void>|)
            (pattern |Pattern<CharString,DPSearch<SimpleScore>>|)
            score)
    (|findBegin<Finder<CharString,void>,Pattern<CharString,DPSearch<SimpleScore>>|
     finder
     pattern
     score))
  (:method ((finder |Finder<DnaString,void>|)
            (pattern |Pattern<DnaString,DPSearch<SimpleScore>>|)
            score)
    (|findBegin<Finder<DnaString,void>,Pattern<DnaString,DPSearch<SimpleScore>>|
     finder
     pattern
     score))
  (:method ((finder |Finder<Dna5QString,void>|)
            (pattern |Pattern<Dna5QString,DPSearch<SimpleScore>>|)
            score)
    (|findBegin<Finder<Dna5QString,void>,Pattern<Dna5QString,DPSearch<SimpleScore>>|
     finder
     pattern
     score))
  #+(or)(:method ((finder |Finder<CharString,void>|)
                  (pattern |Pattern<StringSet<CharString>,DPSearch<SimpleScore>>|)
                  score)
          (|findBegin<Finder<CharString,void>,Pattern<StringSet<CharString>,DPSearch<SimpleScore>>|
           finder
           pattern
           score))
  (:method ((finder |Finder<DnaString,void>|)
            (pattern |Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>|)
            score)
    (|findBegin<Finder<DnaString,void>,Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>|
     finder
     pattern
     score))
  (:method ((finder |Finder<Dna5QString,void>|)
            (pattern |Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>|)
            score)
    (|findBegin<Finder<Dna5QString,void>,Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>|
     finder
     pattern
     score))
  )


(defgeneric begin-position (finder)
  (:method  ((finder |Finder<CharString,void>|)) (|beginPosition<Finder<CharString,void>>| finder))
  (:method  ((finder |Finder<DnaString,void>|)) (|beginPosition<Finder<DnaString,void>>| finder))
  (:method  ((finder |Finder<Dna5QString,void>|)) (|beginPosition<Finder<Dna5QString,void>>| finder))
  )

(defgeneric end-position (finder)
  (:method  ((finder |Finder<CharString,void>|)) (|endPosition<Finder<CharString,void>>| finder))
  (:method  ((finder |Finder<DnaString,void>|)) (|endPosition<Finder<DnaString,void>>| finder))
  (:method  ((finder |Finder<Dna5QString,void>|)) (|endPosition<Finder<Dna5QString,void>>| finder))
  )

(defgeneric infix (finder)
  (:method ((finder |Finder<CharString,void>|)) (|infix<Finder<CharString,void>>| finder))
  (:method ((finder |Finder<DnaString,void>|)) (|infix<Finder<DnaString,void>>| finder))
  (:method ((finder |Finder<Dna5QString,void>|)) (|infix<Finder<Dna5QString,void>>| finder))
  )

(defgeneric length (finder)
  (:method length ((finder |Finder<CharString,void>|)) (|length<Finder<CharString,void>>| finder))
  (:method length ((finder |Finder<DnaString,void>|)) (|length<Finder<DnaString,void>>| finder))
  (:method length ((finder |Finder<Dna5QString,void>|)) (|length<Finder<Dna5QString,void>>| finder))
  )

(defgeneric make-align (kind)
  (:method ((kind (eql :char-string))) (|make-Align<CharString,ArrayGaps>|))
  (:method ((kind (eql :dna-string))) (|make-Align<DnaString,ArrayGaps>|))
  (:method ((kind (eql :dna5q-string))) (|make-Align<Dna5QString,ArrayGaps>|))
  )
  
