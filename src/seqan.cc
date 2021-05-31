
#include <clasp/clasp.h>
#include <clasp/core/translators.h>
#include <iostream>
#include <seqan/align.h>
#include <seqan/seq_io.h>
#include <seqan/find.h>

using namespace seqan;

namespace sa {

int seqan_align(const std::string& input_text, const std::string& input_pattern )
{
  typedef String<char> TSequence;                 // sequence type
  typedef Align<TSequence, ArrayGaps> TAlign;      // align type

  TSequence seq1 = input_text;
  TSequence seq2 = input_pattern;
  
  TAlign align;
  resize(rows(align), 2);
  assignSource(row(align, 0), seq1);
  assignSource(row(align, 1), seq2);
    // Initialization
int score = globalAlignment(align, Score<int, Simple>(0, -1, -1));
    std::cout << "Score: " << score << std::endl;
    std::cout << align << std::endl;

    return 0;
}



};



PACKAGE_USE("COMMON-LISP");
PACKAGE_NICKNAME("SA%");
NAMESPACE_PACKAGE_ASSOCIATION(seqan_,SAPkg,"SEQAN%");

namespace seqan_ {
CL_EXPOSE
void seqan_startup() {

  using namespace clbind;
  package_ sa(SAPkg);
  sa.def("getAbsolutePath"_raw,&getAbsolutePath);
  class_<SeqFileIn>(sa,"SeqFileIn"_raw)
    .def_constructor("make-SeqFileIn"_raw,constructor<const char*>())
    ;
  sa.def("atEnd<SeqFileIn>"_raw,+[](SeqFileIn& sf) {return atEnd(sf); } );

  class_<Segment<CharString,PrefixSegment>>(sa,"Segment<CharString,PrefixSegment>"_raw );
  sa.def("prefix<Segment<CharString,PrefixSegment>>"_raw, +[](CharString& s, int e) { return prefix(s,e); } );
  class_<Segment<CharString,InfixSegment>>(sa,"Segment<CharString,InfixSegment>"_raw );
  sa.def("infix<Segment<CharString,InfixSegment>>"_raw, +[](CharString& s, int b, int e) { return infix(s,b,e); } );
  class_<Segment<CharString,SuffixSegment>>(sa,"Segment<CharString,SuffixSegment>"_raw );
  sa.def("prefix<Segment<CharString,SuffixSegment>>"_raw, +[](CharString& s, int b) { return prefix(s,b); } );

  class_<Segment<DnaString,PrefixSegment>>(sa,"Segment<DnaString,PrefixSegment>"_raw );
  sa.def("prefix<Segment<DnaString,PrefixSegment>>"_raw, +[](DnaString& s, int e) { return prefix(s,e); } );
  class_<Segment<DnaString,InfixSegment>>(sa,"Segment<DnaString,InfixSegment>"_raw );
  sa.def("infix<Segment<DnaString,InfixSegment>>"_raw, +[](DnaString& s, int b, int e) { return infix(s,b,e); } );
  class_<Segment<DnaString,SuffixSegment>>(sa,"Segment<DnaString,SuffixSegment>"_raw );
  sa.def("prefix<Segment<DnaString,SuffixSegment>>"_raw, +[](DnaString& s, int b) { return prefix(s,b); } );

  class_<Segment<Dna5QString,PrefixSegment>>(sa,"Segment<Dna5QString,PrefixSegment>"_raw );
  sa.def("prefix<Segment<Dna5QString,PrefixSegment>>"_raw, +[](Dna5QString& s, int e) { return prefix(s,e); } );
  class_<Segment<Dna5QString,InfixSegment>>(sa,"Segment<Dna5QString,InfixSegment>"_raw );
  sa.def("infix<Segment<Dna5QString,InfixSegment>>"_raw, +[](Dna5QString& s, int b, int e) { return infix(s,b,e); } );
  class_<Segment<Dna5QString,SuffixSegment>>(sa,"Segment<Dna5QString,SuffixSegment>"_raw );
  sa.def("prefix<Segment<Dna5QString,SuffixSegment>>"_raw, +[](Dna5QString& s, int b) { return prefix(s,b); } );
  
  class_<CharString>(sa,"CharString"_raw )
    .def_constructor("make-CharString"_raw,constructor<std::string>()) ;
  sa.def("clear<CharString>"_raw, +[](CharString& ds) {clear(ds);} );
  class_<DnaString>(sa,"DnaString"_raw)
    .def_constructor("make-DnaString"_raw,constructor<std::string>()) ;
  sa.def("clear<DnaString>"_raw, +[](DnaString& ds) {clear(ds);} );
  class_<Dna5QString>(sa,"Dna5QString"_raw)
    .def_constructor("make-Dna5QString"_raw,constructor<std::string>()) ;
  sa.def("clear<Dna5QString>"_raw, +[](Dna5QString& ds) {clear(ds);} );

  
  
  class_<StringSet<CharString>>(sa, "StringSet<CharString>"_raw)
    .def_constructor("make-StringSet<CharString>"_raw, constructor<>())
    ;
  class_<StringSet<DnaString>>(sa, "StringSet<DnaString>"_raw)
    .def_constructor("make-StringSet<DnaString>"_raw, constructor<>())
    ;
  class_<StringSet<Dna5QString>>(sa,"StringSet<Dna5QString>"_raw)
    .def_constructor("make-StringSet<Dna5QString>"_raw, constructor<>())
    ;
  sa.def("appendValue<StringSet<CharString>,CharString>"_raw,+[](StringSet<CharString>& ss, CharString& ds) { appendValue(ss,ds); } );
  sa.def("appendValue<StringSet<DnaString>,DnaString>"_raw,+[](StringSet<DnaString>& ss, DnaString& ds) { appendValue(ss,ds); } );
  sa.def("appendValue<StringSet<Dna5QString>,Dna5QString>"_raw,+[](StringSet<Dna5QString>& ss, Dna5QString& ds) { appendValue(ss,ds); } );

#if 0
  class_<Pattern<CharString,DPSearch<SimpleScore>>>(sa,"Pattern<CharString,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<CharString,DPSearch<SimpleScore>>"_raw,constructor<CharString&,SimpleScore>());
  class_<Pattern<DnaString,DPSearch<SimpleScore>>>(sa,"Pattern<DnaString,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<DnaString,DPSearch<SimpleScore>>"_raw,constructor<DnaString,SimpleScore>());
  class_<Pattern<Dna5QString,DPSearch<SimpleScore>>>(sa,"Pattern<Dna5QString,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<Dna5QString,DPSearch<SimpleScore>>"_raw,constructor<Dna5QString,SimpleScore>());
  class_<Pattern<StringSet<CharString>,DPSearch<SimpleScore>>>(sa,"Pattern<StringSet<CharString>,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<StringSet<CharString>,DPSearch<SimpleScore>>"_raw,constructor<StringSet<CharString>,SimpleScore>());
  class_<Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>>(sa,"Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>"_raw,constructor<StringSet<DnaString>,SimpleScore>());
  class_<Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>>(sa,"Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>"_raw)
    .def_constructor("make-Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>"_raw,constructor<StringSet<Dna5QString>,SimpleScore>());
#else
  // Boilerplate can be reduced using CPP macros - but they can be a bit tricky to define
#define CLASS_PATTERN_DPSEARCH(sa_,first,second) class_<Pattern<first,DPSearch<second>>>(sa_,"Pattern<" #first ",DPSearch<" #second ">>"_raw) \
    .def_constructor("make-Pattern<" #first ",DPSearch<" #second ">>"_raw , constructor<first,second>())
  CLASS_PATTERN_DPSEARCH(sa,CharString,SimpleScore);
  CLASS_PATTERN_DPSEARCH(sa,DnaString,SimpleScore);
  CLASS_PATTERN_DPSEARCH(sa,Dna5QString,SimpleScore);
  CLASS_PATTERN_DPSEARCH(sa,StringSet<CharString>,SimpleScore);
  CLASS_PATTERN_DPSEARCH(sa,StringSet<DnaString>,SimpleScore);
  CLASS_PATTERN_DPSEARCH(sa,StringSet<Dna5QString>,SimpleScore);
#endif
  
  class_<SimpleScore>(sa,"SimpleScore"_raw)
    .def_constructor("make-SimpleScore"_raw,constructor<int,int,int,int>(),
                     "(&optional (match 0) (mismatch -1) (gap -1) (gap-open -1))" )
    ;

  sa.def("getScore<Pattern<CharString,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<CharString,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );
  sa.def("getScore<Pattern<DnaString,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<DnaString,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );
  sa.def("getScore<Pattern<Dna5QString,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<Dna5QString,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );
  sa.def("getScore<Pattern<StringSet<CharString>,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<StringSet<CharString>,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );
  sa.def("getScore<Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );
  sa.def("getScore<Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>"_raw,
         +[](Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>& pattern) {
           return getScore(pattern);
         } );

  class_<Finder<CharString,void>>(sa,"Finder<CharString,void>"_raw)
    .def_constructor("make-Finder<CharString,void>"_raw, constructor<CharString&>() )
    ;
  class_<Finder<DnaString,void>>(sa,"Finder<DnaString,void>"_raw)
    .def_constructor("make-Finder<DnaString,void>"_raw, constructor<DnaString&>() )
    ;
  class_<Finder<Dna5QString,void>>(sa,"Finder<Dna5QString,void>"_raw)
    .def_constructor("make-Finder<Dna5QString,void>"_raw, constructor<Dna5QString&>() )
    ;

  sa.def("find<Finder<CharString,void>,Pattern<CharString,DPSearch<SimpleScore>>"_raw,
         +[](Finder<CharString,void>& Finder, Pattern<CharString,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });
  sa.def("find<Finder<DnaString,void>,Pattern<DnaString,DPSearch<SimpleScore>>"_raw,
         +[](Finder<DnaString,void>& Finder, Pattern<DnaString,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });
  sa.def("find<Finder<Dna5QString,void>,Pattern<Dna5QString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<Dna5QString,void>& Finder, Pattern<Dna5QString,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });
#if 0  
  // clbind doesn't like this binding
  sa.def("find<Finder<CharString,void>,Pattern<StringSet<CharString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<CharString,void>& Finder, Pattern<StringSet<CharString>,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });
#endif
  sa.def("find<Finder<DnaString,void>,Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<DnaString,void>& Finder, Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });
  sa.def("find<Finder<Dna5QString,void>,Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<Dna5QString,void>& Finder, Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>& Pattern, int minScore) -> bool {
           return find(Finder,Pattern,minScore);
         });



  sa.def("findBegin<Finder<CharString,void>,Pattern<CharString,DPSearch<SimpleScore>>"_raw,
         +[](Finder<CharString,void>& Finder, Pattern<CharString,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });
  sa.def("findBegin<Finder<DnaString,void>,Pattern<DnaString,DPSearch<SimpleScore>>"_raw,
         +[](Finder<DnaString,void>& Finder, Pattern<DnaString,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });
  sa.def("findBegin<Finder<Dna5QString,void>,Pattern<Dna5QString,DPSearch<SimpleScore>>"_raw,
         +[](Finder<Dna5QString,void>& Finder, Pattern<Dna5QString,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });
#if 0
  // clbind doesn't like this binding
  sa.def("findBegin<Finder<CharString,void>,Pattern<StringSet<CharString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<CharString,void>& Finder, Pattern<StringSet<CharString>,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });
#endif
  sa.def("findBegin<Finder<DnaString,void>,Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<DnaString,void>& Finder, Pattern<StringSet<DnaString>,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });
  sa.def("findBegin<Finder<Dna5QString,void>,Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>"_raw,
         +[](Finder<Dna5QString,void>& Finder, Pattern<StringSet<Dna5QString>,DPSearch<SimpleScore>>& Pattern, int score) {
           return findBegin(Finder,Pattern,score);
         });

  sa.def("length<Finder<CharString,void>>"_raw,
         +[](Finder<CharString,void>& Finder) { return length(Finder); });
  sa.def("beginPosition<Finder<CharString,void>>"_raw,
         +[](Finder<CharString,void>& Finder) { return beginPosition(Finder); });
  sa.def("endPosition<Finder<CharString,void>>"_raw,
         +[](Finder<CharString,void>& Finder) { return endPosition(Finder); });
  sa.def("infix<Finder<CharString,void>>"_raw,
         +[](Finder<CharString,void>& Finder) {
           return infix(Finder); });

  sa.def("length<Finder<DnaString,void>>"_raw,
         +[](Finder<DnaString,void>& Finder) { return length(Finder); });
  sa.def("beginPosition<Finder<DnaString,void>>"_raw,
         +[](Finder<DnaString,void>& Finder) { return beginPosition(Finder); });
  sa.def("endPosition<Finder<DnaString,void>>"_raw,
         +[](Finder<DnaString,void>& Finder) { return endPosition(Finder); });
  sa.def("infix<Finder<DnaString,void>>"_raw,
         +[](Finder<DnaString,void>& Finder) {
           return infix(Finder); });
  
  sa.def("length<Finder<Dna5QString,void>>"_raw,
         +[](Finder<Dna5QString,void>& Finder) { return length(Finder); });
  sa.def("beginPosition<Finder<Dna5QString,void>>"_raw,
         +[](Finder<Dna5QString,void>& Finder) { return beginPosition(Finder); });
  sa.def("endPosition<Finder<Dna5QString,void>>"_raw,
         +[](Finder<Dna5QString,void>& Finder) { return endPosition(Finder); });
  sa.def("infix<Finder<Dna5QString,void>>"_raw,
         +[](Finder<Dna5QString,void>& Finder) {
           return infix(Finder); });
  
  //
  // Read different types of sequence files
  //
  // Read fasta
  sa.def("readRecord<CharString,DnaString,SeqFileIn>"_raw,+[](CharString& is, DnaString& ds, SeqFileIn& seqs) { readRecord(is,ds,seqs); } );
  // Read fastq
  sa.def("readRecord<CharString,Dna5QString,SeqFileIn>"_raw,+[](CharString& is, Dna5QString& ds, SeqFileIn& seqs) { readRecord(is,ds,seqs); } );

  class_<Align<CharString,ArrayGaps>>(sa,"Align<CharString,ArrayGaps>"_raw)
    .def_constructor("make-Align<CharString,ArrayGaps>>"_raw, constructor<>() );
  class_<Align<DnaString,ArrayGaps>>(sa,"Align<DnaString,ArrayGaps>"_raw)
    .def_constructor("make-Align<DnaString,ArrayGaps>>"_raw, constructor<>() );
  class_<Align<Dna5QString,ArrayGaps>>(sa,"Align<Dna5QString,ArrayGaps>"_raw)
    .def_constructor("make-Align<Dna5QString,ArrayGaps>>"_raw, constructor<>() );
  
  class_<Gaps<CharString>>( sa, "Gaps<CharString>" );
  class_<Gaps<DnaString>>( sa, "Gaps<DnaString>" );
  class_<Gaps<Dna5QString>>( sa, "Gaps<Dna5QString>" );
  class_<String<Gaps<CharString>>>( sa, "String<Gaps<CharString>>" );
  class_<String<Gaps<DnaString>>>( sa, "String<Gaps<DnaString>>" );
  class_<String<Gaps<Dna5QString>>>( sa, "String<Gaps<Dna5QString>>" );
  sa.def("rows<Align<CharString,ArrayGaps>>"_raw, +[](Align<CharString,ArrayGaps>& al) -> String<Gaps<CharString>> { return rows(al); } );
  sa.def("rows<Align<DnaString,ArrayGaps>>"_raw, +[](Align<DnaString,ArrayGaps>& al) -> String<Gaps<DnaString>> { return rows(al); } );
  sa.def("rows<Align<Dna5QString,ArrayGaps>>"_raw, +[](Align<Dna5QString,ArrayGaps>& al) -> String<Gaps<Dna5QString>> { return rows(al); } );
  sa.def("resize<String<Gaps<CharString>>>"_raw, +[](String<Gaps<CharString>>& rows, int num) { resize(rows,num); } );
  sa.def("resize<String<Gaps<DnaString>>>"_raw, +[](String<Gaps<DnaString>>& rows, int num) { resize(rows,num); } );
  sa.def("resize<String<Gaps<Dna5QString>>>"_raw, +[](String<Gaps<Dna5QString>>& rows, int num) { resize(rows,num); } );
  sa.def("assignSource(Gaps<CharString>&,int)"_raw, +[](Gaps<CharString>& gaps, CharString& str) { assignSource( gaps, str ); } );
#if 0
  assignSource(row(align,0), seq1);
  assignSource(row(align,1), seq2);
  std::cout << align << std::endl;
#endif
  sa.def("seqan-align"_raw, &sa::seqan_align );


  // Convenience functions to return a string representation of each SeqAn prefix/infix/suffix type
  sa.def("to-string(Segment<CharString,PrefixSegment>)"_raw,+[](Segment<CharString,PrefixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<CharString,InfixSegment>)"_raw,+[](Segment<CharString,InfixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<CharString,SuffixSegment>)"_raw,+[](Segment<CharString,SuffixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<DnaString,PrefixSegment>)"_raw,+[](Segment<DnaString,PrefixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<DnaString,InfixSegment>)"_raw,+[](Segment<DnaString,InfixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<DnaString,SuffixSegment>)"_raw,+[](Segment<DnaString,SuffixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<Dna5QString,PrefixSegment>)"_raw,+[](Segment<Dna5QString,PrefixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<Dna5QString,InfixSegment>)"_raw,+[](Segment<Dna5QString,InfixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Segment<Dna5QString,SuffixSegment>)"_raw,+[](Segment<Dna5QString,SuffixSegment>& is) { stringstream ss; ss << is; return ss.str(); } );

  // Convenience functions to return a string representation of each SeqAn string type
  sa.def("to-string(CharString)"_raw,+[](CharString& is) { return std::string(toCString(is)); } );
  sa.def("to-string(DnaString)"_raw, +[](DnaString& is)  { stringstream ss; ss << is; return ss.str(); } );
  sa.def("to-string(Dna5QString)"_raw,+[](Dna5QString& is) { stringstream ss; ss << is; return ss.str(); } );

  // to-string for aligns
  sa.def("to-string(Align<CharString,ArrayGaps>)"_raw, +[] (Align<CharString,ArrayGaps>& align) { stringstream ss; ss << align; return ss.str(); } );
  sa.def("to-string(Align<DnaString,ArrayGaps>)"_raw, +[] (Align<DnaString,ArrayGaps>& align) { stringstream ss; ss << align; return ss.str(); } );
  sa.def("to-string(Align<Dna5QString,ArrayGaps>)"_raw, +[] (Align<Dna5QString,ArrayGaps>& align) { stringstream ss; ss << align; return ss.str(); } );
  
  
}
};


