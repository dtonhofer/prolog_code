/*  Zero-Clause BSD (0BSD) follows (https://opensource.org/licenses/0BSD)

    Permission to use, copy, modify, and/or distribute this software for
    any purpose with or without fee is hereby granted.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
    WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
    AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
    TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
    PERFORMANCE OF THIS SOFTWARE.
*/

/*
Homepage for this code:

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_stringy_justify.md
*/

:- use_module(library('onepointfour_basics/stringy_justify.pl')).

:- begin_tests(justify_right).

test("'hello' wandering from right to left in a cut 10-char field") :-
   bagof([Offset,Result],
         (between(-6,11,Offset),
          justify_right(10,"hello",Result,string,_{offset:Offset})),
         Bag),
   assertion(Bag ==
      [[-6 ,"          "],
       [-5 ,"          "],
       [-4 ,"         h"],
       [-3 ,"        he"],
       [-2 ,"       hel"],
       [-1 ,"      hell"],
       [0  ,"     hello"], % the standard right-justification
       [1  ,"    hello "],
       [2  ,"   hello  "],
       [3  ,"  hello   "],
       [4  ," hello    "],
       [5  ,"hello     "],
       [6  ,"ello      "],
       [7  ,"llo       "],
       [8  ,"lo        "],
       [9  ,"o         "],
       [10 ,"          "],
       [11 ,"          "]]).

test("the empty string wandering from right to left") :-
   forall(
      between(-6,11,Offset),
      justify_right(10,"","          ",string,_{offset:Offset})).

test("'hello' wandering from right to left in a non-cut 10-char wide field") :-
   bagof([Offset,Result],
         (between(-6,11,Offset),
          justify_right(10,"hello",Result,string,_{offset:Offset,cut_left:false,cut_right:false})),
         Bag),
   assertion(Bag ==
      [[-6,"           hello"],
       [-5,"          hello"],
       [-4,"         hello"],
       [-3,"        hello"],
       [-2,"       hello"],
       [-1,"      hello"],
       [0,"     hello"],
       [1,"    hello "],
       [2,"   hello  "],
       [3,"  hello   "],
       [4," hello    "],
       [5,"hello     "],
       [6,"hello      "],
       [7,"hello       "],
       [8,"hello        "],
       [9,"hello         "],
       [10,"hello          "],
       [11,"hello           "]]).

test("a zero-width field containing 'hello' being progressively widened") :-
   bagof([FieldWidth,Result],
         (between(0,10,FieldWidth),
          justify_right(FieldWidth,"hello",Result,string)),
         Bag),
   assertion(Bag ==
      [[0,""],
       [1,"o"],
       [2,"lo"],
       [3,"llo"],
       [4,"ello"],
       [5,"hello"],
       [6," hello"],
       [7,"  hello"],
       [8,"   hello"],
       [9,"    hello"],
       [10,"     hello"]]).

:- end_tests(justify_right).

:- begin_tests(justify_left).

test("'hello' wandering from left to right") :-
   bagof([Offset,Result],
         (between(-6,11,Offset),
          justify_left(10,"hello",Result,string,_{offset:Offset})),
         Bag),
   assertion(Bag ==
      [[-6,"          "],
       [-5,"          "],
       [-4,"o         "],
       [-3,"lo        "],
       [-2,"llo       "],
       [-1,"ello      "],
       [0, "hello     "],
       [1, " hello    "],
       [2, "  hello   "],
       [3, "   hello  "],
       [4, "    hello "],
       [5, "     hello"],
       [6, "      hell"],
       [7, "       hel"],
       [8, "        he"],
       [9, "         h"],
       [10,"          "],
       [11,"          "]]).

test("a zero-width field containing 'hello' being progressively widened") :-
   bagof([FieldWidth,Result],
         (between(0,10,FieldWidth),
          justify_left(FieldWidth,"hello",Result,string)),
         Bag),
   assertion(Bag ==
      [[0, ""],
       [1, "h"],
       [2, "he"],
       [3, "hel"],
       [4, "hell"],
       [5, "hello"],
       [6, "hello "],
       [7, "hello  "],
       [8, "hello   "],
       [9, "hello    "],
       [10,"hello     "]]).

:- end_tests(justify_left).

:- begin_tests(justify_center).

text("So_black_holes_are_maximal").

test("center, field of 10 characters, leftly") :-
   bagof(
        [Length,Result],
        Text^PieceText^Before^(
           between(0,15,Length),
           text(Text),
           sub_string(Text,0,Length,Before,PieceText),
           justify_center(10,PieceText,Result,string,_{prefer:leftly})
        ),
        Bag),
   assertion(Bag ==
      [[0, "          "],
       [1, "    S     "],
       [2, "    So    "],
       [3, "   So_    "],
       [4, "   So_b   "],
       [5, "  So_bl   "],
       [6, "  So_bla  "],
       [7, " So_blac  "],
       [8, " So_black "],
       [9, "So_black_ "],
       [10,"So_black_h"],
       [11,"o_black_ho"],
       [12,"o_black_ho"],
       [13,"_black_hol"],
       [14,"_black_hol"],
       [15,"black_hole"]]).

test("center, field of 10 characters, rightly") :-
   bagof(
        [Length,Result],
        Text^PieceText^Before^(
           between(0,15,Length),
           text(Text),
           sub_string(Text,0,Length,Before,PieceText),
           justify_center(10,PieceText,Result,string,_{prefer:rightly})
        ),
        Bag),
   assertion(Bag ==
      [[0, "          "],
       [1, "     S    "],
       [2, "    So    "],
       [3, "    So_   "],
       [4, "   So_b   "],
       [5, "   So_bl  "],
       [6, "  So_bla  "],
       [7, "  So_blac "],
       [8, " So_black "],
       [9, " So_black_"],
       [10,"So_black_h"],
       [11,"So_black_h"],
       [12,"o_black_ho"],
       [13,"o_black_ho"],
       [14,"_black_hol"],
       [15,"_black_hol"]]).

test("center, field of 11 characters, leftly") :-
   bagof(
        [Length,Result],
        Text^PieceText^Before^(
           between(0,15,Length),
           text(Text),
           sub_string(Text,0,Length,Before,PieceText),
           justify_center(11,PieceText,Result,string,_{prefer:leftly})
        ),
        Bag),
   assertion(Bag ==
      [[0, "           "],
       [1, "     S     "],
       [2, "    So     "],
       [3, "    So_    "],
       [4, "   So_b    "],
       [5, "   So_bl   "],
       [6, "  So_bla   "],
       [7, "  So_blac  "],
       [8, " So_black  "],
       [9, " So_black_ "],
       [10,"So_black_h "],
       [11,"So_black_ho"],
       [12,"o_black_hol"],
       [13,"o_black_hol"],
       [14,"_black_hole"],
       [15,"_black_hole"]]).

test("center, field of 11 characters, rightly") :-
   bagof(
        [Length,Result],
        Text^PieceText^Before^(
           between(0,15,Length),
           text(Text),
           sub_string(Text,0,Length,Before,PieceText),
           justify_center(11,PieceText,Result,string,_{prefer:rightly})
        ),
        Bag),
   assertion(Bag ==
      [[0, "           "],
       [1, "     S     "],
       [2, "     So    "],
       [3, "    So_    "],
       [4, "    So_b   "],
       [5, "   So_bl   "],
       [6, "   So_bla  "],
       [7, "  So_blac  "],
       [8, "  So_black "],
       [9, " So_black_ "],
       [10," So_black_h"],
       [11,"So_black_ho"],
       [12,"So_black_ho"],
       [13,"o_black_hol"],
       [14,"o_black_hol"],
       [15,"_black_hole"]]).

% this may not be what one wants

test("center, field of 11 characters, leftly, offset on the left 5") :-
   bagof(
        [Length,Result],
        Text^PieceText^Before^(
           between(0,15,Length),
           text(Text),
           sub_string(Text,0,Length,Before,PieceText),
           justify_center(11,PieceText,Result,string,_{offset_left:5,prefer:leftly})
       ),
       Bag),
   assertion(Bag ==
      [[0, "           "],
       [1, "       S   "],
       [2, "       So  "],
       [3, "      So_  "],
       [4, "      So_b "],
       [5, "     So_bl "],
       [6, "     So_bla"],
       [7, "    So_blac"],
       [8, "    So_blac"],
       [9, "   So_black"],
       [10,"   So_black"],
       [11,"  So_black_"],
       [12,"  So_black_"],
       [13," So_black_h"],
       [14," So_black_h"],
       [15,"So_black_ho"]]).

:- end_tests(justify_center).

