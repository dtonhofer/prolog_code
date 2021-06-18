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

:- begin_tests(stringy_justify).

space_to_dot(R,O) :- 
   re_replace("\\s"/g,".",R,O). % PCRE-based replacement of SPACE by '.' for easier reading

test("simple left") :-
   justify_left("Hello, World",20,Result,string,_),
   space_to_dot(R,RR),
   assertion(RR == "Hello,.World........").

test("simple right") :-
   justify_right("Hello, World",20,R,string,_),
   space_to_dot(R,RR),
   assertion(RR == "........Hello,.World").

test("simple center") :-
   justify_center("Hello, World",20,R,string,_),
   space_to_dot(R,RR),
   assertion(RR == "....Hello,.World....").


/*
% ---
% Full-width calls are hard to read.
%
% justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck)
%
% ---

test("full left", true(O == "Hello,.World........")) :-
   hello_world_justify(left,R),
   space_to_dot(R,O).

test("full right", true(O == "........Hello,.World")) :-
   hello_world_justify(right,R),
   space_to_dot(R,O).

test("full center", true(O == "....Hello,.World....")) :-
   hello_world_justify(center,R),
   space_to_dot(R,O).


test("full left offset +2", true(O ==   "..Hello,.World......")) :-
   hello_world_justify_with_offset(left,+2,R),
   space_to_dot(R,O).

test("full right offset +2", true(O ==  "......Hello,.World..")) :-
   hello_world_justify_with_offset(right,+2,R),
   space_to_dot(R,O).

test("full center offset +2", true(O == "......Hello,.World..")) :-
   hello_world_justify_with_offset(center,+2,R),
   space_to_dot(R,O).

test("full left offset -2", true(O ==   "llo,.World..........")) :-
   hello_world_justify_with_offset(left,-2,R),
   space_to_dot(R,O).

test("full right offset -2", true(O ==  "..........Hello,.Wor")) :-
   hello_world_justify_with_offset(right,-2,R),
   space_to_dot(R,O).

test("full center offset -2", true(O == "..Hello,.World......")) :-
   hello_world_justify_with_offset(center,-2,R),
   space_to_dot(R,O).

% ---
% Testing fine-tuned position in case the text is centered inside a string
% with even/odd number of characters
% ---

test("even width even text left", true(O == "...quux...")) :-
   hello_world_center_even_width_even_text(R,left),
   space_to_dot(R,O).

test("odd width even text left", true(O == "..quux...")) :-
   hello_world_center_odd_width_even_text(R,left),
   space_to_dot(R,O).

test("even width odd text left", true(O == "...foo....")) :-
   hello_world_center_even_width_odd_text(R,left),
   space_to_dot(R,O).

test("odd width odd text left", true(O == "...foo...")) :-
   hello_world_center_odd_width_odd_text(R,left),
   space_to_dot(R,O).

test("even width even text right", true(O == "...quux...")) :-
   hello_world_center_even_width_even_text(R,right),
   space_to_dot(R,O).

test("odd width even text right", true(O == "...quux..")) :-
   hello_world_center_odd_width_even_text(R,right),
   space_to_dot(R,O).

test("even width odd text right", true(O == "....foo...")) :-
   hello_world_center_even_width_odd_text(R,right),
   space_to_dot(R,O).

test("odd width odd text right", true(O == "...foo...")) :-
   hello_world_center_odd_width_odd_text(R,right),
   space_to_dot(R,O).

% ---
% shims on top of
% justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck)
% ---

hello_world_justify(How,Result) :-
   justify("Hello, World", 20, How, _,_,_,_, Result, string, _).

hello_world_justify_with_offset(How,Offset,Result) :-
   justify("Hello, World", 20, How, _,_,_, Offset, Result, string, _).


hello_world_center_even_width_even_text(Result,Prefer) :-
   justify("quux", 10, center, _,_, Prefer, _, Result, string, _).

hello_world_center_odd_width_even_text(Result,Prefer) :-
   justify("quux", 9, center, _,_, Prefer, _, Result, string, _).

hello_world_center_even_width_odd_text(Result,Prefer) :-
   justify("foo", 10, center, _,_, Prefer, _, Result, string, _).

hello_world_center_odd_width_odd_text(Result,Prefer) :-
   justify("foo", 9, center, _,_, Prefer, _, Result, string, _).
*/

:- end_tests(stringy_justify).
