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
 * plunit tests for string_stuff.pl, an assortment of predicates 
 * manipulating strings for the purpose of dict prettyprinting
 */
 
:- use_module(library('onepointfour_basics/dict_pp/string_stuff.pl')).

:- begin_tests(string_stuff).

test("max_line_width over various lines") :-
   max_line_width(["alpha","hello world","two"],MaxLineWidth),
   assertion(string_length("hello world",MaxLineWidth)).

test("max_line_width over no lines, succeeds with 0") :-
   max_line_width([],MaxLineWidth),
   assertion(MaxLineWidth==0).

test("make tag line") :-
   make_tag_line("tag",15,0,0,_{justify_tag: center},TagLineOut),
   assertion(TagLineOut == "      tag      ").

test("make tag line") :-
   make_tag_line("tag",15,0,0,_{justify_tag: left},TagLineOut),
   assertion(TagLineOut == "tag            ").

test("make tag line") :-
   make_tag_line("tag",15,0,0,_{justify_tag: right},TagLineOut),
   assertion(TagLineOut == "            tag").

test("make tag line") :-
   make_tag_line("tag",15,3,3,_{justify_tag_full:false},TagLineOut),
   assertion(TagLineOut == "         tag      "). % NB right 3 chars of padding ar missing
                           %PPP012345678901234

test("make tag line") :-
   make_tag_line("tag",15,3,3,_{justify_tag_full:false,justify_tag: left},TagLineOut),
   assertion(TagLineOut == "   tag            "). % NB right 3 chars of padding are missing
                           %PPP012345678901234

test("make tag line") :-
   make_tag_line("tag",15,3,3,_{justify_tag_full:false,justify_tag: right},TagLineOut),
   assertion(TagLineOut == "               tag"). % NB right 3 chars of padding are missing
                           %PPP012345678901234

test("background line for padding, 3 + 15 + 3") :-
   make_background_line_for_padding(15,3,3,LineOut),
   assertion(LineOut == "                     ").
                        %PPP012345678901234PPP

test("make horizontal border line, with 3 + 15 + 3") :-
   make_bordery_lines(15,3,3,BorderLineOut,BackgroundLineOut),
   assertion(BorderLineOut     == "+---------------------+"),
   assertion(BackgroundLineOut == "|                     |").

test("make horizontal border line, width 0") :-
   make_bordery_lines(0,0,0,BorderLineOut,BackgroundLineOut),
   assertion(BorderLineOut     == "++"),
   assertion(BackgroundLineOut == "||").

test("background line for padding, 0 + 0 + 0") :-
   make_background_line_for_padding(0,0,0,LineOut),
   assertion(LineOut == "").
 
:- end_tests(string_stuff).
