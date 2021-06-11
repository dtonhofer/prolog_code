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

https://github.com/dtonhofer/prolog_code/blob/main/unpacked/onepointfour_basics/README_space_string.md
*/

/* 
How to run:

In the directory holding this file, start swipl, then:

['space_string_performance.plt'],run_tests.
*/

:- use_module(library('onepointfour_basics/space_string.pl')).
:- use_module(library(yall)).
:- use_module(library(apply)).

:- begin_tests(space_string_performance).

callcount(100000).
max_string_size(500).

goal_special(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   space_string(RL,Spaces).

goal_direct(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   length(Codes,RL),
   maplist(=(0'\s),Codes),
   string_codes(Codes,Spaces).

goal_format(SzMax,Spaces) :- 
   random_between(0,SzMax,RL),
   format(string(Spaces),"~t~*|",[RL]),
   string_length(Spaces,RL).

create_and_drop(Goal) :-
   callcount(CC),
   max_string_size(SzMax),
   call_time(
      forall(
         between(1,CC,_),
         call(Goal,SzMax,_Spaces)),
      Dict
   ), 
   Cputime is floor(Dict.cpu * 100)/100,
   Kinf is floor((Dict.inferences + 500)/1000),
   Walltime is floor(Dict.wall * 100)/100,
   format("~n~s (~d calls) (~d max size) using goal '~s'~n",["drop them immediately",CC,SzMax,Goal]),
   format("CPU time: ~q s, KiloInferences: ~q, Wallclock: ~q s~n~n",[Cputime,Kinf,Walltime]).

create_and_collect(Goal) :-
   callcount(CC),
   max_string_size(SzMax),
   length(CollectionList,CC),
   call_time(
      maplist(
         {Goal,SzMax}/[Spaces]>>call(Goal,SzMax,Spaces),
         CollectionList),
      Dict
   ),
   Cputime is floor(Dict.cpu * 100)/100,
   Kinf is floor((Dict.inferences + 500)/1000),
   Walltime is floor(Dict.wall * 100)/100,
   format("~n~s (~d calls) (~d max size) using goal '~s'~n",["collect in list",CC,SzMax,Goal]),
   format("CPU time: ~q s, KiloInferences: ~q, Wallclock: ~q s~n~n",[Cputime,Kinf,Walltime]).

% ---
% The tests themselves
% ---

test("generate strings of space of random length and drop them immediately after creation") :-
   maplist([Goal]>>create_and_drop(Goal),[goal_special,goal_direct,goal_format]).

test("generate strings of space of random length and store them in a list") :-
   maplist([Goal]>>create_and_collect(Goal),[goal_special,goal_direct,goal_format]).

:- end_tests(space_string_performance).

