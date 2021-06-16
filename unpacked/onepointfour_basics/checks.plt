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
 * plunit tests for checks.pl, which implements a replacement for
 * the must_be/2 predicate.
 */

:- use_module(library('onepointfour_basics/checks.pl')).


:- begin_tests(check_that_using_no_conditions).

test("no conditions always succeed #1") :-
   check_that(_,[]).

test("no conditions always succeed #2") :-
   check_that(foo,[]).

:- end_tests(check_that_using_no_conditions).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_var).

test("var, success") :-
   check_that(_,[soft(var)]).

test("var, failure", fail) :-
   check_that(foo,[soft(var)]).

test("var, failure, throw", error(check(uninstantiation,_,_,_))) :-
   check_that(foo,[hard(var)]).

:- end_tests(check_that_using_var).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonvar).

test("nonvar, success") :-
   check_that(foo,[soft(nonvar)]).

test("nonvar, failure", fail) :-
   check_that(_,[soft(nonvar)]).

test("nonvar, failure, throw", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(nonvar)]).

:- end_tests(check_that_using_nonvar).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonground).

test("nonground, success") :-
   forall(
      member(X,[_,f(_)]),
      check_that(X,[soft(nonground)])
   ).

test("nonground, failure", fail) :-
   check_that(foo,[soft(nonground)]).

test("nonground, failure, throw", error(check(domain,_,_,_))) :-
   check_that(foo,[hard(nonground)]).

:- end_tests(check_that_using_nonground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_ground).

test("ground, success") :-
   check_that(foo,[soft(ground)]).

test("ground, failure") :-
   forall(
      member(X,[_,f(_)]),
      \+ check_that(X,[soft(ground)])
   ).

test("ground, failure on var, throw", error(check(domain,_,_,_))) :-
   check_that(_,[hard(ground)]).

test("ground, failure on nonvar, throw", error(check(domain,_,_,_))) :-
   check_that(f(_),[hard(ground)]).

:- end_tests(check_that_using_ground).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atom).

test("atom, success") :-
   check_that(foo,[soft(atom)]).

test("atom, soft, failure") :-
   forall(
      member(X,[1,f(g),"lol",f(_)]),
      \+ check_that(X,[soft(atom)])
   ).

test("atom, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[hard(atom)]).

test("atom, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(atom)]).

test("atom, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(atom)]).

:- end_tests(check_that_using_atom).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_atomic).

test("atomic, success") :-
   forall(
      member(X,[1,foo,"lol"]),
      check_that(X,[soft(atomic)])
   ).

test("atomic, failure",fail) :-
   check_that(f(g(x)),[soft(atomic)]).

test("atomic, hard failure, throw type error", error(check(type,_,_,_))) :-
   check_that(f(g),[hard(atomic)]).

test("atomic, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(atomic)]).

test("atomic, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(atomic)]).

:- end_tests(check_that_using_atomic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_compound).

test("compound, success") :-
   forall(
      member(X,[f(g),f(_)]),
      check_that(X,[soft(compound)])
   ).

test("compound, failure") :-
   forall(
      member(X,[1,foo,"lol"]),
      \+check_that(X,[soft(compound)])
   ).

test("compound, hard failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(compound)]).

test("compound, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(compound)]).

test("compound, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(compound)]).

:- end_tests(check_that_using_compound).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_boolean).

test("boolean, success") :-
   forall(
      member(X,[false,true]),
      check_that(X,[soft(boolean)])
   ).

test("boolean, failure") :-
   forall(
      member(X,[1,0,y,n,yes,no,f(x),f(_),alpha,[],'',"","false","true"]),
      \+check_that(X,[soft(boolean)])
   ).

test("boolean, hard, type exception") :-
   forall(
      member(X,[1,0,f(x),f(_),"false","true"]),
      catch(check_that(X,[hard(boolean)]),error(check(type,_,_,_),_),true)
   ).

test("boolean, hard, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(boolean)]),error(check(domain,_,_,_),_),true)
   ).

test("boolean, hard failure, throw type error", error(check(type,_,_,_))) :-
   check_that(g(g),[hard(boolean)]).

test("boolean, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(boolean)]).

test("boolean, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(boolean)]).

:- end_tests(check_that_using_boolean).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy_typeid).

test("stringy_typeid, success") :-
   forall(
      member(X,[atom,string]),
      check_that(X,[soft(stringy_typeid)])
   ).

test("stringy_typeid, failure") :-
   forall(
      member(X,[foo,"","atom","string",1,0]),
      \+check_that(X,[soft(stringy_typeid)])
   ).

test("stringy_typeid, hard, type exception") :-
   forall(
      member(X,[1,0,f(x),"atom","string"]),
      catch(check_that(X,[hard(stringy_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("stringy_typeid, hard, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(stringy_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("stringy_typeid, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(stringy_typeid)]).

test("stringy_typeid, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(stringy_typeid)]).


:- end_tests(check_that_using_stringy_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary_typeid).

test("chary_typeid, success") :-
   forall(
      member(X,[char,code]),
      check_that(X,[soft(chary_typeid)])
   ).

test("chary_typeid, failure") :-
   forall(
      member(X,[foo,"","char","code",1,0]),
      \+check_that(X,[soft(chary_typeid)])
   ).

test("chary_typeid, hard, type exception") :-
   forall(
      member(X,[1,0,f(x),"char","code"]),
      catch(check_that(X,[hard(chary_typeid)]),error(check(type,_,_,_),_),true)
   ).

test("chary_typeid, hard, domain exception") :-
   forall(
      member(X,[yes,no,'']),
      catch(check_that(X,[hard(chary_typeid)]),error(check(domain,_,_,_),_),true)
   ).

test("chary_typeid, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(chary_typeid)]).

test("chary_typeid, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(chary_typeid)]).

:- end_tests(check_that_using_chary_typeid).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pair).

test("pair, success") :-
   forall(
      member(X,[a-b,1-2,_-_]),
      check_that(X,[soft(pair)])
   ).

test("pair, failure") :-
   forall(
      member(X,[f(x),-(1,2,3),pair,'-']),
      \+check_that(X,[soft(pair)])
   ).

test("pair, hard, type exception") :-
   forall(
      member(X,[1,0,hello]),
      catch(check_that(X,[hard(pair)]),error(check(type,_,_,_),_),true)
   ).

test("pair, hard, domain exception") :-
   forall(
      member(X,[f(x),-(_,_,_),-(1,2,3)]),
      catch(check_that(X,[hard(pair)]),error(check(domain,_,_,_),_),true)
   ).

test("pair, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(pair)]).

test("pair, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(pair)]).

:- end_tests(check_that_using_pair).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_string).

test("string, success") :-
   forall(
      member(X,["","foo"]),
      check_that(X,[soft(string)])
   ).

test("string, failure") :-
   forall(
      member(X,[1,foo,f(x)]),
      \+check_that(X,[soft(string)])
   ).

test("string, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(string)]).

test("string, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(string)]).

test("string, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(string)]).

:- end_tests(check_that_using_string).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stringy).

test("stringy, success") :-
   forall(
      member(X,["","foo",'','foo']),
      check_that(X,[soft(stringy)])
   ).

test("stringy, failure") :-
   forall(
      member(X,[1,f(x)]),
      \+check_that(X,[soft(stringy)])
   ).

test("stringy, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(444,[hard(stringy)]).

test("stringy, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(stringy)]).

test("stringy, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(stringy)]).

:- end_tests(check_that_using_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_stringy).

test("nonempty stringy, success") :-
   forall(
      member(X,["foo",foo]),
      check_that(X,[soft(nonempty_stringy)])
   ).

test("nonempty stringy, failure") :-
   forall(
      member(X,[1,"",'',f(x)]),
      \+check_that(X,[soft(nonempty_stringy)])
   ).

test("nonempty stringy, stringy, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[hard(nonempty_stringy)]).

test("nonempty stringy, stringy, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that("",[hard(nonempty_stringy)]).

test("nonempty stringy, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(nonempty_stringy)]).

test("nonempty stringy, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(nonempty_stringy)]).

:- end_tests(check_that_using_nonempty_stringy).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_char).

test("char, success") :-
   forall(
      member(X,[a,b,c,d]),
      check_that(X,[soft(char)])
   ).

test("char, failure") :-
   forall(
      member(X,[1,f(x),ab,"a","b","",'',[]]),
      \+check_that(X,[soft(char)])
   ).

test("char, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(444,[hard(char)]).

test("char, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(foo,[hard(char)]).

test("char, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(char)]).

test("char, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(char)]).

:- end_tests(check_that_using_char).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_code).

test("code, success") :-
   forall(
      member(X,[0,1,2,3,0x10FFFF]),
      check_that(X,[soft(code)])
   ).

test("code, failure") :-
   forall(
      member(X,[f(x),ab,a,"a",-1,1.0,0xFFFFFFF]),
      \+check_that(X,[soft(code)])
   ).

test("code, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(code)]).

test("code, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(code)]).

test("code, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(code)]).

test("code, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(code)]).

:- end_tests(check_that_using_code).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chary).

test("chary, success") :-
   forall(
      member(X,[0,1,2,3,a,b,c]),
      check_that(X,[soft(chary)])
   ).

test("chary, failure") :-
   forall(
      member(X,[f(x),ab,"a",'',"",-1,[]]),
      \+check_that(X,[soft(chary)])
   ).

test("chary, hard, failure, type exception", error(check(type,_,_,_))) :-
   check_that("foo",[hard(chary)]).

test("chary, hard, failure, domain exception") :-
   forall(
      member(X,[foo,-1]),
      catch(check_that(X,[hard(chary)]),error(check(domain,_,_,_),_),true)
   ).

test("chary, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(chary)]).

test("chary, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(chary)]).

:- end_tests(check_that_using_chary).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_number).

test("number, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0,1,-1,1.0,-1.0,1r12,-1r12,NaN,Inf,-0.0]),
      check_that(X,[soft(number)])
   ).

test("number, failure") :-
   forall(
      member(X,[a,"a",'0']),
      \+check_that(X,[soft(number)])
   ).

test("number, ihard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(number)]).

test("number, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(number)]).
   
test("number, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(number)]).

:- end_tests(check_that_using_number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float).

test("float, success") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[1.0,-1.0,NaN,Inf,-0.0,3.1415]),
      check_that(X,[soft(float)])
   ).

test("float, failure") :-
   forall(
      member(X,[1,1r12,foo]),
      \+check_that(X,[soft(float)])
   ).

test("float, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float)]).

test("float, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(float)]).

test("float, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(float)]).

:- end_tests(check_that_using_float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_integer).

test("integer, success") :-
   forall(
      member(X,[1,0,-1]),
      check_that(X,[soft(integer)])
   ).

test("integer, failure") :-
   forall(
      member(X,[0.0,1r12,-1.0]),
      \+check_that(X,[soft(integer)])
   ).

test("integer, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(integer)]).

test("integer, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(integer)]).

test("integer, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(integer)]).

:- end_tests(check_that_using_integer).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_rational).

test("rational, success") :-
   forall(
      member(X,[1,0,-1,1r12,-1r12]),
      check_that(X,[soft(rational)])
   ).

test("rational, failure") :-
   NaN is nan,
   Inf is -1.0Inf,
   forall(
      member(X,[0.0,3.1415,foo,NaN,Inf]),
      \+check_that(X,[soft(rational)])
   ).

test("rational, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(rational)]).

test("rational, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(rational)]).

test("rational, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(rational)]).

:- end_tests(check_that_using_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonint_rational).

test("nonint_rational, success") :-
   forall(
      member(X,[1r12,-1r12]),
      check_that(X,[soft(nonint_rational)])
   ).

test("nonint_rational, failure") :-
   forall(
      member(X,[0.0,3.1415,foo,1,2,3,4,5]),
      \+check_that(X,[soft(nonint_rational)])
   ).

test("nonint_rational, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(nonint_rational)]).

test("nonint_rational, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(777,[hard(nonint_rational)]).

test("nonint_rational, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(nonint_rational)]).

test("nonint_rational, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(nonint_rational)]).

:- end_tests(check_that_using_nonint_rational).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negnumber).

test("negnumber, success") :-
   MinusInf is -1.0Inf,
   forall(
      member(X,[-1,-1.0,-1r12,MinusInf]),
      check_that(X,[soft(negnumber)])
   ).

test("negnumber, failure") :-
   PlusInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,-0.0,0.0,1,1.0,1r12,NaN,PlusInf]),
      \+check_that(X,[soft(negnumber)])
   ).

test("negnumber, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(negnumber)]).

test("negnumber, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(+1,[hard(negnumber)]).

test("negnumber, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(negnumber)]).

test("negnumber, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(negnumber)]).

:- end_tests(check_that_using_negnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posnumber).

test("posnumber, success") :-
   PlusInf is +1.0Inf,
   forall(
      member(X,[1,1.0,1r12,PlusInf]),
      check_that(X,[soft(posnumber)])
   ).

test("posnumber, failure") :-
   MinusInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,0,+0.0,-1,-1.0,-1r12,NaN,MinusInf]),
      \+check_that(X,[soft(posnumber)])
   ).

test("posnumber, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(posnumber)]).

test("posnumber, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(posnumber)]).

test("posnumber, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(posnumber)]).

test("posnumber, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(posnumber)]).

:- end_tests(check_that_using_posnumber).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_non0number).

test("non0number, success") :-
   forall(
      member(X,[1,1.0,-1.0,-1,1r12]),
      check_that(X,[soft(non0number)])
   ).

test("non0number, failure") :-
   forall(
      member(X,[foo,"foo",0,0.0,-0.0,0r1]),
      \+check_that(X,[soft(non0number)])
   ).

test("non0number, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(non0number)]).

test("non0number, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(0.0,[hard(non0number)]).

test("non0number, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(non0number)]).

test("non0number, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(non0number)]).

:- end_tests(check_that_using_non0number).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_nan).

test("float_not_nan, success") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   forall(
      member(X,[NegInf,-1.0,0.0,-1.0,PosInf]),
      check_that(X,[soft(float_not_nan)])
   ).

test("float_not_nan, failure") :-
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,NaN]),
      \+check_that(X,[soft(float_not_nan)])
   ).

test("float_not_nan, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_nan)]).

test("float_not_nan, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   NaN is nan,
   check_that(NaN,[hard(float_not_nan)]).

test("float_not_nan, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_nan)]).

test("float_not_nan, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_nan)]).

:- end_tests(check_that_using_float_not_nan).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_inf).

test("float_not_inf, success") :-
   NaN is nan,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN]),
      check_that(X,[soft(float_not_inf)])
   ).

test("float_not_inf, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   forall(
      member(X,[foo,"foo",1,NegInf,PosInf]),
      \+check_that(X,[soft(float_not_inf)])
   ).

test("float_not_inf, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_inf)]).

test("float_not_inf, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[hard(float_not_inf)]).

test("float_not_inf, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_inf)]).

test("float_not_inf, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_inf)]).

:- end_tests(check_that_using_float_not_inf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_neginf).

test("float_not_neginf, success") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,PosInf]),
      check_that(X,[soft(float_not_neginf)])
   ).

test("float_not_neginf, failure") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[foo,"foo",1,NegInf]),
      \+check_that(X,[soft(float_not_neginf)])
   ).

test("float_not_neginf, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_neginf)]).

test("float_not_neginf, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   NegInf is -1.0Inf,
   check_that(NegInf,[hard(float_not_neginf)]).

test("float_not_neginf, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_neginf)]).

test("float_not_neginf, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_neginf)]).

:- end_tests(check_that_using_float_not_neginf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_float_not_posinf).

test("float_not_posinf, success") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[-1.0,0.0,-1.0,NaN,NegInf]),
      check_that(X,[soft(float_not_posinf)])
   ).

test("float_not_posinf, failure") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[foo,"foo",1,PosInf]),
      \+check_that(X,[soft(float_not_posinf)])
   ).

test("float_not_posinf, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(float_not_posinf)]).

test("float_not_posinf, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   PosInf is +1.0Inf,
   check_that(PosInf,[hard(float_not_posinf)]).

test("float_not_posinf, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(float_not_posinf)]).

test("float_not_posinf, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(float_not_posinf)]).

:- end_tests(check_that_using_float_not_posinf).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negint).

test("negint, success") :-
   forall(
      member(X,[-2,-1,-6r3]),
      check_that(X,[soft(negint)])
   ).

test("negint, failure") :-
   forall(
      member(X,[foo,"foo",1,0,-1.0,-1r12]),
      \+check_that(X,[soft(negint)])
  ).

test("negint, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(negint)]).

test("negint, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[hard(negint)]).

test("negint, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(negint)]).

test("negint, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(negint)]).

:- end_tests(check_that_using_negint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posint).

test("posint, success") :-
   forall(
      member(X,[2,1,6r3]),
      check_that(X,[soft(posint)])
   ).

test("posint, failure") :-
   forall(
      member(X,[foo,"foo",-1,0,1.0,1r12]),
      \+check_that(X,[soft(posint)])
   ).

test("posint, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(posint)]).

test("posint, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(posint)]).

test("posint, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(posint)]).

test("posint, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(posint)]).

:- end_tests(check_that_using_posint).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0int).

test("neg0int, success") :-
   forall(
      member(X,[-2,-1,-6r3,0]),
      check_that(X,[soft(neg0int)])
   ).

test("neg0int, failure") :-
   forall(
      member(X,[foo,"foo",1,-1.0,-1r12,0.0]),
      \+check_that(X,[soft(neg0int)])
   ).

test("neg0int, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(neg0int)]).

test("neg0int, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(1,[hard(neg0int)]).

test("neg0int, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(neg0int)]).

test("neg0int, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0int)]).

:- end_tests(check_that_using_neg0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0int).

% --- positive-or-zero integer

test("pos0int, success") :-
   forall(
      member(X,[2,1,6r3,0]),
      check_that(X,[soft(pos0int)])
   ).

test("pos0int, failure") :-
   forall(
      member(X,[foo,"foo",-1,1.0,1r12,0.0]),
      \+check_that(X,[soft(pos0int)])
   ).

test("pos0int, hard, failure, throw type error", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0int)]).

test("pos0int, hard, failure, throw domain error", error(check(domain,_,_,_))) :-
   check_that(-1,[hard(pos0int)]).

test("pos0int, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(pos0int)]).
   
test("pos0int, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0int)]).

:- end_tests(check_that_using_pos0int).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_inty).

test("inty, success") :-
   forall(
      member(X,[1, 0, -1, 1.0, 0.0, -0.0, 7777777, 7777777.0]),
      check_that(X,[soft(inty)])
   ).

test("inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      \+check_that(X,[soft(inty)])
   ).

test("inty, hard, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, NegInf, PosInf, NaN, -1.5, +1.5, 0.00000001]),
      catch(check_that(X,[hard(inty)]),error(check(type,_,_,_),_),true)
   ).

test("inty, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(inty)]).

test("inty, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(inty)]).

:- end_tests(check_that_using_inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posinty).

test("posinty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0]),
      check_that(X,[soft(posinty)])
   ).

test("posinty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[soft(posinty)])
   ).

test("posinty, hard, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001]),
      catch(check_that(X,[hard(posinty)]),error(check(type,_,_,_),_),true)
   ).

test("posinty, hard, domain exception") :-
   forall(
      member(X,[-1000, -1000.0]),
      catch(check_that(X,[hard(posinty)]),error(check(domain,_,_,_),_),true)
   ).

test("posinty, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(posinty)]).
   
test("posinty, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(posinty)]).

:- end_tests(check_that_using_posinty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neginty).

test("neginty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0]),
      check_that(X,[soft(neginty)])
   ).

test("neginty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, 0, 0.0, -0.0, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[soft(neginty)])
   ).

test("neginty, hard, type exception") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001 ]),
      catch(check_that(X,[hard(neginty)]),error(check(type,_,_,_),_),true)
   ).

test("neginty, hard, domain exception") :-
   forall(
      member(X,[0, 0.0, -0.0, 1000, 1000.0]),
      catch(check_that(X,[hard(neginty)]),error(check(domain,_,_,_),_),true)
   ).

test("neginty, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(neginty)]).

test("neginty, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(neginty)]).

:- end_tests(check_that_using_neginty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0inty).

test("pos0inty, success") :-
   forall(
      member(X,[1, 1.0, 7777777, 7777777.0, 0.0, 0, -0.0]),
      check_that(X,[soft(pos0inty)])
   ).

test("pos0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, -1000, -1000.0]),
      \+check_that(X,[soft(pos0inty)])
   ).

test("pos0inty, hard, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0inty)]).

test("pos0inty, hard, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[hard(pos0inty)]).

test("pos0inty, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(pos0inty)]).

test("pos0inty, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0inty)]).

:- end_tests(check_that_using_pos0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0inty).

test("neg0inty, success") :-
   forall(
      member(X,[-1, -1.0, -7777777, -7777777.0, 0, 0.0, -0.0]),
      check_that(X,[soft(neg0inty)])
   ).

test("neg0inty, failure") :-
   NegInf is -1.0Inf,
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, -1.5, +1.5, NegInf, PosInf, NaN, 0.00000001, 1000, 1000.0]),
      \+check_that(X,[soft(neg0inty)])
   ).

test("neg0inty, hard, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(neg0inty)]).

test("neg0inty, hard, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(1.0,[hard(neg0inty)]).

test("neg0inty, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(neg0inty)]).

test("neg0inty, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0inty)]).

:- end_tests(check_that_using_neg0inty).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_negfloat).

test("negfloat, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0]),
      check_that(X,[soft(negfloat)])
   ).

test("negfloat, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1.0, -1,0, -0.0, 0.0, PosInf, NaN]),
      \+check_that(X,[soft(negfloat)])
   ).

test("negfloat, hard, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[hard(negfloat)]),error(check(type,_,_,_),_),true)
   ).

test("negfloat, hard, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, PosInf, NaN]),
      catch(check_that(X,[hard(negfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("negfloat, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(negfloat)]).

test("negfloat, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(negfloat)]).

:- end_tests(check_that_using_negfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_posfloat).

test("posfloat, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0]),
      check_that(X,[soft(posfloat)])
   ).

test("posfloat, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,0.0,1r12,NegInf,NaN]),
      \+check_that(X,[soft(posfloat)])
   ).

test("posfloat, hard, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0]),
      catch(check_that(X,[hard(posfloat)]),error(check(type,_,_,_),_),true)
   ).

test("posfloat, hard, domain exception") :-
   NaN is nan,
   NegInf is -1.0Inf,
   forall(
      member(X,[0.0, -0.0, -1000.0, NegInf, NaN]),
      catch(check_that(X,[hard(posfloat)]),error(check(domain,_,_,_),_),true)
   ).

test("posfloat, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(posfloat)]).

test("posfloat, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(posfloat)]).

:- end_tests(check_that_using_posfloat).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_neg0float).

test("neg0float, success") :-
   NegInf is -1.0Inf,
   forall(
      member(X,[NegInf, -2.0, -1.0, 0.0]),
      check_that(X,[soft(neg0float)])
   ).

test("neg0float, failure") :-
   PosInf is +1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000, 1000.0, 1.0, PosInf, NaN]),
      \+check_that(X,[soft(neg0float)])
   ).

test("neg0float, hard, type exception") :-
   forall(
      member(X,[foo, "foo", 1r12, 0, 1000]),
      catch(check_that(X,[hard(neg0float)]),error(check(type,_,_,_),_),true)
   ).

test("neg0float, hard, domain exception") :-
   NaN is nan,
   PosInf is +1.0Inf,
   forall(
      member(X,[0.0, -0.0, 1000.0, 1.0, PosInf,NaN]),
      catch(check_that(X,[hard(neg0float)]),error(check(domain,_,_,_),_),true)
   ).

test("neg0float, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(neg0float)]).

test("neg0float, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(neg0float)]).

:- end_tests(check_that_using_neg0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_pos0float).

test("pos0float, success") :-
   PosInf is +1.0Inf,
   forall(
      member(X,[PosInf,2.0,1.0,0.0]),
      check_that(X,[soft(pos0float)])
   ).

test("pos0float, failure") :-
   NegInf is -1.0Inf,
   NaN is nan,
   forall(
      member(X,[foo,"foo",1,-1.0,0,1r12,NegInf,NaN]),
      \+check_that(X,[soft(pos0float)])
   ).

test("pos0float, hard, failure, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(pos0float)]).

test("pos0float, hard, failure, throw domain exception", error(check(domain,_,_,_))) :-
   check_that(-1.0,[hard(pos0float)]).

test("pos0float, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(pos0float)]).

test("pos0float, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(pos0float)]).

:- end_tests(check_that_using_pos0float).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_list).

test("list, success") :-
   forall(
      member(X,[ [], [_], [1,2,3], [_,_,_] ]),
      check_that(X,[soft(list)])
   ).

test("list, failure") :-
   forall(
      member(X,["",[1|2],[1|_]]),
      \+check_that(X,[soft(list)])
   ).

test("list, failure, hard, throw type exception", error(check(type,_,_,_))) :-
   check_that(foo,[hard(list)]).

test("list, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(list)]).

test("list, failure, soft",fail) :-
   check_that(foo,[soft(list)]).

test("list, soft, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[soft(list)]).

test("list, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(list)]).

:- end_tests(check_that_using_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_nonempty_list).

test("nonempty list, success because these are nonempty lists") :-
   forall(
      member(X,[[1,2,3],[_,_,_]]),
      check_that(X,[soft(nonempty_list)])
   ).

test("nonempty list, soft, failure if empty list",fail) :-
   check_that([],[soft(nonempty_list)]).

test("nonempty list, soft, failure if nonlists (not the correct type)") :-
   forall(
      member(X,["",[1|2],'',foo,123,""]),
      \+check_that(X,[soft(nonempty_list)])
   ).

test("nonempty list, hard, throw type exception if it's not a list") :-
   forall(
      member(X,["",[1|2],'',foo,123,""]),
      catch(check_that(X,[hard(nonempty_list)]),error(check(type,_,_,_),_),true)
   ).

test("nonempty list, hard, throw domain exception if it's the empty list") :-
   catch(check_that([],[hard(nonempty_list)]),error(check(domain,_,_,_),_),true).

test("nonempty list, hard, success it's a nonempty list") :-
   check_that([1,2,3],[hard(nonempty_list)]).

test("nonempty_list, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(nonempty_list)]).

test("nonempty_list, soft, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[soft(nonempty_list)]).

test("nonempty_list, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(nonempty_list)]).

:- end_tests(check_that_using_nonempty_list).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_member).

test("member, success #1") :-
   check_that(a,[soft(member([a,b,c]))]).

test("member, success #2") :-
   check_that(a,[soft(member([a,X,c]))]),
   assertion(var(X)).

test("member, success #3") :-
   check_that(v,[soft(member([a,_,c]))]).

test("member, failure #1",fail) :-
   check_that(v,[soft(member([a,b,c]))]).

test("member, failure #2",fail) :-
   check_that(v,[soft(member([]))]).

test("member, X nonground",error(check(instantiation,_,_,_))) :-
   check_that(_,[soft(member([a,b,c]))]).

test("member, argument to member/1 term not a proper list #1",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[soft(member(_))]). % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #2",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[soft(member(foo))]).  % already caught in well-formedness check

test("member, argument to member/1 term not a proper list #3",error(check(unknown_or_problematic_check,_,_,_))) :-
   check_that(a,[soft(member([a,b,c|_]))]).  % already caught in well-formedness check

:- end_tests(check_that_using_member).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_dict_has_key).

test("throw if dict is uninstantiated",error(check(instantiation,_,_,_))) :-
   check_that(_,[soft(dict_has_key(foo))]).

test("throw if dict is not actually a dict",error(check(type,_,_,_))) :-
   check_that(bar,[soft(dict_has_key(foo))]).

test("throw if key is uninstantiated",error(check(instantiation,_,_,_))) :-
   check_that(_{},[soft(dict_has_key(_))]).

test("throw if key is not atomic",error(check(type,_,_,_))) :-
   check_that(_{},[soft(dict_has_key(g(x)))]).

test("throw if key is not in dict (hard mode)",error(check(domain,_,_,_))) :-
   check_that(_{},[hard(dict_has_key(foo))]).

test("fail if key is not in dict (soft mode)",fail) :-
   check_that(_{},[soft(dict_has_key(foo))]).

test("succeed if key is in dict") :-
   check_that(_{key:value},[soft(dict_has_key(key))]).

:- end_tests(check_that_using_dict_has_key).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_random).

test("random until failure",error(check(random,_,_,_))) :-
   repeat,
   check_that(a,[hard(random(0.5))]),
   fail.

:- end_tests(check_that_using_random).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_dict).

test("dict, success") :-
   forall(
      member(X,[_{},foo{},bar{a:1,b:2}]),
      check_that(X,[soft(dict)])
   ).

test("dict, failure") :-
   forall(
      member(X,["",foo,foo(a,b)]),
      \+check_that(X,[soft(dict)])
   ).

test("dict, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(dict)]).

test("dict, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(dict)]).

:- end_tests(check_that_using_dict).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_cyclic).

test("cyclic, throw if uninstantiated (hard)",error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(cyclic)]).

test("cyclic, throw if nonground acyclic (hard)",error(check(instantiation,_,_,_))) :-
   check_that(s(_),[hard(cyclic)]).


test("cyclic, throw if uninstantiated (soft)",error(check(instantiation,_,_,_))) :-
   check_that(_,[soft(cyclic)]).

test("cyclic, throw if nonground acyclic (soft)",error(check(instantiation,_,_,_))) :-
   check_that(s(_),[soft(cyclic)]).


test("cyclic, fail if uninstantiated (smooth)",fail) :-
   check_that(_,[smooth(cyclic)]).

test("cyclic, fail if nonground acyclic (smooth)",fail) :-
   check_that(s(_),[smooth(cyclic)]).


test("cyclic, throw if ground acyclic (hard)",error(check(domain,_,_,_))) :-
   check_that(s(a),[hard(cyclic)]).

test("cyclic, fail if ground acyclic (soft)",fail) :-
   check_that(s(a),[soft(cyclic)]).


test("cyclic, succeed if nonground cyclic") :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(cyclic)]).

test("cyclic, succeed if ground cyclic") :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(cyclic)]).

:- end_tests(check_that_using_cyclic).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_cyclic_now).

test("cyclic_now, throw if uninstantiated (hard)",error(check(domain,_,_,_))) :-
   check_that(_,[hard(cyclic_now)]).

test("cyclic_now, throw if nonground acyclic (hard)",error(check(domain,_,_,_))) :-
   check_that(s(_),[hard(cyclic_now)]).

test("cyclic_now, throw if ground acyclic (hard)",error(check(domain,_,_,_))) :-
   check_that(s(a),[hard(cyclic_now)]).

test("cyclic_now, fail if uninstantiated (soft)",fail) :-
   check_that(_,[soft(cyclic_now)]).

test("cyclic_now, fail if uninstantiated (soft)",fail) :- 
   check_that(s(_),[soft(cyclic_now)]).

test("cyclic_now, fail if uninstantiated (soft)",fail) :-
   check_that(s(a),[soft(cyclic_now)]).

test("cyclic_now, succeed if nonground cyclic") :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(cyclic_now)]).

test("cyclic_now, succeed if ground cyclic") :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(cyclic_now)]).

:- end_tests(check_that_using_cyclic_now).


% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_now).

test("acyclic_now, succeed if uninstantiated") :-
   check_that(_,[hard(acyclic_now)]).

test("acyclic_now, succeed if nonground acyclic") :-
   check_that(s(_),[hard(acyclic_now)]).

test("acyclic_now, succeed if ground acyclic") :-
   check_that(s(a),[hard(acyclic_now)]).

test("acyclic_now, throw if nonground cyclic (hard)",error(check(domain,_,_,_))) :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(acyclic_now)]).

test("acyclic_now, thriw if ground cyclic (hard)",error(check(domain,_,_,_))) :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(acyclic_now)]).

test("acyclic_now, fail if nonground cyclic (soft)",fail) :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[soft(acyclic_now)]).

test("acyclic_now, fail if ground cyclic (soft)",fail) :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[soft(acyclic_now)]).

:- end_tests(check_that_using_acyclic_now).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_acyclic_forever).

test("acyclic_forever, thwo if uninstantiated (hard)",error(check(domain,_,_,_))) :-
   check_that(_,[hard(acyclic_forever)]).

test("acyclic_forever, throw if nonground acyclic (hard)",error(check(domain,_,_,_))) :-
   check_that(s(_),[hard(acyclic_forever)]).

test("acyclic_forever, fail if uninstantiated (soft)",fail) :-
   check_that(_,[soft(acyclic_forever)]).

test("acyclic_forever, fail if nonground acyclic (soft)",fail) :-
   check_that(s(_),[soft(acyclic_forever)]).

test("acyclic_forever, success if ground acyclic") :-
   check_that(s(a),[hard(acyclic_forever)]).

test("acyclic_forever, fail if nonground cyclic (soft)",fail) :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[soft(acyclic_forever)]).

test("acyclic_forever, fail if ground cyclic (soft)",fail) :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[soft(acyclic_forever)]).

test("acyclic_forever, throw if nonground cyclic (hard)",error(check(domain,_,_,_))) :-
   X=s(X,_),
   assertion(\+ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(acyclic_forever)]).

test("acyclic_forever, throw if ground cyclic (hard)",error(check(domain,_,_,_))) :-
   X=s(X),
   assertion(ground(X)),
   assertion(cyclic_term(X)),
   check_that(X,[hard(acyclic_forever)]).

:- end_tests(check_that_using_acyclic_forever).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_stream).

test("stream atoms (aliases), success") :-
   forall(
      member(X,[user_input, user_output, user_error, current_input, current_output]),
      check_that(X,[soft(stream)])
   ).

test("stream atoms (aliases), failure") :-
   forall(
      member(X,[foo,bar,'']),
      catch(check_that(X,[hard(stream)]),error(check(domain,_,_,_),_),true)
   ).

test("stream, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(stream)]).

test("stream, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(stream)]).

:- end_tests(check_that_using_stream).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_chars).

test("chars, success") :-
   forall(
      member(X,[[],[a,b,c]]),
      check_that(X,[soft(chars)])
   ).

test("chars, failure (includes things that are not a list)") :-
   forall(
      member(X,[[1,2,3], ["a","b"], [ab,cd], [1|2], foo ]),
      \+check_that(X,[soft(chars)])
   ).

test("chars, hard, type exception (not a list, not a list of atoms)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [1, 2]]),
      catch(check_that(X,[hard(chars)]),error(check(type,_,_,_),_),true)
   ).

test("chars, hard, domain exception (list of atoms, but one atom is not of length 1)") :-
   catch(check_that([a,bb,c],[hard(chars)]),error(check(domain,_,_,_),_),true).

test("chars, fully smooth, fails because of uninstantiated member",fail) :-
   check_that([a,b,c,_,d,e,f],[smooth(chars)]).

test("chars, hard, uninstantiated exception because of uninstantiated member",error(check(instantiation,_,_,_))) :-
   check_that([a,b,c,_,d,e,f],[hard(chars)]).

test("chars, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(chars)]).

test("chars, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(chars)]).

:- end_tests(check_that_using_chars).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_codes).

test("codes, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[soft(codes)])
   ).

test("codes, failure (includes things that are not a list)") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",[a,b,c],["a","b"],[ab,cd],[-1,0],[0xFFFFFFFF,0]]),
      \+check_that(X,[soft(codes)])
   ).

test("codes, hard, type exception (not a list, not a list of codes)") :-
   forall(
      member(X,[foo, "foo", [1|2], ["a","b"], [a, b]]),
      catch(check_that(X,[hard(codes)]),error(check(type,_,_,_),_),true)
   ).

test("codes, hard, domain exception (list of codes, but one of the codes is not in range)") :-
   catch(check_that([1,-1,2],[hard(codes)]),error(check(domain,_,_,_),_),true).

test("codes, fully smooth, fails because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[smooth(codes)]).

test("codes, hard, uninstantiated exception because of uninstantiated member",error(check(instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[hard(codes)]).

test("codes, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(codes)]).

test("codes, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(codes)]).

:- end_tests(check_that_using_codes).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_charys).

test("charys, success") :-
   forall(
      member(X,[[],[1,2,3]]),
      check_that(X,[soft(charys)])
   ).

test("charys, failure") :-
   forall(
      member(X,["",[1|2],'',foo,123,"",["a","b"],[ab,cd],[-1,0],[1,2,a],[0xFFFFFFFF,0]]),
      \+check_that(X,[soft(charys)])
   ).

test("charys, hard, type exception") :-
   forall(
      member(X,[foo, "foo", [1|2]]),
      catch(check_that(X,[hard(charys)]),error(check(type,_,_,_),_),true)
   ).

test("charys, smooth, fail instead of throwing in case of char/code mix",fail) :-
   check_that([1,2,a,3],[soft(charys)]).

test("charys, hard, forany exception in case of char/code mix",error(check(forany,_,_,_),_)) :-
   check_that([1,2,a,3],[hard(charys)]).

test("charys, hard, forany exception (list of codes, but one code is not in range)") :-
   catch(check_that([1,-1,2],[hard(charys)]),error(check(forany,_,_,_),_),true).

test("charys, hard, forany exception (list of 1-character strings, which is not chary)") :-
   catch(check_that(["a","b"],[hard(charys)]),error(check(forany,_,_,_),_),true).

test("charys, smooth, fails instead of throwing because of uninstantiated member",fail) :-
   check_that([1,2,3,_,4,5,6],[smooth(charys)]).

test("charys, hard, uninstantiated exception because of uninstantiated member",error(check(instantiation,_,_,_))) :-
   check_that([1,2,3,_,4,5,6],[hard(charys)]).

test("charys, hard, throw instantiation error on uninstantiated X", error(check(instantiation,_,_,_))) :-
   check_that(_,[hard(charys)]).

test("charys, smooth, fail instead of throwing on uninstantiated X", fail) :-
   check_that(_,[smooth(charys)]).

:- end_tests(check_that_using_charys).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_forX).

% --- forall: the single element X must pass all of the checks in the list that is "Check"

test("forall, success") :-
   check_that(12.0,[soft(forall([float,inty,posnum]))]).

test("forall, empty list of checks, success") :-
   check_that(12.0,[]).

test("forall, failure",fail) :-
   check_that(-12.0,[soft(forall([float,inty,posnum]))]).

test("forall, hard, failure",error(check(domain,_,_,_))) :-
   check_that(-12.0,[hard(forall([float,inty,posnum]))]). % fails on posnum

% --- forany: the single element X must pass at least one of the checks in the list that is "Check"

test("forany, success") :-
   check_that(-12,[soft(forany([float,inty,posnum]))]).

test("forany, empty list of checks, failure",fail) :-
   check_that(-12,[soft(forany([]))]).

test("forany, hard, failure",error(check(forany,_,_,_))) :-
   check_that(-12,[hard(forany([float,proper_rational,posnum]))]).

% --- fornone: the single element X must not pass any of the checks in the list that is "Check"

test("fornone, success") :-
   check_that(foo,[soft(fornone([float,inty,posnum]))]).

test("fornone, empty list of checks, failure", fail) :-
   check_that(foo,[soft(fornone([]))]).

test("fornone, failure",fail) :-
   check_that(12.0,[soft(fornone([integer,negnum,inty]))]).

test("fornone, hard, failure",error(check(fornone,_,_,_))) :-
   check_that(12.0,[hard(fornone([integer,negnum,inty]))]).

:- end_tests(check_that_using_forX).

% -------------------------------------------------------------------

:- begin_tests(check_that_using_passX).

% --- passall: all of the elements in the list that is X must pass the single "check"

test("passall, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[soft(passall(integer))]).

test("passall, empty list as X, success") :-
   check_that([],[soft(passall(integer))]).

test("passall, success (all elements are inty)") :-
   check_that([1, 0, 3.0],[soft(passall(inty))]).

test("passall, failure (some elements are not inty)",fail) :-
   check_that([1, 0, 3.1],[soft(passall(inty))]).

test("passall, hard, failure, throws (type error on encountering 3.1)", error(check(type,_,_,_),_)) :-
   check_that([1, 0, 3.1],[hard(passall(pos0inty))]).

test("passall, hard, failure, throws (domain error on encountering -1)", error(check(domain,_,_,_),_)) :-
   check_that([1, -1, 3.1],[hard(passall(pos0inty))]).

% --- passany: at least one of the elements in the list that is X must pass the single "check"

test("passany, not a list as X", error(check(type,_,_,_),_)) :-
   check_that(foo,[soft(passany(integer))]).

test("passany, empty list as X, failure", fail) :-
   check_that([],[soft(passany(integer))]).

test("passany, success (at least one element is inty)") :-
   check_that([foo, g(x), 3.0],[soft(passany(inty))]).

test("passany, failure (none of the elements is inty)",fail) :-
   check_that([foo, g(x), 3.1],[soft(passany(inty))]).

test("passany, hard, failure (none of the elements is posinty, they are all out-of-type)", error(check(passany,_,_,_),_)) :-
   check_that([foo, g(x), 3.1],[hard(passany(inty))]).

test("passany, hard, failure (one of the elements is nonground)") :-
   check_that([1, g(_), 1],[hard(passany(inty))]).

test("passany, hard, failure (none of the elements is posinty, they are all out-of-domain)", error(check(passany,_,_,_),_)) :-
   check_that([-1, -2, 0],[hard(passany(posinty))]).

test("passany, hard, failure (none of the elements is posinty, and some are uninstantiated)", error(check(instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[hard(passany(posinty))]).

test("passany, hard, failure (none of the elements is posinty, and some are uninstantiated)", error(check(instantiation,_,_,_),_)) :-
   check_that([-1, _, _],[hard(passany(posinty))]).

% --- passnone: none of the elements in the list that is X may pass the single "check"

test("passnone, not a-list-as-X", error(check(type,_,_,_),_)) :-
   check_that(foo,[soft(passnone(integer))]).

test("passnone, empty list as X, failure", fail) :-
   check_that([],[soft(passnone(integer))]).

test("passnone, success: there is no inty") :-
   check_that([foo, g(x), 3.1],[soft(passnone(inty))]).

test("passnone, failure: there is an inty",fail) :-
   check_that([foo, 2, 3.1],[soft(passnone(inty))]).

test("passnone, hard, failure: there is an inty, #1", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[tuned(passnone(inty))],throw).

test("passnone, hard, failure: there is an inty, #2", error(check(passnone,_,_,_),_)) :-
   check_that([foo, 2, 3.1],[hard(passnone(inty))]).

test("passnone, hard, failure: there is a nonground") :-
   check_that([foo, g(_), 3.1],[hard(passnone(inty))]).

test("passnone, hard, failure: there is an uninstantiated", error(check(instantiation,_,_,_),_)) :-
   check_that([foo, _, 3.1],[hard(passnone(inty))]).

:- end_tests(check_that_using_passX).

% -------------------------------------------------------------------

:- begin_tests(check_that_multicondition).

test("12 is nonvar and an integer, hard, succeeds") :-
   check_that(12,[hard(nonvar),hard(int)]).

test("foo is nonvar and an integer, hard, throws",error(check(type,_,_,_))) :-
   check_that(foo,[hard(nonvar),hard(int)]).

test("foo is nonvar and an integer, soft, fails", fail) :-
   check_that(foo,[soft(nonvar),soft(int)]).

test("foo is nonvar and an integer, marked tuned, but is hard by switching on hardness, throws", error(check(type,_,_,_))) :-
   check_that(foo,[tuned(nonvar),tuned(int)],throw).

test("a var is nonvar and an integer, soft, fails", fail) :-
   check_that(_,[soft(nonvar),soft(int)]).

:- end_tests(check_that_multicondition).


