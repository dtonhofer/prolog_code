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

:- use_module(library('onepointfour_basics/dict_settings.pl')).

:- begin_tests(dict_settings).

test("no default: getting with key that exists") :-
   get_setting(_{x:foo,y:bar},x,Value),
   assertion(Value == foo).

test("no default: getting with key that exists and an instantiated value; succeed if unifiable") :-
   get_setting(_{x:foo,y:bar},x,foo).

test("no default: getting with key that exists and an instantiated value; fail if not unifiable",fail) :-
   get_setting(_{x:foo,y:bar},x,quux).

test("no default: getting with key that does not exist, throws",error(dict_error(missing_entry,z))) :-
   get_setting(_{x:foo,y:bar},z,_).



test("with default: normal getting with key that does not exist, default is obtained") :-
   get_setting(_{x:foo,y:bar},z,Value,"(the default)"),
   assertion(Value == "(the default)").

test("with default: normal getting with key that exists: obtain value") :-
   get_setting(_{x:foo,y:bar},x,Value,"(the default)"),
   assertion(Value == foo).

test("with default: normal getting with key that exists, and an instantiated value that does not unify",fail) :-
   get_setting(_{x:foo,y:bar},x,quux,"(the default)").

test("with default: normal getting with key that exists, and an instantiated value that unifies") :-
   get_setting(_{x:foo,y:bar},x,foo,"(the default)").

test("with default: normal getting with key that does not exist, and an instantiated value that unifies with the default") :-
   get_setting(_{x:foo,y:bar},z,"(the default)","(the default)").

test("with default: normal getting with key that does not exist, and an instantiated value that does not unify with the default",fail) :-
   get_setting(_{x:foo,y:bar},z,quux,"(the default)").

test("with default: normal getting with key that does not exist, more generally") :-
   get_setting(_{x:foo,y:bar},z,K1,K2),
   assertion(K1==K2).



test("tuned hard no default: getting with key that does not exist, throws",error(dict_error(missing_entry,z))) :-
   get_setting_tuned(_{x:foo,y:bar},z,_,hard).

test("tuned soft no default: getting with key that does not exist, fails",fail) :-
   get_setting_tuned(_{x:foo,y:bar},z,_,soft).

test("tuned soft no default: getting with key that exists and an instantiated value; succeed if unifiable") :-
   get_setting_tuned(_{x:foo,y:bar},x,foo,soft).

test("tuned soft no default: getting with key that exists and an instantiated value; fail if not unifiable",fail) :-
   get_setting_tuned(_{x:foo,y:bar},x,quux,soft).



test("get from something that is not a dict",error(type_error(dict,_))) :-
   get_setting(notadict,z,_Value).

test("normal getting with key that is out-of-type for a dict key",error(type_error('dict-key',g(foo)))) :-
   get_setting(_{x:foo,y:bar},g(foo),_Value).

:- end_tests(dict_settings).

