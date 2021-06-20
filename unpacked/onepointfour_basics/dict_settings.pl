:- module(onepointfour_basics_dict_settings,
          [
           get_setting/3  % get_setting(+SettingsDict,+Key,?Value)
          ,get_setting/4  % get_setting(+SettingsDict,+Key,?Value,+Default)
          ,get_setting_tuned/4 % get_setting_tuned(SettingsDict,Key,Value,Tuned)
          ]).

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
 * Simple predicates to extract a value from a dict or provide a default instead.
 */

%! get_setting(+SettingsDict,+Key,?Value,+Default)
% 
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing, unifies Value with Default.

get_setting(SettingsDict,Key,Value,_Default) :-
   get_dict(Key,SettingsDict,Value2),             % get_dict/3 succeeds if entry with "Key" exists
   !,
   Value=Value2.
get_setting(_,_,ValueIsDefault,ValueIsDefault).   % fallback to "Default"

%! get_setting_tuned(+SettingsDict,+Key,?Value,@Tuned)
%
% Same as get_setting/3, but with Tuned = 'soft', the predicate
% just fail instead of throwing. Set Tuned = 'hard' (or anything else)
% to recover get_setting/3 behaviour

get_setting_tuned(SettingsDict,Key,Value,Tuned) :-
   (Tuned==soft)
   ->
   get_setting(SettingsDict,Key,Value2,Default),
   (
      (Value2\==Default)
      ->
      Value=Value2   % entry was found, unify
      ;
      fail           % entry does not exist, fail
   )
   ;
   get_setting(SettingsDict,Key,Value).

%! get_setting(+SettingsDict,+Key,?Value)
%
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing a (non-ISO) error term error(dict_error(missing_entry,Key))
% is thrown. 

get_setting(SettingsDict,Key,Value) :-
   get_dict(Key,SettingsDict,Value2),             % get_dict/3 succeeds if entry with "Key" exists
   !,
   Value=Value2.
get_setting(_SettingsDict,Key,_) :-
   throw(error(dict_error(missing_entry,Key),_)).

% Properly printing the error(dict_error(missing_entry,_,_,_),_) exception term
% by adding rules to the prolog::error_message//1 multifile DCG rule.

:- multifile prolog:error_message//1.  % 1-st argument of error term

prolog:error_message(dict_error(missing_entry,Key)) -->
   [ "expected dict entry, which was missing", nl, "dict key: ~q"-Key, nl ].

