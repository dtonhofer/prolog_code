:- module(path_list,
          [
           is_path_list/3     % is_path_list(+PathList,?Type,?Ground)
          ,path_text_list/2   % path_text_list(?PathText,?PathList)
          ]).

:- use_module(library(clpfd)).
          
%! is_path_list(+PathList,?Type,?Ground)
%
% Is this a correctly set up PathList? Fails if not.
%
% Type can be unbound, 'atom' (in which case all the fragments must be
% atoms), 'string' (in which case all the fragments must be strings),
% 'any' (mixed atom/strings allowed). If unbound, it is bound to the
% situation encountered.
%
% Ground can be unbound, 'true' (all elements of PathList must be
% ground), 'false' (some elements of PathList may be unbound). If unbound
% it is bound to the situation encountered.
% ---

is_path_list(PathList,Type,Ground) :-   
   assertion((var(Ground);member(Ground,[true,false]))),
   assertion((var(Type);member(Type,[atom,string,any]))),
   once(phrase(path_list(Types,Grounds),PathList)),  % once/1 to get rid of useless nondeterminism
   ground_check(Grounds,Ground),
   type_check(Types,Type).
   
ground_check(Grounds,Ground) :-
   forall(member(X,Grounds),X==true) 
   -> Ground=true     % includes the case of the path without any fragments
   ;  Ground=false.   % some fragments are unbound (fully or partially)

type_check(Types,Type) :-
   var(Type),
   Types==[],
   !. % leave Type unbound
      
type_check(Types,Type) :-
   var(Type),
   Types\==[],
   !,
   (forall(member(X,Types),X==atom) 
    -> 
    Type=atom
    ;  
    forall(member(X,Types),X==string)
    -> 
    Type=string
    ;
    Type=any).
   
type_check(Types,Type) :-
   nonvar(Type),
   !,
   (Type==atom
    -> forall(member(X,Types),X==atom) % accepts the case of the path without any fragments
    ;  
    Type==string
    -> forall(member(X,Types),X==string) % accepts the case of the path without any fragments
    ;
    Type==any % 'any' just accepts anything
    -> true).

path_list(Types,Grounds) --> path_list_now_slash(Types,Grounds).
path_list(Types,Grounds) --> path_list_now_fragment(Types,Grounds).

path_list_now_slash([],[]) --> [].  
path_list_now_slash(Types,[false|Grounds]) --> [slashes(N)], { var(N),! }, path_list_now_fragment(Types,Grounds).
path_list_now_slash(Types,[true|Grounds])  --> [slashes(N)], { integer(N),N>0,! }, path_list_now_fragment(Types,Grounds).

path_list_now_fragment([],[]) --> [].   
path_list_now_fragment(Types,[false|Grounds])         --> [frag(Frag)], { var(Frag),! }, path_list_now_slash(Types,Grounds).
path_list_now_fragment([atom|Types],[true|Grounds])   --> [frag(Frag)], { atom(Frag),atom_length(Frag,L),L>0,! }, path_list_now_slash(Types,Grounds).
path_list_now_fragment([string|Types],[true|Grounds]) --> [frag(Frag)], { string(Frag),string_length(Frag,L),L>0,! }, path_list_now_slash(Types,Grounds).

%! path_text_list(?PathText,?PathList)
%
% Map PathText (which can be string or atom) to a PathList, which is an
% alternating sequence of terms slashes(Count) and frag(Fragment), where
% Count is the number of sequential slashes (at least 1) and Fragment is
% is path fragment (an atom or a string). 
%
% When going from Text to List, the "stringness" or "atomness" of the
% original text is retained for the fragments. 
%
% When going from List to Tex, the "stringness" or "atomness" of the
% fragments is retained if possible. If there are both strings and 
% atoms or no fragments, the result will be a string.
%
% If both arguments are instantiated "stringness" or "atomness" of the
% original text is retained for the fragments and fragment-per-fragment
% unification is done based on that.

path_text_list(PathText,PathList) :-
   assertion((nonvar(PathText);nonvar(PathList))),
   assertion((var(PathText);string(PathText);atom(PathText))),
   assertion((var(PathList);is_list(PathList))), % we don't check is_path_list(PathList,_,_))
   assertion((nonvar(PathText);ground(PathList))), % if we create text, we need groundedness
   once(path_text_list_2(PathText,PathList)). % Get rid of nondeterminism
   
path_text_list_2(PathText,PathList) :-
   string(PathText),
   !,
   string_codes(PathText,Codes),
   phrase(path(PathList2,string),Codes),
   PathList = PathList2.
      
path_text_list_2(PathText,PathList) :-
   atom(PathText),
   !,
   atom_codes(PathText,Codes),
   phrase(path(PathList2,atom),Codes),
   PathList = PathList2.   
   
path_text_list_2(PathText,PathList) :-
   var(PathText),
   !,   
   is_path_list(PathList,Type,true), % find type; demand groundedness via 2nd arg
   phrase(path(PathList,_),Codes), % build codes
   (var(Type) -> text_codes(string,PathText,Codes) ; text_codes(Type,PathText,Codes)).

% ---
% A path is 
% - empty or
% - slashes or
% - an absolute or 
% - a relative path
% 
% An absolute path is
% - slashes followed by a relative path
%
% A relative path is
% - a slashless fragment or
% - a slashless fragment followed by slashes or
% - a slashless fragment followed by slashes followed by a relative path
% ---

path([],_)               --> [].
path([slashes(Count)],_) --> slashes(Count).
path(Path,Type)          --> absolute_path(Path,Type).
path(Path,Type)          --> relative_path(Path,Type).

slashes(1)     --> "/". 
slashes(Count) --> "/", { Count #> 0, Count #= SubCount+1 }, slashes(SubCount).

absolute_path([slashes(Count)|Path],Type) --> slashes(Count),relative_path(Path,Type).

relative_path([frag(Frag)],Type)                     --> fragment(Frag,Type).
relative_path([frag(Frag),slashes(Count)],Type)      --> fragment(Frag,Type), slashes(Count).
relative_path([frag(Frag),slashes(Count)|Path],Type) --> fragment(Frag,Type), slashes(Count), relative_path(Path,Type).

fragment(Frag,Type) --> { var(Frag),! }, fragment_codes(FragCodes), { text_codes(Type,Frag,FragCodes) }. % build Frag from list
fragment(Frag,_)    --> { nonvar(Frag),!,atom_codes(Frag,FragCodes) }, fragment_codes(FragCodes).        % build list from Frag

fragment_codes([C])    --> [C], { \+ atom_codes('/',[C]) }.
fragment_codes([C|Cs]) --> [C], { \+ atom_codes('/',[C]) }, fragment_codes(Cs).

text_codes(string,Text,Codes) :- string_codes(Text,Codes).
text_codes(atom,Text,Codes)   :- atom_codes(Text,Codes).
