:- module(texty,
          [
          texty/2
          ]).
          
!% determinacy(TypeId,What)

determinacy(var,canbe).
determinacy(emptylist,depends).
determinacy(charlist(_),settled).
determinacy(codelist(_),settled).
determinacy(varlist(_),canbe).
determinacy(charylist(_,_,_),canbe).
determinacy(codeylist(_,_,_),canbe).
determinacy(openvarlist(_),canbe).
determinacy(opencharylist(_,_,_),canbe).
determinacy(opencodeylist(_,_,_),canbe).
determinacy(string(_),settled).
determinacy(atom(_),settled).
determinacy(number,settled).
empty(string(0)).
empty(atom(0)).
empty(emptylist).




%! texty(@X,-TypeId)
% 
% If X can be interpreted as a "texty thing", then succeed and set the ouput values accordingly.
% If X cannot be interpreted as a "texty thing", fail.
% Note that texty(X,X) fails, which is logically correct.
% However, 
% ?- texty([X,Y,X,Z],Y).
% Y = varlist(4).
% which is not logically correct.
% In fact, one should preclude variable sharing between the arguments!
% ?- texty([X,X,X|X],K).
% K = openvarlist(3).

% Below the following is understood:
% var = unbound variable
% proper list = list that is properly terminated [a,b,c]
% open list = list that has a var at its "fin"; it may still become a proper list [a,b,c|_]
%             generally a var is also said to be an "open list" but that case is handled separately here
% code = integer, a unicode code point, integer between 0 and 0x10FFFF (we don't bother to verify that the code falls into one of the agreed codepages, see https://en.wikipedia.org/wiki/Unicode#Code_planes_and_blocks)
% char = atom of length 1, aka. "character"
% string = SWI-Prolog string
%
% var                         var
% empty list                  emptylist
% proper list with N chars    charlist(N) N > 0
% proper list with N codes    codelist(N) N > 0
% proper list with N vars     varlist(N)  N > 0
% proper list with N chars, V vars    charylist(N,V,L) where L = N+V L > 0, V > 0, N>0
% proper list with N codes, V vars    codeylist(N,V,L) where L = N+V L > 0, V > 0, N>0
% open list with N vars     openvarlist(N), N > 0
% open list with N chars, V vars    opencharylist(N,V,L) where L = N+V L > 0, N>0, V >= 0
% open list with N codes, V vars    opencodeylist(N,V,L) where L = N+V L > 0, N>0, V >=0


texty(X,var) :- var(X),!.  
   
texty([],emptylist) :- !.
   
texty([X|Xs],TypeIdCompound) :-
   !,
   listy([X|Xs],0,0,0,VarCount,CharCount,CodeCount,TypeFromListy),
   decision(TypeFromListy,VarCount,CharCount,CodeCount,TypeIdCompound).
   
texty(X,string(Length)) :- string(X),!,string_length(X,Length).
   
texty(X,atom(Length)) :- atom(X),!,atom_length(X,Length).
   
texty(X,number) :- number(X).

%! decision(+TypeFromListy,+VarCount,+CharCount,+CodeCount,-TypeIdCompound)
%
% Decision on output values after a list has been examined. Just match what was
% found in the list and set outputs accordingly. Note that in no cases can the examined
% list have been the empty list because that case is special.

decision(properlist , 0        , 0         , CodeCount , codelist(CodeCount))                      :- !,assertion(CodeCount > 0).
decision(properlist , 0        , CharCount , 0         , charlist(CharCount))                      :- !,assertion(CharCount > 0).
decision(properlist , VarCount , 0         , 0         , varlist(VarCount))                        :- !,assertion(VarCount > 0).
decision(properlist , VarCount , 0         , CodeCount , codeylist(CodeCount,VarCount,Length))     :- !,assertion(CodeCount > 0),assertion(VarCount > 0),Length is CodeCount+VarCount.
decision(properlist , VarCount , CharCount , 0         , charylist(CharCount,VarCount,Length))     :- !,assertion(CharCount > 0),assertion(VarCount > 0),Length is CharCount+VarCount.
decision(openlist   , VarCount , 0         , 0         , openvarlist(VarCount))                    :- !,assertion(VarCount > 0).
decision(openlist   , VarCount , 0         , CodeCount , opencodeylist(CodeCount,VarCount,Length)) :- !,assertion(CodeCount > 0),Length is CodeCount+VarCount.
decision(openlist ,   VarCount , CharCount , 0         , opencharylist(CharCount,VarCount,Length)) :- !,assertion(CharCount > 0),Length is CharCount+VarCount.

%! counterupdate(@X,+VarCountIn,+CharCountIn,+CodeCountIn,-VarCountOut,-CharCountOut,-CodeCountOut)
%
% Examine element X of a list and increment the corresponding counter of chars, codes or vars.
% Fail if X is not a char, code or var, or if it is a char and codes have already been found
% or if it is a code and chars have already been found.

counterupdate(X,VarCountIn,CharCount,CodeCount,VarCountOut,CharCount,CodeCount) :-
   var(X),
   !,
   VarCountOut is VarCountIn+1.
   
counterupdate(X,VarCount,CharCountIn,CodeCount,VarCount,CharCountOut,CodeCount) :-
   char(X),
   !,
   (CodeCount == 0), % Fail at once if both char and code found
   CharCountOut is CharCountIn+1.
   
counterupdate(X,VarCount,CharCount,CodeCountIn,VarCount,CharCount,CodeCountOut) :-
   unicode(X),
   !,
   (CharCount == 0), % Fail at once if both char and code found
   CodeCountOut is CodeCountIn+1.
   
%! unicode(@X) is X a valid unicode code point (not a var)
%
% We do not fully & faithfully translate the valid Unicode codepages into this test...

unicode(X) :- integer(X), X >= 0, X =< 0x10FFFF.

%! char(@X) is X a valid char (not a var)

char(X) :- atom(X),atom_length(X,1).
      
%! listy(@List,+VarCountIn,+CharCountIn,+CodeCountIn,-VarCountOut,-CharCountOut,-CodeCountOut,-Type)
% 
% Going down something that looks like a list, updating counters and setting Type. Fails if this is 
% neither an open list nor a proper list, or if it contains other things than vars, chars or codes.
% Additionally, chars and codes must not be mixed.

% Case: Second element of listbox is a var: this is an open list, but we still need to examine X. Type := openlist.

listy([X|Xs],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,openlist) :-
   var(Xs),
   !,
   counterupdate(X,VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut).
   
% Case: Still looks like a list from here, continue down the list.

listy([X1|[X2|Xs]],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,Type) :-
   !,
   counterupdate(X1,VarCountIn,CharCountIn,CodeCountIn,VarCountMid,CharCountMid,CodeCountMid),
   listy([X2|Xs],VarCountMid,CharCountMid,CodeCountMid,VarCountOut,CharCountOut,CodeCountOut,Type).
      
% Case: End of proper list. Type := properlist

listy([X],VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut,properlist) :-
   !,
   counterupdate(X,VarCountIn,CharCountIn,CodeCountIn,VarCountOut,CharCountOut,CodeCountOut).
   
