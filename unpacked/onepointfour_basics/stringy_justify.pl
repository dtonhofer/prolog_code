:- module(onepointfour_basic_justify,
          [
          justify_left/3    % justify_left(Text,Width,Result)
         ,justify_right/3   % justify_right(Text,Width,Result)
         ,justify_center/3  % justify_center(Text,Width,Result)
         ,justify_how/4     % justify_how(How,Text,Width,Result)
         ,justify_left/5    % justify_left(Text,Width,Result,Want,Nocheck)
         ,justify_right/5   % justify_right(Text,Width,Result,Want,Nocheck)
         ,justify_center/5  % justify_center(Text,Width,Result,Want,Nocheck)
         ,justify/10        % justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck)
          ]).

:- use_module(library('onepointfour_basic_justify/space_string.pl')).
:- use_module(library('onepointfour_basic_justify/stringy_overwrite.pl')).

%! justify_left(+Te
% ===
% Simple calls that don't check and always return strings
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% Result   : The Result, a string (not an atom)
% ===

justify_left(Text,Width,Result) :-
   justify(Text,Width,left,_,_,_,_,Result,string,false).

justify_right(Text,Width,Result) :-
   justify(Text,Width,right,_,_,_,_,Result,string,false).

justify_center(Text,Width,Result) :-
   justify(Text,Width,center,_,_,_,_,Result,string,false).

% ===
% Simple calls that don't check and always return strings
%
% How      : One of: left, right, center, none - specifies how to justify (none: just pass the text through)
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% Result   : The Result, a string (not an atom)
% ===

% TODO: Use switch meta-predicate here

justify_how(How,Text,Width,Result) :-
   ((How==left)
    -> justify_left(Text,Width,Result) ;
    (How==right)
    -> justify_right(Text,Width,Result) ;
    (How==center)
    -> justify_center(Text,Width,Result) ;
    (How==none)
    -> stringy_ensure(Text,Result,string) ;
   domain_error([left,right,center,none],How)).

% ===
% Simple calls with reduced parameter count
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% Result   : The Result, an atom or a string depending on 'Want'
%
% Want     : One of 'atom' or 'string' to indicate what the "Result" should be. No default is accepted.
% Nocheck  : Bypass assertions on intro (if they haven't been compiled-out already). If 'true', then bypass. Anything else, including _: do not bypass.
% ===

justify_left(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,left,_,_,_,_,Result,Want,Nocheck).

justify_right(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,right,_,_,_,_,Result,Want,Nocheck).

justify_center(Text,Width,Result,Want,Nocheck) :-
   justify(Text,Width,center,_,_,_,_,Result,Want,Nocheck).

% ===
% Perform text justification, the whole enchilada.
%
% (Prolog abosolutely needs name-based parameter passing. Productions systems traditionally have them!)
%
% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% How      : One of the atoms 'left', 'right', 'center', 'none'. No default is accepted.
% Want     : One of 'atom' or 'string' to indicate what the "Result" should be. No default is accepted.
% CutLeft  : Cut off on the left (lower than position 0?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% CutRight : Cut off on the right (higher than position Width?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% Prefer   : In case of "How" = 'center', should the mass of the text be moved leftwards or rightwards if there is 1 leftover character? One of 'left' or 'right' or leave it at _, in which case 'left' is assumed (and unified).
% Offset   : An offset to apply in any case. For a positive value: When How == 'left', move rightwards, When How == 'right', move leftwards, When How == 'center', move leftwards. Leave at _ for 0.
% Nocheck  : Bypass assertions on intro (if they haven't been compiled-out already). If 'true', then bypass. Anything else, including _: do not bypass.
% Result   : The Result, an atom or a string depending on 'Want'
% ===

%! justify(+Text,+NoCheck) :-
   entry_check(NoCheck),



entry_check(true,_) :- !.

% get_dict_defaultily(+Key,+Dict,+Default,-Found)

get_dict_defaultily(Key,Dict,Default,Found) :-
   get_dict(Key,Dict,Found) 
   ->
   true
   ;
   Default=Found.



% What is passed:
% Argument Text       : stringy
% Argument Width      : integer, positive or 0
% Argument How        : atom, one of left, right, center
% Argument ResultType : atom, one of true, false or may be instantiated to one of true, false
% Argument Throw      : true,throw (switch tuned checks to hard) or something else (switch tuned checks to soft)
% Argument Nocheck    : Bypass checks 
% Argument ConfigDict : Dict (may be missing)
%   Key 'prefer'        : left, right, default left
%   Key 'offset'        : integer, can be negative
%   Key 'pad_right'     : true, false, default true
%   Key 'cut_left'      : true, false, default true
%   Key 'cut_right'     : true, false, default true

% Text     : The text to justify (spaces will be added)
% Width    : The width of the field in which the text shall be justified (an integer >= 0)
% How      : One of the atoms 'left', 'right', 'center', 'none'. No default is accepted.
% Want     : One of 'atom' or 'string' to indicate what the "Result" should be. No default is accepted.
% CutLeft  : Cut off on the left (lower than position 0?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% CutRight : Cut off on the right (higher than position Width?). One of 'true' or 'false' or leave it at _, in which case 'true' is assumed (and unified).
% Prefer   : In case of "How" = 'center', should the mass of the text be moved leftwards or rightwards if there is 1 leftover character? One of 'left' or 'right' or leave it at _, in which case 'left' is assumed (and unified).
% Offset   : An offset to apply in any case. For a positive value: When How == 'left', move rightwards, When How == 'right', move leftwards, When How == 'center', move leftwards. Leave at _ for 0.
% Nocheck  : Bypass assertions on intro (if they haven't been compiled-out already). If 'true', then bypass. Anything else, including _: do not bypass.
% Result   : The Result, an atom or a string depending on 'Want'
%


left

   |-------------------------- width -----------------------------|
   |--offset--|------- text ------|-------------pad---------------|
              |---------------width available---------------------|

WidthAvailable is Width - Offset.
stringy_length(Text,TextLen),


right

   |************************FieldWidth****************************|
   |-------PadWidth-----------|*********TextWidth******|**Offset**|
   |---------------AvailableWidth----------------------|

stringy_length(Text,TextWidth),
PadWidth is FieldWidth-TextWidth-Offset


PadWidth is AvailableWidth-TextWidth,

space_string(PasPadding),

center

   |-------------------------- width -----------------------------|
   |--offset--|------pad------|------- text ------|------pad------|
              |---------------width available---------------------|

WidthAvailable is Width - Offset.
stringy_length(Text,TextLen),




       






 




entry_check(false,Text,Width,How,ConfigDict,Throw) :-
   check_that(Text,[hard(stringy)]),
   check_that(Width,[hard(integer),tuned(pos0int)],Throw),
   check_that(How,[hard(member([left,right,center,none]))]),     


   check_that(ConfigDict,[break(var),hard(boolean)]),
   check_that(ConfigDict,[break(var),hard(boolean)]),

   check_that(Prefer,[break(var),hard(member([left,right]))),



   assertions_intro_for_justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Want) :-
   assertion((var(Prefer);memberchk(Prefer,[left,right]))),
   assertion((var(Offset);integer(Offset))),
   assertion(memberchk(Want,[atom,string])).

  
entry_check(NoCheck, ) :-
   check_that(NoCheck,[hard(boolean)]).




justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Result,Want,Nocheck) :-
   unless((Nocheck == true),assertions_intro_for_justify(Text,Width,How,CutLeft,CutRight,Prefer,Offset,Want)),
   unless((var(Result);stringy(Result)),fail), % shortcut in case of "checking the result"
   if_then(var(CutLeft),CutLeft=true),
   if_then(var(CutRight),CutRight=true),
   if_then(var(Prefer),Prefer=left),
   if_then(var(Offset),Offset=0),
   justify_helper(How,Prefer,Text,Width,Offset,CutLeft,CutRight,Result,Want).

justify_helper(left,_,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   space_string(Width,Spaces,throw), % should use Want
   overwrite_using_runs(Spaces,Text,Offset,CutLeft,CutRight,Result,Want).

justify_helper(right,_,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   space_string(Width,Spaces,throw), % should use Want
   stringy_length(Text,TextLen),
   ActualOffset is Width-TextLen-Offset,
   overwrite_using_runs(Spaces,Text,ActualOffset,CutLeft,CutRight,Result,Want).

justify_helper(center,Prefer,Text,Width,Offset,CutLeft,CutRight,Result,Want) :-
   space_string(Width,Spaces,throw), % should use Want
   stringy_length(Text,TextLen),
   reify(odd(TextLen),IsOddTextLen),
   reify(odd(Width),IsOddWidth),
   actual_offset(IsOddTextLen,IsOddWidth,TextLen,Width,Offset,Prefer,ActualOffset),
   overwrite_using_runs(Spaces,Text,ActualOffset,CutLeft,CutRight,Result,Want).

justify_helper(none,_,Text,_,_,_,_,Result,Want) :-
   stringy_ensure(Text,Result,Want).

% ---
% Computing the actual offset to apply in case of a "centrally justified text"
%
%
%   oooooooooXiiiiiiiii       width is odd (and has a central character)
%         aaaXbbb             text is odd (and has a central character)
%                             ActualOffset := (Width-1)/2 - (TextLen-1)/2 + Offset
%
%   ooooooooooiiiiiiiiii      width is even
%          aaabbb             text is even
%                             ActualOffset := Width/2 - TextLen/2 + Offset
%
%   ooooooooooiiiiiiiiii      width is even
%          aaaXbbb            rightly behaviour : text is odd (and has a central character): Correction =  0 (default)
%         aaaXbbb             leftly  behaviour : text is odd (and has a central character): Correction = -1
%                             ActualOffset := Width/2 - (TextLen-1)/2 + Correction + (user-requested offset)
%
%   oooooooooXiiiiiiiii       width is odd (and has a central character)
%          aaabbb             rightly behaviour : text is even: Correction = +1
%         aaabbb              leftly  behviour  : text is even: Correction =  0 (default)
%                             ActualOffset := (Width-1)/2 - (TextLen-1)/2 + Correction + (user-requested offset)
% ---

% :- debug(actual_offset).

actual_offset(IsOddTextLen,IsOddWidth,TextLen,Width,Offset,Prefer,ActualOffset) :-
   correction(Prefer,IsOddTextLen,IsOddWidth,Correction),
   % debug(actual_offset,"TextLen = ~d ~q  Width = ~d ~q",[TextLen,IsOddTextLen,Width,IsOddWidth]),
   (IsOddWidth   -> (HalfWidth    is (Width-1)/2)   ; (HalfWidth    is Width/2)), assertion(integer(HalfWidth)),
   (IsOddTextLen -> (HalfTextLen  is (TextLen-1)/2) ; (HalfTextLen  is TextLen/2)), assertion(integer(HalfTextLen)),
   ActualOffset is (HalfWidth - HalfTextLen + Correction + Offset).

% ---
% Determining the correction (0,-1,+1) top apply to a "centrally justified text"
% depending on various factors
% ---

correction(left,true,false,-1) :- !.
correction(right,false,true,1) :- !.
correction(_,_,_,0).

% ---
% Simple helpers
% ---

odd(X)  :- 1 =:= (X mod 2).

