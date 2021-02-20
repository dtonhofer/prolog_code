% digits_ng(Sign,Plus,Minus,Zeros,ValueDigits)
%
% Accepts/Recognizes a list of chars or codes that represents a possibly signed
% decimal integer with possibly leading zeros. On success:
%
% Sign is +1 if the integer is positive, -1 if negative, 0 if it is actually 0.
% Plus is 'x' if there was no plus sign, '+' if there was a plus sign
% Minus is 'x' if there was no minus sign, '-' if there was a minus sign
% Zeros is a list of codes or chars (depending on input) containing all the
%   leading zeros found. If the input contains only 0s, this list contains 
%   all but one of the zeros.
% ValueDigits is the list of value digits, without any leading sign and without any
%   leading zeros.
%
% When you want to force certain forms, you can just prefill the corresponding
% variables. For example, Plus='+' requires a '+' sign.

digits_ng(Sign,Plus,Minus,Zeros,ValueDigits) -->
   { nonvar(Sign) -> Sign2=Sign ; true },
   digits_ng_2(Sign2,Plus,Minus,Zeros,ValueDigits),
   { suffix_zero(ValueDigits) -> Sign=0 ; Sign=Sign2 }.
   
digits_ng_2(Sign,Plus,Minus,Zeros,ValueDigits) -->
   maybe_sign(Sign,Plus,Minus),        % grab any + or -
   leading_zeros(Zeros),               % grab leading zeros with lookahead
   digits_suffix(ValueDigits).              % greedily follow up with a suffix

% ---
% Verify whether the suffix is 0, i.e. a list with 1 code/char indicating 0.
% suffix_zero(ValueDigits)
% ---

suffix_zero([C]) :- zero_p(C).

% ---
% Some tests on a char/code
% Using code_types/2 allows us to transparently work with codes or chars.
% ---

zero_p(C)     :- code_type(C,digit(0)).      % alternative to C=='0';C==48
nonzero_p(C)  :- code_type(C,digit(D)),D>0.
nondigit_p(C) :- \+code_type(C,digit).

zero(C)    --> [C],{zero_p(C)}.
nonzero(C) --> [C],{nonzero_p(C)}.

% ---
% Lookaheads
% ---

nondigit_follows,[C] --> [C],!,{nondigit_p(C)}.
nondigit_follows     --> [].

zero_follows,[C]     --> zero(C).
nonzero_follows,[C]  --> nonzero(C).

% ---
% Greedily grab the suffix
% ---

digits_suffix([C|Cs]) --> nonzero(C),!,any_digits(Cs).
digits_suffix([C])    --> zero(C),nondigit_follows.

% ---
% maybe_leading_zeros(ZeroAsCodeOrChar).
% Non-greedily (because we may have to back off to leave a final 0) accept an arbitrary number of leading zeros.
% ---

maybe_leading_zeros([])     --> [].
maybe_leading_zeros([C|Cs]) --> zero(C),maybe_leading_zeros(Cs).

% ---
% leading_zeros(ZeroAsCodeOrChar).
% An alternative that uses a look-ahead to get rid of indeterminacy.
% In effect, we are moving the "backtracking stack" from the execution stack to the code/char list
% ---

leading_zeros([C|Cs]) --> zero(C),zero_follows,!,leading_zeros(Cs). % 0 followed by 0
leading_zeros([C])    --> zero(C),nonzero_follows,!.                % 0 followed by some other digit$
leading_zeros([])     --> [].                                       % other cases (end of input or 0 followed by a non-digit)

% ---
% maybe_sign(Sign,PlusFound,MinusFound).
% Maybe there is a plus or minus sign. Grab it greedily.
% ---

maybe_sign(+1,'+','x') --> (['+']|[43]),!.
maybe_sign(-1,'x','-') --> (['-']|[45]),!.
maybe_sign(+1,'x','x') --> [].

% ---
% any_digits(ListOfDigitsAsCodesOrChars)
% Greedily accept an arbitrary sequence of digits, including none, maybe with leading zeros.
% ---

any_digits([C|Cs]) --> [C],{code_type(C,digit)},!,any_digits(Cs).
any_digits([])     --> [].
