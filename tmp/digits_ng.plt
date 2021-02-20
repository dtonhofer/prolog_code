
:- begin_tests(helpers).

test("any digits, no rest (chars)") :- 
   accept_any_digits('01276',Acc,Rest,chars),
   assertion((Acc == ['0','1','2','7','6'], Rest == [])).
   
test("any digits, with rest (chars)") :- 
   accept_any_digits('01276x',Acc,Rest,chars),
   assertion((Acc == ['0','1','2','7','6'], Rest == ['x'])).

test("any digits, no rest (codes)") :- 
   accept_any_digits('01276',Acc,Rest,codes),
   assertion((Acc == [48,49,50,55,54], Rest == [])).
   
test("any digits, with rest (codes)") :- 
   accept_any_digits('01276x',Acc,Rest,codes),
   assertion((Acc == [48,49,50,55,54], Rest == [120])).

test("any digits, but there are none (chars)") :- 
   accept_any_digits('',Acc,Rest,chars),
   assertion((Acc == [], Rest == [])).

test("any digits, but there are none (chars)") :- 
   accept_any_digits('x',Acc,Rest,chars),
   assertion((Acc == [], Rest == ['x'])).

test("any digits, no rest (chars)") :- 
   accept_any_digits('01276',Acc,Rest,chars),
   assertion((Acc == ['0','1','2','7','6'], Rest == [])).
  
test("accept leading zeros, many zeros (chars)") :-
   findall(Acc-Rest,accept_leading_zeros('00001',Acc,Rest,chars),All),
   assertion(All == [[]-['0','0','0','0','1'],
                     ['0']-['0','0','0','1'],
                     ['0','0']-['0','0','1'],
                     ['0','0','0']-['0','1'],
                     ['0','0','0','0']-['1']]).

test("accept leading zeros, many zeros (codes)") :-
   findall(Acc-Rest,accept_leading_zeros('00001',Acc,Rest,codes),All),
   assertion(All == [[]-[48,48,48,48,49], 
                     [48]-[48,48,48,49],
                     [48,48]-[48,48,49],
                     [48,48,48]-[48,49],
                     [48,48,48,48]-[49]]).
                     
test("accept leading zeros, nothing") :-
   once(accept_leading_zeros('',Acc,Rest,chars)),
   assertion((Acc==[],Rest==[])).
   
test("accept leading zeros, no zeros just a 1 (chars)") :-
   once(accept_leading_zeros('1',Acc,Rest,chars)),
   assertion((Acc==[],Rest==['1'])).
   
test("accept leading zeros, no zeros just a 1 (codes)") :-
   once(accept_leading_zeros('1',Acc,Rest,codes)),
   assertion((Acc==[],Rest==[49])).
   
:- end_tests(helpers).

:- begin_tests(digits_ng).

test("empty",fail) :- 
   accept_digits_ng('',_Sign,_Plus,_Minus,_Zeros,_ValueDigits,_Rest,chars).
   
test("plus sign only",fail) :- 
   accept_digits_ng('+',_Sign,_Plus,_Minus,_Zeros,_ValueDigits,_Rest,chars).

test("minus sign only",fail) :- 
   accept_digits_ng('-',_Sign,_Plus,_Minus,_Zeros,_ValueDigits,_Rest,chars).
   
test("1234 (chars)") :- 
   accept_digits_ng('1234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==[],ValueDigits=['1','2','3','4'],Rest==[])).

test("1234 (codes)") :- 
   accept_digits_ng('1234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==[],ValueDigits=[49,50,51,52],Rest==[])).

test("1234x (chars)") :- 
   accept_digits_ng('1234x',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==[],ValueDigits=['1','2','3','4'],Rest==['x'])).

test("1234x (codes)") :- 
   accept_digits_ng('1234x',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==[],ValueDigits=[49,50,51,52],Rest==[120])).

test("0001234 (chars)") :- 
   accept_digits_ng('0001234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==['0','0','0'],ValueDigits=['1','2','3','4'],Rest==[])).

test("0001234 (codes)") :- 
   accept_digits_ng('0001234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==1,Plus==x,Minus==x,Zeros==[48,48,48],ValueDigits=[49,50,51,52],Rest==[])).

test("0 (chars)") :- 
   accept_digits_ng('0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==[],ValueDigits=['0'],Rest==[])).

test("0 (codes)") :- 
   accept_digits_ng('0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==[],ValueDigits=[48],Rest==[])).

test("+0 (chars)") :- 
   accept_digits_ng('+0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==(+),Minus==x,Zeros==[],ValueDigits=['0'],Rest==[])).

test("+0 (codes)") :- 
   accept_digits_ng('+0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==0,Plus==(+),Minus==x,Zeros==[],ValueDigits=[48],Rest==[])).

test("-0 (chars)") :- 
   accept_digits_ng('-0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==x,Minus==(-),Zeros==[],ValueDigits=['0'],Rest==[])).
   
test("-0 (codes)") :- 
   accept_digits_ng('-0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==0,Plus==x,Minus==(-),Zeros==[],ValueDigits=[48],Rest==[])).
   
test("0x (chars)") :- 
   accept_digits_ng('0x',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==[],ValueDigits=['0'],Rest==['x'])).

test("0x (codes)") :- 
   accept_digits_ng('0x',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==[],ValueDigits=[48],Rest==[120])).
   
test("0000 (chars)") :- 
   accept_digits_ng('0000',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==['0','0','0'],ValueDigits=['0'],Rest==[])).

test("0000 (codes)") :- 
   accept_digits_ng('0000',Sign,Plus,Minus,Zeros,ValueDigits,Rest,codes),
   assertion((Sign==0,Plus==x,Minus==x,Zeros==[48,48,48],ValueDigits=[48],Rest==[])).

test("-1234 (chars)") :- 
   accept_digits_ng('-1234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==(-1),Plus==x,Minus==(-),Zeros==[],ValueDigits=['1','2','3','4'],Rest==[])).

test("+1234 (chars)") :- 
   accept_digits_ng('+1234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==1,Plus==(+),Minus==x,Zeros==[],ValueDigits=['1','2','3','4'],Rest==[])).

test("+001234 (chars)") :- 
   accept_digits_ng('+001234',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==1,Plus==(+),Minus==x,Zeros==['0','0'],ValueDigits=['1','2','3','4'],Rest==[])).

test("+0 (chars)") :- 
   accept_digits_ng('+0',Sign,Plus,Minus,Zeros,ValueDigits,Rest,chars),
   assertion((Sign==0,Plus==(+),Minus==x,Zeros==[],ValueDigits=['0'],Rest==[])).

test("Demand 'no plus sign' an 'no leading zeros' #1", fail) :- 
   accept_digits_ng('-000018889',_Sign,x,_Minus,[],_ValueDigits,_Rest,chars).
   
test("Demand 'no plus sign' an 'no leading zeros' #2") :- 
   accept_digits_ng('-18889',Sign,x,Minus,[],ValueDigits,Rest,chars),
   assertion((Sign==(-1),Minus==(-),ValueDigits==['1','8','8','8','9'],Rest==[])).
   
test("Demand 'no plus sign' an 'no leading zeros' #3") :- 
   accept_digits_ng('-0',Sign,x,Minus,[],ValueDigits,Rest,chars), % Very edge case: -0
   assertion((Sign==0,Minus==(-),ValueDigits==['0'],Rest==[])).
   
test("Demand 'no plus sign' an 'no leading zeros' #4", fail) :- 
   accept_digits_ng('+0',_Sign,x,_Minus,[],_ValueDigits,_Rest,chars). % Very edge case: +0

test("Demand 'no plus sign' an 'no leading zeros' #5") :- 
   accept_digits_ng('18889',Sign,x,Minus,[],ValueDigits,Rest,chars),
   assertion((Sign==1,Minus==(x),ValueDigits==['1','8','8','8','9'],Rest==[])).

:- end_tests(digits_ng).

% ---
% Parsing helpers
% ---

accept_any_digits(Atom,Acc,Rest,How) :-
   assertion(member(How,[chars,codes])),
   explode(How,Atom,List),
   phrase(any_digits(Acc),List,Rest).

accept_leading_zeros(Atom,Acc,Rest,How) :-
   assertion(member(How,[chars,codes])),
   explode(How,Atom,List),
   phrase(maybe_leading_zeros(Acc),List,Rest).

accept_digits_ng(Atom,Sign,Plus,Minus,Zeros,ValueDigits,Rest,How) :-
   assertion(member(How,[chars,codes])),
   explode(How,Atom,List),
   phrase(digits_ng(Sign,Plus,Minus,Zeros,ValueDigits),List,Rest).

accept_dcg_basics_digits(Atom,Digits,Rest,How) :-
   assertion(member(How,[chars,codes])),
   explode(How,Atom,List),
   phrase(dcg_basics:digits(Digits),List,Rest).
   
explode(chars,Atom,List) :- atom_chars(Atom,List).
explode(codes,Atom,List) :- atom_codes(Atom,List).
