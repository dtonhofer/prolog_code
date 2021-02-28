:- use_module('semver.pl').

% =============================================================================
% Unit test code for
% Semantic versioning string assembler/disassembler/verifier in Prolog
%
% V 2.0.0.
%
% Based on https://semver.org/spec/v2.0.0.html
%
% Unit Test code is based on the "plunit" framework available in SICStus and
% SWI-Prolog and maybe other. See:
% https://eu.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
% =============================================================================
% David Tonhofer (ronerycoder@gluino.name) says:
% This code is licensed under:
% "Zero-Clause BSD / Free Public License 1.0.0 (0BSD)"
% https://opensource.org/licenses/0BSD
% =============================================================================

:- begin_tests(semver).

test("empty is not allowed",fail) :-
   semantic_version('',_,_,_,_,_).

test("major-minor-patch disassemble 1",[nondet,true([Major,Minor,Patch,Pre,Bui]==[1,2,3,[],[]])]) :-
  semantic_version('1.2.3',Major,Minor,Patch,Pre,Bui).

test("major-minor-patch disassemble 2",[nondet,true([Major,Minor,Patch,Pre,Bui]==[0,0,0,[],[]])]) :-
  semantic_version('0.0.0',Major,Minor,Patch,Pre,Bui).

test("major-minor-patch disassemble 3",[nondet,true([Major,Minor,Patch,Pre,Bui]==[3,2,1,[],[]])]) :-
  semantic_version('3.2.1',Major,Minor,Patch,Pre,Bui).

test("major-minor-patch disassemble failure: no leading zeros",fail) :-
  semantic_version('03.2.1',_,_,_,_,_).

test("major-minor-patch disassemble failure: no alphanumerics",fail) :-
  semantic_version('v3.2.1',_,_,_,_,_).

test("major-minor-patch assemble 1",[true(Text=='1.2.3')]) :-
  semantic_version(Text,1,2,3,[],[]).

test("major-minor-patch assemble 2",[true(Text=='0.0.0')]) :-
  semantic_version(Text,0,0,0,[],[]).

test("bad pre-release id #1",fail) :-
   semantic_version(_,1,2,3,['0067'],[]). % must be integer or alphanumeric; fails because the grammar fails

test("bad pre-release id #2",[error(type_error(_,_))]) :-
   semantic_version(_,1,2,3,[3.1415],[]). % must be integer or alphanumeric; throws because entry test catches it

test("disassemble+assemble #1",[nondet,true(Text=='1.2.3')]) :-
  semantic_version(Text,1,2,3,[],[]), % assemble into Text
  semantic_version(Text,1,2,3,[],[]). % disassemble Text and verify

test("disassemble+assemble #2",[nondet,true(Text=='1.2.3-10.20.30.40+foo.bar.baz')]) :-
  semantic_version(Text,1,2,3,[10,20,30,40],[foo,bar,baz]), % assemble into Text
  semantic_version(Text,1,2,3,[10,20,30,40],[foo,bar,baz]). % disassemble Text and verify

test("disassemble+assemble #3",[nondet,true(Text=='1.2.3-10.abc.4-5.00x67.order66')]) :-
  semantic_version(Text,1,2,3,[10,abc,'4-5','00x67',order66],[]), % assemble into Text
  semantic_version(Text,1,2,3,[10,abc,'4-5','00x67',order66],[]). % disassemble Text and verify

test("disassemble+assemble #4",[nondet,true(Text=='1.2.3-00x67+alpha.foo-bar.0099')]) :-
  semantic_version(Text,1,2,3,['00x67'],[alpha,'foo-bar','0099']), % assemble into Text
  semantic_version(Text,1,2,3,['00x67'],[alpha,'foo-bar','0099']). % disassemble Text and verify

test("disassemble+assemble #5",[nondet,true(Text=='1.2.3+bravo.foo-bar.0099')]) :-
  semantic_version(Text,1,2,3,[],[bravo,'foo-bar','0099']), % assemble into Text
  semantic_version(Text,1,2,3,[],[bravo,'foo-bar','0099']). % disassemble Text and verify

test("disassemble+assemble #7",fail) :-
  semantic_version(_,1,2,3,['00001'],[]). % Prerelease '00001' cannot be represented; it must be an integer

test("disassemble+assemble #8",[nondet,true(Text=='1.2.3-00001x')]) :-
  semantic_version(Text,1,2,3,['00001x'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['00001x'],[]). % disassemble Text and verify

test("disassemble+assemble #9",fail) :-
  semantic_version(_,1,2,3,['111111'],[]). % Prerelease '1111111' cannot be represented; it must be an integer

test("disassemble+assemble #10",[nondet,true(Text=='1.2.3-111111x')]) :-
  semantic_version(Text,1,2,3,['111111x'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['111111x'],[]). % disassemble Text and verify

test("disassemble+assemble #11",[nondet,true(Text== '1.2.3-x')]) :-
  semantic_version(Text,1,2,3,['x'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['x'],[]). % disassemble Text and verify

test("disassemble+assemble #12",[nondet,true(Text== '1.2.3-x000000')]) :-
  semantic_version(Text,1,2,3,['x000000'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['x000000'],[]). % disassemble Text and verify

test("disassemble+assemble #13",[nondet,true(Text== '1.2.3-abcx')]) :-
  semantic_version(Text,1,2,3,['abcx'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['abcx'],[]). % disassemble Text and verify

test("disassemble+assemble #14",[nondet,true(Text== '1.2.3-abcx000000')]) :-
  semantic_version(Text,1,2,3,['abcx000000'],[]), % assemble into Text
  semantic_version(Text,1,2,3,['abcx000000'],[]). % disassemble Text and verify

:- end_tests(semver).

