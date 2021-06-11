% =============================================================================
% Unit test code for semantic versioning string V2.0.0 assembler/disassembler 
% in Prolog. See https://semver.org/spec/v2.0.0.html for the specification.
%
% The unit Test code is based on the "plunit" framework available in SICStus 
% and SWI-Prolog and maybe other Prologs.
%
% Homepage for this code:
%
% https://github.com/dtonhofer/prolog_code/tree/main/unpacked/onepointfour_semver
%
% This code uses some specificities of SWI-Prolog: 
%
% - dicts (associative arrays)
% - the Perl Compatible Regular Expression (pcre) module
%   https://eu.swi-prolog.org/pldoc/man?section=pcre which, however, is just
%   used to run supplementary tests against a semver string regular expression.
%   The use of PCRE and the tests which use it can be safely commented out.
% =============================================================================
% License information
%
% Author:  David Tonhofer (ronerycoder@gluino.name) 
% License: Zero-Clause BSD / Free Public License 1.0.0 (0BSD)
%          https://opensource.org/licenses/0BSD
% =============================================================================

:- use_module(library('onepointfour_semver/semver.pl')). 

% ---
% Example strings that must be *accepted*, from https://regex101.com/r/Ly7O1x/3/
% ---

example(positive,'0.0.4',[major:"0",minor:"0",patch:"4"]).
example(positive,'1.2.3',[major:"1",minor:"2",patch:"3"]).
example(positive,'10.20.30',[major:"10",minor:"20",patch:"30"]).
example(positive,'1.1.2-prerelease+meta',[buildmetadata:"meta",major:"1",minor:"1",patch:"2",prerelease:"prerelease"]).
example(positive,'1.1.2+meta',[buildmetadata:"meta",major:"1",minor:"1",patch:"2",prerelease:""]).
example(positive,'1.1.2+meta-valid',[buildmetadata:"meta-valid",major:"1",minor:"1",patch:"2",prerelease:""]).
example(positive,'1.0.0-alpha',[major:"1",minor:"0",patch:"0",prerelease:"alpha"]).
example(positive,'1.0.0-beta',[major:"1",minor:"0",patch:"0",prerelease:"beta"]).
example(positive,'1.0.0-alpha.beta',[major:"1",minor:"0",patch:"0",prerelease:"alpha.beta"]).
example(positive,'1.0.0-alpha.beta.1',[major:"1",minor:"0",patch:"0",prerelease:"alpha.beta.1"]).
example(positive,'1.0.0-alpha.1',[major:"1",minor:"0",patch:"0",prerelease:"alpha.1"]).
example(positive,'1.0.0-alpha0.valid',[major:"1",minor:"0",patch:"0",prerelease:"alpha0.valid"]).
example(positive,'1.0.0-alpha.0valid',[major:"1",minor:"0",patch:"0",prerelease:"alpha.0valid"]).
example(positive,'1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay',[buildmetadata:"build.1-aef.1-its-okay",major:"1",minor:"0",patch:"0",prerelease:"alpha-a.b-c-somethinglong"]).
example(positive,'1.0.0-rc.1+build.1',[buildmetadata:"build.1",major:"1",minor:"0",patch:"0",prerelease:"rc.1"]).
example(positive,'2.0.0-rc.1+build.123',[buildmetadata:"build.123",major:"2",minor:"0",patch:"0",prerelease:"rc.1"]).
example(positive,'1.2.3-beta',[major:"1",minor:"2",patch:"3",prerelease:"beta"]).
example(positive,'10.2.3-DEV-SNAPSHOT',[major:"10",minor:"2",patch:"3",prerelease:"DEV-SNAPSHOT"]).
example(positive,'1.2.3-SNAPSHOT-123',[major:"1",minor:"2",patch:"3",prerelease:"SNAPSHOT-123"]).
example(positive,'1.0.0',[major:"1",minor:"0",patch:"0"]).
example(positive,'2.0.0',[major:"2",minor:"0",patch:"0"]).
example(positive,'1.1.7',[major:"1",minor:"1",patch:"7"]).
example(positive,'2.0.0+build.1848',[buildmetadata:"build.1848",major:"2",minor:"0",patch:"0",prerelease:""]).
example(positive,'2.0.1-alpha.1227',[major:"2",minor:"0",patch:"1",prerelease:"alpha.1227"]).
example(positive,'1.0.0-alpha+beta',[buildmetadata:"beta",major:"1",minor:"0",patch:"0",prerelease:"alpha"]).
example(positive,'1.2.3----RC-SNAPSHOT.12.9.1--.12+788',[buildmetadata:"788",major:"1",minor:"2",patch:"3",prerelease:"---RC-SNAPSHOT.12.9.1--.12"]).
example(positive,'1.2.3----R-S.12.9.1--.12+meta',[buildmetadata:"meta",major:"1",minor:"2",patch:"3",prerelease:"---R-S.12.9.1--.12"]).
example(positive,'1.2.3----RC-SNAPSHOT.12.9.1--.12',[major:"1",minor:"2",patch:"3",prerelease:"---RC-SNAPSHOT.12.9.1--.12"]).
example(positive,'1.0.0+0.build.1-rc.10000aaa-kk-0.1',[buildmetadata:"0.build.1-rc.10000aaa-kk-0.1",major:"1",minor:"0",patch:"0",prerelease:""]).
example(positive,'99999999999999999999999.999999999999999999.99999999999999999',[major:"99999999999999999999999",minor:"999999999999999999",patch:"99999999999999999"]).
example(positive,'1.0.0-0A.is.legal',[major:"1",minor:"0",patch:"0",prerelease:"0A.is.legal"]).

% ---
% Example strings that must be *rejected*, from https://regex101.com/r/Ly7O1x/3/
% ---

example(negative,'1').
example(negative,'1.2').
example(negative,'1.2.3-0123').
example(negative,'1.2.3-0123.0123').
example(negative,'1.1.2+.123').
example(negative,'+invalid').
example(negative,'-invalid').
example(negative,'-invalid+invalid').
example(negative,'-invalid.01').
example(negative,'alpha').
example(negative,'alpha.beta').
example(negative,'alpha.beta.1').
example(negative,'alpha.1').
example(negative,'alpha+beta').
example(negative,'alpha_beta').
example(negative,'alpha.').
example(negative,'alpha..').
example(negative,'beta').
example(negative,'1.0.0-alpha_beta').
example(negative,'-alpha.').
example(negative,'1.0.0-alpha..').
example(negative,'1.0.0-alpha..1').
example(negative,'1.0.0-alpha...1').
example(negative,'1.0.0-alpha....1').
example(negative,'1.0.0-alpha.....1').
example(negative,'1.0.0-alpha......1').
example(negative,'1.0.0-alpha.......1').
example(negative,'01.1.1').
example(negative,'1.01.1').
example(negative,'1.1.01').
example(negative,'1.2').
example(negative,'1.2.3.DEV').
example(negative,'1.2-SNAPSHOT').
example(negative,'1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788').
example(negative,'1.2-RC-SNAPSHOT').
example(negative,'-1.0.3-gamma+b7718').
example(negative,'+justmeta').
example(negative,'9.8.7+meta+meta').
example(negative,'9.8.7-whatever+meta+meta').
example(negative,'99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12').

% ---
% Plunit module with test cases for accepting/rejecting the examples using
% the PCRE provided by a third party. If you want, you can comment out this
% pluint block as it *tests the examples*, not the semver DCG.
% ---

:- begin_tests(semver_regex).

:- use_module(library(pcre)). 

regex_atom(RegexAtom) :-
   atomic_list_concat(
      [
          '^(?P<major>0|[1-9]\\d*)'
         ,'\\.'
         ,'(?P<minor>0|[1-9]\\d*)'
         ,'\\.'
         ,'(?P<patch>0|[1-9]\\d*)'
         ,'(?:-(?P<prerelease>(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\\.(?:0|[1-9]\\d*|\\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?'
         ,'(?:\\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\\.[0-9a-zA-Z-]+)*))?$'
      ],RegexAtom).

regex_blob(RegexBlob) :-
   regex_atom(RegexAtom),
   re_compile(RegexAtom,RegexBlob,[]).

test(accept_via_pcre) :-
   regex_blob(RegexBlob),
   forall(
      example(positive,Text,Captures),         % backtrack over examples
      (re_matchsub(RegexBlob,Text,Dict,[]),    % test that matching succeeds and delivers expected captures
       del_dict(0,Dict,_,DictCleaned),         % 0 stores the whole capture
       dict_create(DictExpected,_,Captures),   % create a dict from the list of Key:Value pairs
       DictExpected = DictCleaned)).           % tag must unify, keys must must and values must unify

test(reject_via_pcre) :-
   regex_blob(RegexBlob),
   forall(
      example(negative,Text),                   % backtrack over examples
      \+ re_matchsub(RegexBlob,Text,_Dict,[])). % test that matching fails
 
:- end_tests(semver_regex).

% ---
% Testing the semver DCG.
% 
% The DCG (and its surrounding "helper" code) can 
% - disassemble a semantic version string into its components and also
% - assemble a semantic version string from given components 
%   (so no need to have separate string generation code).
% ---

:- begin_tests(semver_dcg).

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

:- end_tests(semver_dcg).

% ---
% Accept or reject semver example strings using semver DCG
% ---

:- begin_tests(accept_reject_strings_using_semver_dcg).

test("accept semver strings") :-
   forall(example(positive,Text), semantic_version(Text,_Maj,_Min,_Pat,_Pre,_Bui)).

test("reject semver strings") :-
   forall(example(negative,Text), \+semantic_version(Text,_Maj,_Min,_Pat,_Pre,_Bui)).

:- end_tests(accept_reject_strings_using_semver_dcg).

