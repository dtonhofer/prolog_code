:- module(onepointfour_basics_dict_pp_decision,
          [
          decision_how_to_display/5 % decision_how_to_display(+Tag,+SettingsDict,-DecisionForTag,-DecisionForBorder,-DecisionForPadding)
          ]).

:- use_module(library('onepointfour_basics/checks.pl')).
:- use_module(library('onepointfour_basics/dict_pp/helpers.pl')).
:- use_module(library('onepointfour_basics/dict_settings.pl')).

%! decision_how_to_display(+Tag,+SettingsDict,-DecisionForTag,-DecisionForBorder,-DecisionForPadding)
%
% Determine what to do with a dict that shall be transformed into lines,
% given its Tag, which may be an unbound variable, and the settings in 
% SettingsDict.
%
% Instantiates as follows:
%
% | DecisionForTag         | =true=   | print the dict's tag  | 
% |                        | =false=  | do not print the dict's tag (the tag is never printed if it is an unbound variable) |
% | DecisionForBorder      | =true=   | decorate output with an ASCII border |
% |                        | =false=  | do not decorate output |
% | DecisionForPadding     | =true=   | pad around content lines with a (possibly zero-thickness) border of whitespace |
% |                        | =false=  | do not pad |

decision_how_to_display(Tag,SettingsDict,DecisionForTag,DecisionForBorder,DecisionForPadding) :-
   get_setting(SettingsDict, pad,        PadFlag,       false),
   get_setting(SettingsDict, sub_pad,    SubPadFlag,    inherit),
   get_setting(SettingsDict, border,     BorderFlag,    false),
   get_setting(SettingsDict, sub_border, SubBorderFlag, inherit),
   get_setting(SettingsDict, tag,        TagFlag,       true),
   get_setting(SettingsDict, sub_tag,    SubTagFlag,    inherit),
   get_setting(SettingsDict, depth,      Depth),                  % must exist in "SettingsDict", so no default
   % 
   % we don't want surprises or arbitrary failures later, so make sure we are on the rails
   %
   assertion(check_that(PadFlag       ,[hard(boolean)])),
   assertion(check_that(SubPadFlag    ,[hard(member([true,false,inherit]))])),
   assertion(check_that(BorderFlag    ,[hard(boolean)])),
   assertion(check_that(SubBorderFlag ,[hard(member([true,false,inherit]))])),
   assertion(check_that(TagFlag       ,[hard(boolean)])),
   assertion(check_that(SubTagFlag    ,[hard(member([true,false,inherit]))])),
   %
   % do we print the tag or not?
   %
   decision_for_tag(Depth,Tag,TagFlag,SubTagFlag,DecisionForTag),
   assertion(check_that(DecisionForTag,[hard(boolean)])),
   %
   % do we print an ASCII border or not?
   %
   decision_for_border(Depth,BorderFlag,SubBorderFlag,DecisionForBorder),
   assertion(check_that(DecisionForBorder,[hard(boolean)])),
   %
   % do we do padding or not?
   %
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight),
   all_zero_flag(PadTop,PadBottom,PadLeft,PadRight,AllPadsZero),
   decision_for_padding(Depth,PadFlag,SubPadFlag,AllPadsZero,DecisionForPadding),
   %
   % postcondition
   %
   assertion(check_that(DecisionForPadding,[hard(boolean)])),
   assertion(check_that(DecisionForTag,[hard(boolean)])),
   assertion(check_that(DecisionForBorder,[hard(boolean)])).

% decision_for_tag(+Depth,+Tag,+TagFlag,+SubTagFlag,-Decision)
%
% If "Tag" is an unbound variable -> do not print the tag
% else
% If we are at depth 0 (outermost dict) -> just do what "TagFlag" says
% else
% If we are at depth > 0 and the "SubTagFlag" is 'inherit' -> just do what "TagFlag" says
% else
% Do what the "SubTagFlag" says
%
% Note the reification of the boolean outcome in "Decision".
% Note that the guard conditions are superfluous due to the use of the cut.
% Leave them in anyway for readability.

decision_for_tag(_     , Tag ,       _ ,          _ , false)      :- var(Tag),!.
decision_for_tag(0     , Tag , TagFlag ,          _ , TagFlag)    :- nonvar(Tag),!.
decision_for_tag(Depth , Tag , TagFlag ,    inherit , TagFlag)    :- nonvar(Tag),Depth>0,!.
decision_for_tag(Depth , Tag ,       _ , SubTagFlag , SubTagFlag) :- nonvar(Tag),Depth>0,SubTagFlag\==inherit.

% decision_for_border(+Depth,+BorderFlag,+SubBorderFlag,-Decision)
% 
% If we are at depth 0 (outermost dict) -> just do what "BorderFlag" says
% else 
% If the "SubBorderFlag" is 'inherit' -> just do what "BorderFlag" says
% else
% Do what the "SubBorderFlag" says
%
% Note the reification of the boolean outcome in "Decision".
% Note that the guard conditions are superfluous due to the use of the cut.
% Leave them in anyway for readability.

decision_for_border(0     , BorderFlag ,             _ , BorderFlag)    :- !.
decision_for_border(Depth , BorderFlag ,       inherit , BorderFlag)    :- Depth>0,!.
decision_for_border(Depth ,          _ , SubBorderFlag , SubBorderFlag) :- Depth>0,SubBorderFlag\==inherit.

% decision_for_padding(+Depth,+PadFlag,+SubPadFlag,+AllPadsZero,-Decision)
%
% If we are at depth 0 (outermost dict) -> just do what "PadFlag" says
% ... because if "PadFlag" is 'true', then we want to pad, regardless of whether
%     "all pads are zero" or not (if they are all zero, we get a nice tight
%     rectangle of whitespace)
% ... and if "PadFlag" is 'false' then we explicitly don't want to pad
%     (padding by 0 will still be done if a border decoration was ordered)
% else 
% If "all pads are zero" then we don't want to pad
% ... because padding is useless: the text will be integrated into the 
%     representation of a higher dict and whitespace-padded in any case
% else
% If "SubPadFlag" is 'inherit' -> just do what "PadFlag" says
% else 
% Do what the "SubPadFlag" says
%
% Note the reification of the boolean outcome in "Decision".
% Note that the guard conditions are superfluous due to the use of the cut.
% Leave them in anyway for readability.

decision_for_padding(0     , PadFlag ,          _ ,     _ , PadFlag)    :- !.
decision_for_padding(Depth ,       _ ,          _ ,  true , false)      :- Depth>0,!.
decision_for_padding(Depth , PadFlag ,    inherit , false , PadFlag)    :- Depth>0,!.
decision_for_padding(Depth ,       _ , SubPadFlag , false , SubPadFlag) :- Depth>0,SubPadFlag\==inherit.

% Decide whether for a set of 4 numbers, are all 0, and reify the outcome
% in the last argument.

all_zero_flag(0,0,0,0,true) :- !.
all_zero_flag(_,_,_,_,false).

