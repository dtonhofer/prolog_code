
%! get_setting(+SettingsDict,+Key,?Value,+Default)
% 
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing, unifies Value with Default.

get_setting(SettingsDict,Key,Value,_Default) :-
   get_dict(Key,SettingsDict,Value),              % get_dict/3 succeeds if entry with "Key" exists
   !.

get_setting(_,_,ValueIsDefault,ValueIsDefault).   % fallback to "Default"

%! get_setting(+SettingsDict,+Key,?Value)
%
% Instantiate Value to the value stored under Key in SettingsDict. If it
% is missing a (resolutely non-ISO) error term 
% error(dict_error(missing_entry,Key))
% is thrown. 
% TODO: add a top-level printer for this error term.

get_setting(SettingsDict,Key,Value) :-         
   get_dict(Key,SettingsDict,Value),              % get_dict/3 succeeds if entry with "Key" exists
   !.

get_setting(SettingsDict,Key,_) :- 
   throw(error(dict_error(missing_entry,Key))).

%! get_padding_settings(+SettingDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get multiple specific values, those for paddings, in one call.
% This predicate also checks that the retrieved values are all integers >= 0.

get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_setting(SettingsDict,pad_top,    PadTop    ,0),
   get_setting(SettingsDict,pad_bottom, PadBottom ,0),
   get_setting(SettingsDict,pad_left,   PadLeft   ,0),
   get_setting(SettingsDict,pad_right,  PadRight  ,0),
   check_that(PadTop,hard(pos0int)),
   check_that(PadBottom,hard(pos0int)),
   check_that(PadLeft,hard(pos0int)),
   check_that(PadRight,hard(pos0int)).

%! get_padding_settings_modulated(+DecisionForPadding,+SettingsDict,-PadTop,-PadBottom,-PadLeft,-PadRight)
%
% Get the settings for the padding, but clamp them all to 0 if DecisionForPadding is =false=.

get_padding_settings_modulated(false,_,0,0,0,0) :- !.

get_padding_settings_modulated(true,SettingsDict,PadTop,PadBottom,PadLeft,PadRight) :-
   get_padding_settings(SettingsDict,PadTop,PadBottom,PadLeft,PadRight).

