%% @author ming luo
%% @doc eunit for ealarm.


-module(ealarm_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../include/alarm.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

test_raise() ->
    alarm:start_link(),
    AlarmId = t123,
    alarm:quick_raise(AlarmId, major, not_me, "failure imminent", "self recover"),
    AA#alarm{alarmId=AlarmId0} = alarm:get_active(),
    ?_assert(AlarmId =:= AlarmId0).

%% ====================================================================
%% Internal functions
%% ====================================================================


