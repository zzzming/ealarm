%% @author ezheluo
%% @doc A module to manage active and historical alarms.
%% @end


-module(alarm).

-include("alarm.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, quick_raise/5, raise/1, clear/1,
         get_active/0, get_historical/0,
         pr_active/0, pr_historical/0, help/0]).

help() -> not_ok.

start_link() ->
    case alarm_handler:start_link() of
    {ok, Pid} ->
        historical_alarm_handler:start_link(),
        {ok, Pid, ?MODULE};
    {error, {already_started, _Pid}} ->
        InstalledHandlers = gen_event:which_handlers(?ALARM_MANAGER),
        case lists:dropwhile(fun(X) -> historical_alarm_handler =/= X end, InstalledHandlers) of
            [] -> historical_alarm_handler:start_link();
            _  -> alarm_handler_already_installed
         end;
    Error -> Error
    end.
%%
%%
%% @doc An active alarm is tracked by the unique alarm id. A duplicated alarm id will not be added.
%%      
-spec raise(#alarm{}) -> {ok, #alarm{}} | {duplicated, #alarm{}}.
raise(ActiveAlarm) ->
    AlarmId = ActiveAlarm#alarm.alarmId,
    Alarms = alarm_handler:get_alarms(),
    CurrentTimeStamp = erlang:system_time(),
    DateAndTime = calendar:local_time_to_universal_time_dst(calendar:local_time()),
    case proplists:lookup(AlarmId, Alarms) of
        none ->
            NewAlarm = 
                ActiveAlarm#alarm{alarmRaisedTime=CurrentTimeStamp,
                                 alarmChangedTime=CurrentTimeStamp,
                                 alarmRaisedUTCTime=DateAndTime,
                                 alarmChangedUTCTime=DateAndTime},
            alarm_handler:set_alarm({AlarmId, NewAlarm}),
            {ok, NewAlarm};
        {AlarmId, FoundAlarm} ->
            NewCounter = 1 + FoundAlarm#alarm.counter,
            UpdatedAlarm = FoundAlarm#alarm{alarmChangedTime=CurrentTimeStamp,
                                                   alarmChangedUTCTime=DateAndTime,
                                                   counter=NewCounter},
            alarm_handler:clear_alarm(AlarmId),
            alarm_handler:set_alarm({AlarmId, UpdatedAlarm}),
            {duplicate, UpdatedAlarm}
    end.

%%
%%
%% @doc Quick raise an alarm without creating a lengthy record.
%%  
-spec quick_raise(atom(), alarm_severity(), atom(), string(), string()) ->
                        {ok, #alarm{}} | {duplicated, #alarm{}}.
quick_raise(AlarmId, Severity, Module, Description, Actions) ->
    Alarm = #alarm{alarmId = AlarmId,
                          perceivedSeverity = Severity,
                          specificProblem = Description,
                          proposedRepairActions = Actions,
                          source = Module},
    raise(Alarm).
%%
%%
%% @doc clear an active alarm.
%%      
-spec clear(atom()) -> {ok, #alarm{}} | unregistered_alarm.
clear(AlarmId) ->
    Alarms = alarm_handler:get_alarms(),
    case proplists:lookup(AlarmId, Alarms) of
        none         -> unregistered_alarm;
        {AlarmId, ClearedAlarm} ->
            historical_alarm_handler:add_alarm(ClearedAlarm),
            alarm_handler:clear_alarm(AlarmId),
            {ok, ClearedAlarm}
    end.

%%
%%
%% @doc nothings fancy just get the list of alarms by the default erlang alarm handler.
%%
-spec get_active() -> list().
get_active() ->
    alarm_handler:get_alarms().

-spec get_historical() -> list().
get_historical() ->
    historical_alarm_handler:get_alarms().

%%
%%
%% @doc print alarms in a neat format with record fields.
%%
-spec pr_active() -> list().
pr_active() ->
    [record_to_proplist(X) || {_Id, X} <- alarm:get_active()].

-spec pr_historical() -> list().
pr_historical() ->
    [record_to_proplist(X) || X <- historical_alarm_handler:get_alarms()].

%% ====================================================================
%% Internal functions
%% ====================================================================
record_to_proplist(#alarm{} = Rec) ->
    lists:zip(record_info(fields, alarm), tl(tuple_to_list(Rec))).