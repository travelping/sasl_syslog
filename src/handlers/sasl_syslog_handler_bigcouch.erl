-module(sasl_syslog_handler_bigcouch).
-export([event_to_report/1]).
-include("sasl_syslog.hrl").
-behaviour(sasl_syslog_h).

event_to_report({Atom, _, _} = Message) ->
    {ok, Options} = application:get_env(sasl_syslog, ?MODULE),
    {events, Events} = lists:keyfind(events, 1, Options),
    case lists:member(Atom, Events) of
        true -> handle_report_event(Message);
        false -> undefined
    end;
event_to_report(_) ->
    undefined.

handle_report_event({Atom, _ConMsg, FileMsg}) ->
    #report{text = FileMsg,
            type = type(Atom),
            subtype = undefined,
            pid = sasl_syslog_h:process_pid(self()),
            process_name = sasl_syslog_h:process_name(self()),
            data = FileMsg}.

type(couch_info) -> info;
type(_) -> error.
