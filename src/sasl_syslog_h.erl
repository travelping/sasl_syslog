% Copyright 2011, Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(sasl_syslog_h).
-behaviour(gen_event).

-export([attach/0, detach/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("sasl_syslog.hrl").

%% ------------------------------------------------------------------------------------------
%% -- misc API
attach() ->
    Handlers = gen_event:which_handlers(error_logger),
    case lists:member(?MODULE, Handlers) of
        false ->
            error_logger:add_report_handler(?MODULE);
        _ ->
            ok
    end.

detach() ->
    detach_all(ok).

%% @private
detach_all(ok) ->
    detach_all(error_logger:delete_report_handler(?MODULE));
detach_all(_) ->
    ok.

%% ------------------------------------------------------------------------------------------
%% -- gen_event callbacks
-record(state, {socket, emulator_pid}).

%% @private
init(_) ->
    {ok, SyslogPort} = application:get_env(sasl_syslog, local_port),
    {ok, Socket} = sasl_syslog:open_socket(SyslogPort),
    {ok, #state{socket = Socket, emulator_pid = os:getpid()}}.

%% @private
handle_event(Event, State = #state{socket = Socket, emulator_pid = EmulatorPid}) ->
    case event_to_msg(Event) of
        undefined ->
            {ok, State};
        MsgRec1 ->
            {ok, RemoteHost} = application:get_env(sasl_syslog, remote_host),
            {ok, RemotePort} = application:get_env(sasl_syslog, remote_port),
            {ok, Facility}   = application:get_env(sasl_syslog, facility),
            MsgRec2 = MsgRec1#msg{timestamp = erlang:universaltime(),
                                  facility = Facility,
                                  procid = EmulatorPid,
                                  appname = "beam",
                                  hostname = nodename_to_host(node())},
            lists:foreach(fun (MsgRec3) ->
                                  sasl_syslog:send(Socket, RemoteHost, RemotePort, MsgRec3)
                          end, maybe_split_msg(MsgRec2)),
            {ok, State}
    end.

%% @private
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{socket = Socket}) ->
    sasl_syslog:close_socket(Socket),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers
maybe_split_msg(Msg = #msg{msg = Text}) ->
    case application:get_env(sasl_syslog, multiline) of
        undefined   ->
            [Msg];
        {ok, true}  ->
            [Msg];
        {ok, false} ->
            BinText = unicode:characters_to_binary(Text),
            [Msg#msg{msg = Line} || Line <- binary:split(BinText, <<"\n">>, [global])]
    end.

event_to_msg({error, _GL, Report}) ->
    fmt_msg(error, Report);
event_to_msg({error_report, _GL, Report}) ->
    report_msg(error, Report);
event_to_msg({warning_msg, _GL, Report}) ->
    fmt_msg(warning, Report);
event_to_msg({warning_report, _GL, Report}) ->
    report_msg(warning, Report);
event_to_msg({info_msg, _GL, Report}) ->
    fmt_msg(info, Report);
event_to_msg({info_report, _GL, Report}) ->
    report_msg(info, Report);
event_to_msg(_) ->
    undefined.

fmt_msg(Severity, {Pid, Format, Args}) ->
    case (catch io_lib:format(Format, Args)) of
        {'EXIT', _} -> undefined;
        ReportText  -> #msg{msg = ReportText, severity = Severity, msgid = process_name(Pid)}
    end.

report_msg(Severity, {Pid, _Type, Report}) ->
    #msg{msg = report_text(Report), severity = Severity, msgid = process_name(Pid)}.

process_name(Pid) ->
    case process_info(Pid, registered_name) of
        []                        -> pid_to_list(Pid);
        undefined                 -> pid_to_list(Pid);
        [{registered_name, Name}] -> atom_to_list(Name);
        {registered_name, Name}   -> atom_to_list(Name)
    end.

nodename_to_host('nonode@nohost') ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
nodename_to_host(Node) ->
    re:replace(atom_to_list(Node), "@", ".").

report_text(Report) ->
    try
        unicode:characters_to_binary(Report)
    catch
        error:badarg ->
            if
                is_list(Report) -> unicode:characters_to_binary(string:join(lists:map(fun pp_line/1, Report), "\n"));
                true            -> unicode:characters_to_binary(io_lib_pretty:print(Report))
            end
    end.

pp_line({Tag, Data}) ->
    io_lib:format("~p: ~p", [Tag, Data]);
pp_line(Term) ->
    io_lib_pretty:print(Term).
