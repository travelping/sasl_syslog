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

-type process() :: pid() | atom().
-type group_leader() ::  pid() | atom().
-type std_report_type() :: std_error | std_warning | std_info.
-type sasl_report_type() :: supervisor_report | crash_report | progress.
-type report_type() :: std_report_type() | sasl_report_type() | atom().

-type event() :: {error, group_leader(), {process(), string(), list()}}
               | {error_report, group_leader(), {process(), report_type(), Report}}
               | {warning_msg, group_leader(), {process(), string(), list()}}
               | {warning_report, group_leader(), {process(), report_type(), Report}}
               | {info_msg, group_leader(), {process(), string(), list()}}
               | {info_report, group_leader(), {process(), report_type(), Report}}
               | term().

%% ------------------------------------------------------------------------------------------
%% -- misc API
-spec attach() -> ok.
attach() ->
    Handlers = gen_event:which_handlers(error_logger),
    case lists:member(?MODULE, Handlers) of
        false ->
            error_logger:add_report_handler(?MODULE);
        _ ->
            ok
    end.

-spec detach() -> ok.
detach() ->
    detach_all(ok).

%% @private
detach_all(ok) ->
    detach_all(error_logger:delete_report_handler(?MODULE));
detach_all(_) ->
    ok.

%% ------------------------------------------------------------------------------------------
%% -- gen_event callbacks
-record(state, {
    socket       :: sasl_syslog:udp_socket(),
    emulator_pid :: string()
}).

%% @private
init(_) ->
    {ok, SyslogPort} = application:get_env(sasl_syslog, local_port),
    {ok, Socket} = sasl_syslog:open_socket(SyslogPort),
    {ok, #state{socket = Socket, emulator_pid = os:getpid()}}.

%% @private
-spec handle_event(event(), #state{}) -> {ok, #state{}}.
handle_event(Event, State = #state{socket = Socket, emulator_pid = EmulatorPid}) ->
    Timestamp = os:timestamp(),
    case event_to_report(Event) of
        undefined ->
            {ok, State};
        Report1 ->
            {ok, RemoteHost} = application:get_env(sasl_syslog, remote_host),
            {ok, Formatter}  = application:get_env(sasl_syslog, formatter),
            RemotePort = sasl_syslog:get_remote_port(),

            Report2 = Report1#report{beam_pid = EmulatorPid,
                                     host = nodename_to_host(node()),
                                     node = atom_to_list(node()),
                                     timestamp = Timestamp},
            Formatter:send_report(Socket, RemoteHost, RemotePort, Report2),
            {ok, State}
    end.

%% @private
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{socket = Socket}) ->
    sasl_syslog:close_socket(Socket).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------------------------------
%% -- helpers
-spec event_to_report(event()) -> #report{} | undefined.
event_to_report({error, _GL, Report}) ->
    handle_fmt_event(error, Report);
event_to_report({error_report, _GL, Report}) ->
    handle_report_event(error, Report);
event_to_report({warning_msg, _GL, Report}) ->
    handle_fmt_event(warning, Report);
event_to_report({warning_report, _GL, Report}) ->
    handle_report_event(warning, Report);
event_to_report({info_msg, _GL, Report}) ->
    handle_fmt_event(info, Report);
event_to_report({info_report, _GL, Report}) ->
    handle_report_event(info, Report);
event_to_report(_InternalEvent) ->
    undefined.

handle_fmt_event(Type, {Pid, Format, Args}) ->
    case (catch io_lib:format(Format, Args)) of
        {'EXIT', _} ->
            CrashMsg = unicode:characters_to_binary(io_lib:format("(io_lib:format/2 crash): ~p~n~p", [Format, Args])),
            #report{text = CrashMsg,
                    type = Type,
                    pid = process_pid(Pid),
                    process_name = process_name(Pid)};
        ReportText  ->
            #report{text = ReportText,
                    type = Type,
                    pid = process_pid(Pid),
                    process_name = process_name(Pid)}
    end.

handle_report_event(Type, {Pid, SubType, Report}) ->
    #report{text = report_text(Report),
            type = Type,
            subtype = subtype(SubType),
            pid = process_pid(Pid),
            process_name = process_name(Pid),
            data = Report}.

subtype(crash_report)      -> crash;
subtype(supervisor_report) -> supervisor;
subtype(progress)          -> progress;
subtype(_)                 -> undefined.

-spec process_pid(atom() | pid()) -> string().
process_pid(Name) when is_atom(Name) ->
    atom_to_list(Name);
process_pid(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

-spec process_name(atom() | pid()) -> string() | undefined.
process_name(Name) when is_atom(Name) ->
    atom_to_list(Name);
process_name(Pid) when is_pid(Pid) ->
    case process_info(Pid, registered_name) of
        []                        -> undefined;
        undefined                 -> undefined;
        {registered_name, Name}   -> atom_to_list(Name)
    end.

-spec nodename_to_host(node()) -> string().
nodename_to_host('nonode@nohost') ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
nodename_to_host(Node) ->
    atom_to_list(Node).

-spec report_text(term()) -> binary().
report_text(Data) ->
    try
        unicode:characters_to_binary(Data)
    catch
        error:badarg ->
            if
                is_list(Data) -> unicode:characters_to_binary(string:join(lists:map(fun pp_line/1, Data), "\n"));
                true          -> unicode:characters_to_binary(io_lib_pretty:print(Data))
            end
    end.

-spec pp_line(term()) -> iolist().
pp_line({Tag, Data}) ->
    io_lib:format("~p: ~p", [Tag, Data]);
pp_line(Term) ->
    io_lib_pretty:print(Term).
