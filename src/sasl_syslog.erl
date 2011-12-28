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

-module(sasl_syslog).
-export([open_socket/1, close_socket/1, send/4, msg_to_binary/1, msg_to_binary/2,
         facility_int/1, severity_int/1, report_severity/1,
         get_remote_port/0]).
-export([truncate/2]).
-export_type([facility/0, severity/0, timestamp/0]).
-export_type([udp_port_no/0, host/0, udp_socket/0]).

-include("sasl_syslog.hrl").

-type udp_port_no()  :: 0..65535.
-type host()         :: inet:ip_address() | inet:hostname().
-opaque udp_socket() :: port().

-type facility() :: 0..23
                  | kern | kernel | user | mail | daemon | auth | syslog
                  | lpr | news | uucp | cron | authpriv | ftp | ntp | audit | alert
                  | local0 | local1 | local2 | local3 | local4 | local5 | local6 | local7.
-type severity() :: 0..7
                  | emergency | emerg | alert | critical | crit | error | err | warning | notice | informational | info | debug.
-type timestamp() :: calendar:t_now() % {MegaSecs, Secs, MicroSecs}
                   | datetime().

%% WTF? why is this not exported from the calendar module?
-type datetime() :: {{non_neg_integer(), month(), day()}, {hour(), minute(), second()}}.
-type month()    :: 1..12.
-type day()      :: 1..31.
-type hour()     :: 0..23.
-type minute()   :: 0..59.
-type second()   :: 0..59.

-spec open_socket(udp_port_no()) -> {ok, udp_socket()} | {error, file:posix()}.
open_socket(LocalPort) ->
    gen_udp:open(LocalPort, [{active, false}, {reuseaddr, true}]).

-spec close_socket(udp_socket()) -> ok.
close_socket(Socket) ->
    gen_udp:close(Socket).

%% @deprecated
-spec send(udp_socket(), host(), udp_port_no(), #msg{}) -> any().
send(Socket, Address, Port, Msg) ->
    sasl_syslog_rfc5424:send_msg(Socket, Address, Port, Msg).

%% @deprecated
-spec msg_to_binary(#msg{}) -> binary().
msg_to_binary(Msg) ->
    sasl_syslog_rfc5424:msg_to_binary(Msg).

%% @deprecated
-spec msg_to_binary(#msg{}, pos_integer()) -> binary().
msg_to_binary(Msg, Size) ->
    sasl_syslog_rfc5424:msg_to_binary(Msg, Size).

-spec truncate(iolist() | unicode:unicode_binary(), pos_integer()) -> binary().
truncate(String, Length) when is_list(String) ->
    case unicode:characters_to_binary(String) of
        Text when is_binary(Text) ->
            truncate(Text, Length);
        _ ->
            error(badunicode)
    end;
truncate(String, Length) when is_binary(String) ->
    Size = byte_size(String),
    if
        Size >= Length -> <<(erlang:binary_part(String, 0, Length))/bytes, "...">>;
        true           -> String
    end.

-spec report_severity(#report{}) -> 'critical' | 'error' | 'informational' | 'notice' | 'warning'.
report_severity(#report{type = error, subtype = crash}) -> critical;
report_severity(#report{type = error}) -> error;
report_severity(#report{type = warning}) -> warning;
report_severity(#report{type = info, subtype = progress, data = [{application, _}, {started_at, _} | _]}) -> informational;
report_severity(#report{type = info, subtype = progress}) -> informational;
report_severity(#report{type = info, subtype = supervisor}) -> error;
report_severity(#report{type = info, data = [{application, _}, {exited, _} | _]}) -> error;
report_severity(#report{type = info}) -> notice.

-spec facility_int(facility()) -> pos_integer().
facility_int(I) when is_integer(I), I >= 0, I =< 23 -> I;
facility_int(kernel)   -> 0;
facility_int(kern)     -> 0;
facility_int(mail)     -> 2;
facility_int(daemon)   -> 3;
facility_int(auth)     -> 4;
facility_int(syslog)   -> 5;
facility_int(lpr)      -> 6;
facility_int(news)     -> 7;
facility_int(uucp)     -> 8;
facility_int(cron)     -> 9;
facility_int(authpriv) -> 10;
facility_int(ftp)      -> 11;
facility_int(ntp)      -> 12;
facility_int(logaudit) -> 13;
facility_int(logalert) -> 14;
facility_int(local0)   -> 16;
facility_int(local1)   -> 17;
facility_int(local2)   -> 18;
facility_int(local3)   -> 19;
facility_int(local4)   -> 20;
facility_int(local5)   -> 21;
facility_int(local6)   -> 22;
facility_int(local7)   -> 23;
facility_int(Fac)      -> error(badarg, [Fac]).

-spec severity_int(severity()) -> pos_integer().
severity_int(I) when is_integer(I), I >= 0, I =< 7 -> I;
severity_int(emergency)     -> 0;
severity_int(emerg)         -> 0;
severity_int(alert)         -> 1;
severity_int(critical)      -> 2;
severity_int(crit)          -> 2;
severity_int(error)         -> 3;
severity_int(err)           -> 3;
severity_int(warning)       -> 4;
severity_int(notice)        -> 5;
severity_int(informational) -> 6;
severity_int(info)          -> 6;
severity_int(debug)         -> 7;
severity_int(Sev)           -> error(badarg, [Sev]).

-spec get_remote_port() -> udp_port_no().
get_remote_port() ->
    case application:get_env(sasl_syslog, remote_port) of
        {ok, syslog} -> 514;
        {ok, gelf} -> 12201;
        {ok, auto} ->
            case application:get_env(sasl_syslog, formatter) of
                {ok, sasl_syslog_gelf} -> 12201;
                {ok, _} -> 514
            end;
        {ok, Int} when is_integer(Int) ->
            Int;
        {ok, Str} when is_list(Str) ->
            list_to_integer(Str)
    end.
