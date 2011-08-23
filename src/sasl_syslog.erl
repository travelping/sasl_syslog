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
         severity_int/1]).
-export_type([facility/0, severity/0]).

-include("sasl_syslog.hrl").

-type facility() :: 0..23
                  | kern | kernel | user | mail | daemon | auth | syslog
                  | lpr | news | uucp | cron | authpriv | ftp | ntp | audit | alert
                  | local0 | local1 | local2 | local3 | local4 | local5 | local6 | local7.
-type severity() :: 0..7
                  | emergency | emerg | alert | critical | crit | error | err | warning | notice | informational | info | debug.
-type timestamp() :: calendar:t_now() % {MegaSecs, Secs, MicroSecs}
                   | calendar:t_datetime().

open_socket(LocalPort) ->
    gen_udp:open(LocalPort, [{active, false}, {reuseaddr, true}]).

close_socket(Socket) ->
    gen_udp:close(Socket).

send(Socket, RemoteHost, RemotePort, Message) ->
    SyslogPacket = msg_to_binary(Message),
    gen_udp:send(Socket, RemoteHost, RemotePort, SyslogPacket).

%% implements format from RFC 5424
msg_to_binary(M) ->
    msg_to_binary(M, 2000).

msg_to_binary(M = #msg{vsn = 1}, MsgLimit) ->
    Version  = "1",
    Pri      = [$<, integer_to_list(calc_prival(M#msg.facility, M#msg.severity)), $>],
    Tstamp   = fmt_timestamp(M#msg.timestamp),
    Hostname = str_or_nil(M#msg.hostname, 255),
    Appname  = str_or_nil(M#msg.appname,  48),
    Procid   = str_or_nil(M#msg.procid,   128),
    Msgid    = str_or_nil(M#msg.msgid,    32),
    Message  = str_or_nil(M#msg.msg, MsgLimit),
    SP       = $\s,
    BOM      = unicode:encoding_to_bom(utf8),

    PacketL = [Pri, Version, SP, Tstamp, SP, Hostname,
               SP, Appname, SP, Procid, SP, Msgid,
               SP, "-", SP, BOM, Message],
    unicode:characters_to_binary(PacketL);
msg_to_binary(_Msg, _MsgLimit) ->
    error(syslog_msg_vsn_unsupported).

calc_prival(Facility, Severity) ->
    (facility_int(Facility) bsl 3) + severity_int(Severity).

facility_int(I) when is_integer(I), I >= 0, I =< 23 -> I;
facility_int(A) ->
    case A of
        kernel   -> 0;
        kern     -> 0;
        user     -> 1;
        mail     -> 2;
        daemon   -> 3;
        auth     -> 4;
        syslog   -> 5;
        lpr      -> 6;
        news     -> 7;
        uucp     -> 8;
        cron     -> 9;
        authpriv -> 10;
        ftp      -> 11;
        ntp      -> 12;
        logaudit -> 13;
        logalert -> 14;
        local0   -> 16;
        local1   -> 17;
        local2   -> 18;
        local3   -> 19;
        local4   -> 20;
        local5   -> 21;
        local6   -> 22;
        local7   -> 23;
        _        -> error(badarg)
    end.

severity_int(I) when is_integer(I), I >= 0, I =< 7 -> I;
severity_int(emergency)                            -> 0;
severity_int(emerg)                                -> 0;
severity_int(alert)                                -> 1;
severity_int(critical)                             -> 2;
severity_int(crit)                                 -> 2;
severity_int(error)                                -> 3;
severity_int(err)                                  -> 3;
severity_int(warning)                              -> 4;
severity_int(notice)                               -> 5;
severity_int(informational)                        -> 6;
severity_int(info)                                 -> 6;
severity_int(debug)                                -> 7.

fmt_timestamp({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0bZ", [Year, Month, Day, Hours, Minutes, Seconds]);
fmt_timestamp({NowMegaSecs, NowSecs, NowMicroSecs}) ->
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:now_to_universal_time({NowMegaSecs, NowSecs, 0}),
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~6..0bZ", [Year, Month, Day, Hours, Minutes, Seconds, NowMicroSecs]);
fmt_timestamp(undefined) ->
    "-".

str_or_nil(undefined, _Limit) -> $-;
str_or_nil(String, Limit)     -> truncate(String, Limit).

truncate(String, Length) when is_list(String) ->
    truncate(unicode:characters_to_binary(String), Length);
truncate(String, Length) when is_binary(String) ->
    Size = byte_size(String),
    if
        Size >= Length -> erlang:binary_part(String, 0, Length);
        true           -> String
    end.
