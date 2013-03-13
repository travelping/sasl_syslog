%% @doc implements the message format from RFC 5424.
-module(sasl_syslog_rfc5424).
-export([msg_to_binary/1, msg_to_binary/2, send_msg/4, send_report/4]).

-include("sasl_syslog.hrl").

-spec send_report(sasl_syslog:udp_socket(), sasl_syslog:host(), sasl_syslog:udp_port_no(), #report{}) -> any().
send_report(Socket, Address, Port, R = #report{}) ->
    Severity = sasl_syslog:report_severity(R),
    Msg = #msg{appname = "beam",
               timestamp = R#report.timestamp,
               facility = get_facility(Severity),
               severity = Severity,
               hostname = R#report.host,
               procid = R#report.beam_pid,
               msgid = msgid_from_report(R),
               msg = R#report.text},
    send_msg(Socket, Address, Port, Msg).

-spec msgid_from_report(#report{}) -> string().
msgid_from_report(#report{process_name = undefined, pid = Pid}) -> Pid;
msgid_from_report(#report{process_name = Name}) -> Name.

-spec send_msg(sasl_syslog:udp_socket(), sasl_syslog:host(), sasl_syslog:udp_port_no(), #msg{}) -> any().
send_msg(Socket, Address, Port, MsgRec = #msg{}) ->
    lists:foreach(fun (MsgPart) ->
                          gen_udp:send(Socket, Address, Port, msg_to_binary(MsgPart))
                  end, maybe_split_msg(MsgRec)).

-spec maybe_split_msg(#msg{}) -> list(#msg{}).
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

-spec msg_to_binary(#msg{}) -> binary().
msg_to_binary(M) ->
    msg_to_binary(M, 2000).

-spec msg_to_binary(#msg{}, pos_integer()) -> binary().
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

    case application:get_env(sasl_syslog, rfc5424_bom) of
        {ok, true} ->
            BOM = unicode:encoding_to_bom(utf8);
        {ok, false} ->
            BOM = <<>>
    end,

    PacketL = [Pri, Version, SP, Tstamp, SP, Hostname,
               SP, Appname, SP, Procid, SP, Msgid,
               SP, "-", SP, BOM, Message],
    unicode:characters_to_binary(PacketL);
msg_to_binary(_Msg, _MsgLimit) ->
    error(syslog_msg_vsn_unsupported).

-spec get_facility(sasl_syslog:severity()) -> sasl_syslog:facility().
get_facility(Severity) when Severity =:= critical orelse Severity =:= error ->
    {ok, Facility} = application:get_env(sasl_syslog, error_facility),
    Facility;
get_facility(_Severity) ->
    {ok, Facility} = application:get_env(sasl_syslog, facility),
    Facility.

-spec calc_prival(sasl_syslog:facility(), sasl_syslog:severity()) -> pos_integer().
calc_prival(Facility, Severity) ->
    (sasl_syslog:facility_int(Facility) bsl 3) + sasl_syslog:severity_int(Severity).

-spec fmt_timestamp(sasl_syslog:timestamp() | undefined) -> iolist().
fmt_timestamp({{Year, Month, Day}, {Hours, Minutes, Seconds}}) ->
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.000000Z", [Year, Month, Day, Hours, Minutes, Seconds]);
fmt_timestamp({NowMegaSecs, NowSecs, NowMicroSecs}) ->
    {{Year, Month, Day}, {Hours, Minutes, Seconds}} = calendar:now_to_universal_time({NowMegaSecs, NowSecs, 0}),
    io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~6..0bZ", [Year, Month, Day, Hours, Minutes, Seconds, NowMicroSecs]);
fmt_timestamp(undefined) ->
    "-".

-spec str_or_nil(undefined | unicode:chardata(), pos_integer()) -> binary().
str_or_nil(undefined, _Limit) -> $-;
str_or_nil(String, Limit)     -> sasl_syslog:truncate(String, Limit).
