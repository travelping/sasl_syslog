-module(sasl_syslog_gelf).
-export([send_msg/4, send_report/4]).

-include("sasl_syslog.hrl").

-define(SHORTMSG_MAXLEN, 180).
-define(int_to_bin(X), list_to_binary(integer_to_list(X))).

-spec send_msg(sasl_syslog:udp_socket(), sasl_syslog:host(), sasl_syslog:udp_port_no(), #msg{}) -> any().
send_msg(Socket, Address, Port, Msg) ->
    gen_udp:send(Socket, Address, Port, zlib:gzip(msg_to_binary(Msg))).

-spec send_report(sasl_syslog:udp_socket(), sasl_syslog:host(), sasl_syslog:udp_port_no(), #report{}) -> any().
send_report(Socket, Address, Port, Report) ->
    gen_udp:send(Socket, Address, Port, zlib:gzip(report_to_binary(Report))).

-spec msg_to_binary(#msg{}) -> binary().
msg_to_binary(Msg) ->
    <<"{\"version\":\"1.0\",",
      "\"host\":", (enc_string(Msg#msg.hostname))/bytes, ",",
      "\"short_message\":", (enc_string(Msg#msg.msg))/bytes, ",",
      "\"level\":", (?int_to_bin(sasl_syslog:severity_int(Msg#msg.severity)))/bytes, ",",
      (maybe_empty(<<"facility">>, enc_syslog_facility(Msg#msg.facility)))/bytes,
      (maybe_empty(<<"_appname">>, Msg#msg.appname))/bytes,
      (maybe_empty(<<"_procid">>, Msg#msg.procid))/bytes,
      (maybe_empty(<<"_msgid">>, Msg#msg.msgid))/bytes,
      "\"timestamp\":", (enc_timestamp(unix_microtime(Msg#msg.timestamp)))/bytes,
      "}">>.

maybe_empty(_Prefix, undefined) -> <<>>;
maybe_empty(Prefix, StrData)    -> <<$\", Prefix/bytes, "\":", (enc_string(StrData))/bytes, ",">>.

-spec report_to_binary(#report{}) -> binary().
report_to_binary(Report) ->
    <<"{\"version\":\"1.0\",",
      "\"host\":", (enc_string(Report#report.host))/bytes, ",",
      "\"short_message\":", (enc_string(report_short_message(Report)))/bytes, ",",
      "\"full_message\":", (enc_string(Report#report.text))/bytes, ",",
      "\"facility\":", (enc_string(report_facility(Report)))/bytes, ",",
      "\"level\":", (?int_to_bin(sasl_syslog:severity_int(sasl_syslog:report_severity(Report))))/bytes, ",",
      "\"_node\":", (enc_string(Report#report.node))/bytes, ",",
      "\"_process\":", (enc_string(Report#report.pid))/bytes, ",",
      "\"_vm_os_pid\":", (enc_string(Report#report.beam_pid))/bytes, ",",
      "\"timestamp\":", (enc_timestamp(unix_microtime(Report#report.timestamp)))/binary,
      "}">>.

-spec report_facility(#report{}) -> iolist().
report_facility(#report{process_name = undefined, pid = Pid}) -> Pid;
report_facility(#report{process_name = Name}) -> Name.

-spec report_short_message(#report{}) -> binary().
report_short_message(#report{subtype = progress, text = Text}) ->
    <<"(PROGRESS) ", (sasl_syslog:truncate(Text, ?SHORTMSG_MAXLEN - 11))/bytes>>;
report_short_message(#report{subtype = supervisor, text = Text}) ->
    <<"(SUPERVISOR) ", (sasl_syslog:truncate(Text, ?SHORTMSG_MAXLEN - 13))/bytes>>;
report_short_message(#report{subtype = crash, text = Text}) ->
    <<"(CRASH) " , (sasl_syslog:truncate(Text, ?SHORTMSG_MAXLEN - 8))/bytes>>;
report_short_message(#report{text = Text}) ->
    sasl_syslog:truncate(Text, ?SHORTMSG_MAXLEN).

-spec unix_microtime(sasl_syslog:timestamp()) -> integer().
unix_microtime({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000000000) + (Secs * 1000000) + MicroSecs;
unix_microtime(Datetime = {{_, _, _}, {_, _, _}}) ->
    EpochSeconds = 62167219200,
    Seconds = calendar:datetime_to_gregorian_seconds(Datetime),
    (Seconds - EpochSeconds) * 1000000.

-spec enc_timestamp(integer()) -> binary().
enc_timestamp(TimeStamp) ->
    list_to_binary(io_lib:format("~.6f", [TimeStamp / 1000000])).
    
-spec enc_syslog_facility(undefined | sasl_syslog:facility()) -> iolist().
enc_syslog_facility(undefined) ->
    undefined;
enc_syslog_facility(Int) when is_integer(Int) ->
    integer_to_list(Int);
enc_syslog_facility(Atm) when is_atom(Atm) ->
    Atm.

-spec enc_string(binary() | iolist() | atom()) -> binary().
enc_string(Bin) when is_binary(Bin) ->
    <<$", (escape(Bin))/binary, $">>;
enc_string(Lis) when is_list(Lis) ->
    <<$", (escape(iolist_to_binary(Lis)))/binary, $">>;
enc_string(Atm) when is_atom(Atm) ->
    <<$", (escape(atom_to_binary(Atm, utf8)))/binary, $">>.

-compile({inline, escape/1}).
escape(Bin) ->
    << <<(esc_chr(Chr))/binary>> || <<Chr/utf8>> <= Bin >>.

-compile({inline, esc_chr/1}).
esc_chr($")  -> <<"\\\"">>;
esc_chr($\\) -> <<"\\">>;
esc_chr($\n) -> <<"\\n">>;
esc_chr($\r) -> <<"\\r">>;
esc_chr($\t) -> <<"\\t">>;
esc_chr($\b) -> <<"\\b">>;
esc_chr($\f) -> <<"\\f">>;
esc_chr(Chr) when Chr > 31 ->
    <<Chr/utf8>>;
esc_chr(Chr) ->
    <<"\\u", (pad4(list_to_binary(integer_to_list(Chr, 16))))/binary>>.

-compile({inline, pad4/1}).
pad4(<<X>>)       -> <<"000", X>>;
pad4(<<X, Y>>)    -> <<"00", X, Y>>;
pad4(<<X, Y, Z>>) -> <<"0", X, Y, Z>>;
pad4(Bin)         -> Bin.
