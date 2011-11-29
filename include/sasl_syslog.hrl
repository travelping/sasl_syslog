-record(msg, {
    vsn = 1   :: non_neg_integer(),
    facility  :: sasl_syslog:facility(),
    severity  :: sasl_syslog:severity(),
    timestamp :: sasl_syslog:timestamp(),
    hostname  :: unicode:chardata(),
    appname   :: unicode:chardata(),
    procid    :: unicode:chardata(),
    msgid     :: unicode:chardata(),
    msg       :: unicode:chardata()
}).

-record(report, {
    pid          :: string(),
    process_name :: string(),
    text         :: unicode:unicode_binary(),
    type         :: 'error' | 'info' | 'warning',
    subtype      :: 'progress' | 'supervisor' | 'crash',
    host         :: string(),
    node         :: string(),
    beam_pid     :: string(),
    timestamp    :: calendar:t_now(),
    data         :: term()
}).