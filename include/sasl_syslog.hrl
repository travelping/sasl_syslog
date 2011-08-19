-record(msg, {
    vsn = 1   :: non_neg_integer(),
    facility  :: sasl_syslog:facility(),
    severity  :: sasl_syslog:severity(),
    timestamp :: sasl_syslog:timestamp(),
    hostname  :: iolist(),
    appname   :: iolist(),
    procid    :: iolist(),
    msgid     :: iolist(),
    msg       :: iolist()
}).
