# sasl_syslog

This OTP application provides an error_logger report handler
that forwards messages to a syslog relay/collector using UDP.

It implements both the RFC5424 syslog message format and GELF.
[RFC5424][1] is supported by syslog-ng (since version 3.0) and rsyslogd.
[GELF][2] is the format used by graylog2.

Currently, there is no support for transports other than plain UDP.

It requires Erlang R14B03 or a later version.

## Handler Configuration
You can configure the syslog handler by setting parameters in it's OTP application environment.

    =========================================================================================
    | parameter name | default              | desc                                          |
    =========================================================================================
    |                |                      |                                               |
    | enabled        | false                | boolean() that controls wheter the            |
    |                |                      | syslog handler will be added on startup       |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | facility       | daemon               | the syslog facility that should be used       |
    |                |                      | see src/sasl_syslog.erl for a list of         |
    |                |                      | possible values (type facility())             |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | error_facility | undefined            | the syslog facility that should receive       |
    |                |                      | reports of severity 'critical' and 'error'    |
    |                |                      | see src/sasl_syslog.erl for a list of         |
    |                |                      | possible values (type facility())             |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | formatter      | sasl_syslog_rfc5424  | The module used to encode messages.           |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | multiline      | false                | when set to true, the log message will        |
    |                |                      | contain multiple lines. if false, each line   |
    |                |                      | will be sent as a separate message (looks     |
    |                |                      | better but causes more traffic). this option  |
    |                |                      | only affects the RFC5424 formatter.           |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | rfc5424_bom    | false                | send a UTF-8 BOM in every message. RFC5424    |
    |                |                      | requires this, but it's confusing to          |
    |                |                      | many tools so we disabled it by default.      |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | local_port     | 0                    | UDP port the messages will be sent from.      |
    |                |                      | 0 means OS assigned.                          |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | remote_host    | {127,0,0,1}          | inet:hostname() | inet:ip_address()           |
    |                |                      | host of the syslog receiver                   |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|
    |                |                      |                                               |
    | remote_port    | auto                 | 'auto' | 'syslog' | 'gelf' | pos_integer()    |
    |                |                      | Port of the log receiver                      |
    |                |                      | The default value 'auto' selects a value      |
    |                |                      | port based on the formatter.                  |
    |                |                      |                                               |
    |----------------|----------------------|-----------------------------------------------|

## Receiver Configuration Example (syslog-ng 3.2)

The [syslog() driver][3] must be used to receive RFC5424 compliant messages.
A source configuration that matches the sasl_syslog defaults:

    source src {
      syslog(ip(127.0.0.1) port(514) transport(udp));
    }

[1]: http://tools.ietf.org/html/rfc5424
[2]: https://github.com/Graylog2/graylog2-docs/wiki/GELF
[3]: http://www.balabit.com/sites/default/files/documents/syslog-ng-ose-3.2-guides/syslog-ng-ose-v3.2-guide-admin-en.html/configuring_sources_syslog.html
