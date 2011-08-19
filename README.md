# sasl_syslog

This OTP application provides an error_logger report handler
that forwards messages to a syslog relay/collector using UDP.

The syslog messages it sends comply with RFC5424. This format
is supported by syslog-ng (since version 3.0) and rsyslogd.
Currently, there is no support for transports other than plain UDP.

It requires Erlang R14B03 or a later version.

## Handler Configuration
You can configure the syslog handler by setting parameters in it's OTP application environment.

    ================================================================================
    | parameter name | default     | desc                                          |
    ================================================================================
    |                |             |                                               |
    | enabled        | false       | boolean() that controls wheter the            |
    |                |             | syslog handler will be added on startup       |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|
    |                |             |                                               |
    | facility       | daemon      | the syslog facility that should be used       |
    |                |             | see src/sasl_syslog.erl for a list of         |
    |                |             | possible values (type facility())             |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|
    |                |             |                                               |
    | multiline      | false       | when set to true, the log message will        |
    |                |             | contain multiple lines. if false, each line   |
    |                |             | will be sent as a separate message (looks     |
    |                |             | better but causes more traffic).              |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|
    |                |             |                                               |
    | local_port     | 0           | UDP port the messages will be sent from.      |
    |                |             | 0 means OS assigned.                          |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|
    |                |             |                                               |
    | remote_host    | {127,0,0,1} | inet:hostname() | inet:ip_address()           |
    |                |             | host of the syslog receiver                   |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|
    |                |             |                                               |
    | remote_port    | 514         | port of the syslog receiver                   |
    |                |             |                                               |
    |----------------|-------------|-----------------------------------------------|

## Receiver Configuration Example (syslog-ng 3.2)

Use the [syslog() driver][1] must be used to receive RFC5424 compliant messages.
A source configuration that matches the sasl_syslog defaults:

    source src {
      syslog(ip(127.0.0.1) port(514) transport(udp));
    }

[1]: http://www.balabit.com/sites/default/files/documents/syslog-ng-ose-3.2-guides/syslog-ng-ose-v3.2-guide-admin-en.html/configuring_sources_syslog.html
