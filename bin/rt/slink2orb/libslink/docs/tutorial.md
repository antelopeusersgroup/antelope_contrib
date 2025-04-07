# Tutorial for libslink

[TOC]

## SeedLink concepts

The general concept of collecting data from a SeedLink server is
to connect, submit one or more data selections, and receive streaming
data from the server continuously or until a configured limit.

The selection of data is separated into two parts:

1. A station ID identifies the highest level of selection.
2. One or more stream IDs per station to limit individual data streams
   from the station.

See the [description in the SeedLink v4 specification](https://docs.fdsn.org/projects/seedlink/en/latest/protocol.html#station-id-and-stream-id).

For miniSEED payloads, the station ID should be a combination of network
and station codes: `NET_STA`, and a stream ID is a combination
of location and channel codes: `LOC_B_S_SS`.  These codes are the same,
and simply different components of the
[FDSN Source Identifier specification](https://docs.fdsn.org/projects/source-identifiers).

### Wildcards

For stream IDs, globbing-style wildcards (`*` and `?`) are supported by
all versions of the SeedLink protocol and most known server implementations.

For station IDs, the same globbing is supported by some server
implementations for protocol versions < 4.  For protocol v4 wildcards
for station IDs are part of the specification and expected to be
supported.

## SeedLink Connection Description

All details of connection to a SeedLink server are contained in a
SeedLink Connection Description (SLCD) structure.

Programs are expected to initialize a SLCD using sl_initslcd(),
then configure connection details using setting and configuration
functions, followed by data collection with sl_collect().

Following  initializing a SLCD, sl_set_serveraddress() must be
called to set the address of the target server.  After which
a program would call other configuration functions and finally call
sl_collect() in a loop to collect data.

See @ref md_examples

### Configuration functions

The following functions are available for configuring a connection:

* sl_set_clientname() - Set the client program name and version
* sl_set_serveraddress() - Set the server address (host:port)
* sl_set_auth_params() - Set authentication callbacks, SL v4 only
* sl_add_stream() - Add data selections from a string
* sl_add_streamlist() - Add data selections from a string
* sl_add_streamlist_file() - Add data selections from a file
* sl_set_allstation_params() - Configure parameters for all-station mode
* sl_request_info() - Set INFO level to be requested
* sl_recoverstate() - Set the stream state from a file
* sl_set_timewindow() - Set global time range for selected streams
* sl_set_keepalive() - Set keep alive interval for idle connections
* sl_set_iotimeout() - Set socket-level I/O timeout
* sl_set_idletimeout() - Set idle connection timeout
* sl_set_reconnectdelay() - Set delay when reconnecting
* sl_set_blockingmode() - Enable non-blocking or blocking mode
* sl_set_dialupmode() - Enable "dial-up" mode
* sl_set_batchmode() - Enable batch mode, SL v3 only

These functions are used to configure a SLCD.

## Using sl_collect()

Following initialization and configuration, a program must call
sl_collect() to collect data.  This function fully manages the
connection, performing the following:
- (re)connecting when needed
- negotiating and configuring selected data streams on connection
- sending keepalives at itervals (if configured)
- disconnecting idle connections after timeout (if configured)
- sending `INFO` requests (if configured)
- tracking the state of each stream received
- terminating the connection when triggered

Typically, sl_collect() is run in a loop and returns when a
packet is received, like this:

```C
while ((status = sl_collect (slconn, &packetinfo,
                             plbuffer, plbuffersize)) != SLTERMINATE)
{
   ... Do something with the packet here
}
```

The \p plbuffer is a caller-provided buffer for data payload.  The
station ID, payload size, format, subformat, and SeedLink sequence
number of returned in \p packetinfo; seed SLpacketinfo.

The sl_collect() routine returns one of @ref collect-status.

### Non-blocking connections

By default, sl_collect() will not return until a packet is available
or the connection is terminated.

In some scenarios it is necessary for a program to do other tasks
while waiting for packets to arrive.  A connection can be configured
in non-blocking mode using sl_set_blockingmode().

When non-blocking mode is enabled, sl_collect() will quickly return
`SLNOPACKET` when no data is available.  It is then a task for the caller to
throttle any loops that call sl_collect() as required.

## Closing connections

It is usually desirable to cleanly shutdown a client. In particular
to allow processing of data already received in internal buffers and
to save the connection state.

To accomplish this sl_terminate() should be called, which will cause
sl_collect() to cleanly finish processing data and return `SLTERMINATE`.
Afterwhich, the program can do whatever it wishes to finish before
exiting such as saving a statefile.

A common approach is to set signal handlers for `SIGINT`, `SIGTERM`, etc.
and run sl_terminate() from the handler.  The library includes
sl_set_termination_handler(), which will do exactly this (but is not
thread safe).  Alternatively, some other mechanism can be employed
to run sl_terminate() when shutdown is needed.

## Authentication

SeedLink protocol verision 4 allows the client to submit credentials
for authentication.  The primary use for this mechanism is to control
access to restricted data streams.

@note It is strong recommended to submit credentials only over
TLS encrypted connections, as they are otherwise plain text.

The library supports submitting credentials by providing
function callback hooks, specifically:

- \a auth_value(), callback to provide an auth value
- \a auth_finish(), callback to perform cleanup
- \a auth_data, a caller-supplied pointer passed to the functions

These values are set with sl_set_auth_params().

The \a auth_value() function has the following signature:
```
const char * auth_value(const char *server, void *auth_data)
```

When this function is set, the `AUTH` command will be
sent during connection negotiation with the value returned by the
function.  The \p server value will be the SeedLink server address
and the \p auth_data will be the pointer set by the caller.

The \a auth_finish() function has the following signature:
```
void auth_finish(const char *server, void *auth_data)
```

When this function is set, it will be called after
the authentication value has been submitted.  This can be used
to clean up memory, close files, etc.  The arguments are the same
as for \a auth_value().

## Time window requests and dial-up mode

Most SeedLink connections are intended to continue streaming data
continuously.  There are two alterantives in the protocol as
follows:

1) Time window requests allow a client to specify a start time and,
   optionally, an end time, which is applied to all selected data
   streams.  This feature is not required for server implementaion,
   and may not be supported.

2) When dial-up mode is enabled the server will close the conection
   when all of the selected data available to the server has been
   sent.  This is useful in scenarios where a permanent connection
   is not possible and you wish to minimize the connection duration.

## References

Finalized in 2024, the SeedLink v4 protocol is standardized by the
[International FDSN](https://www.fdsn.org) with the specification
published here:

https://docs.fdsn.org/projects/seedlink

The previous version of the protocol, referred to in this software as
version 3.X, is documented in the [SeisComP](https://www.seiscomp.de)
sofware package here:

https://www.seiscomp.de/doc/apps/seedlink.html
