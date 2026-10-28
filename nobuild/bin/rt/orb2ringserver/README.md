
# orb2ringserver

Send data from an [Antelope](http://brtt.com/) ORB to a ringserver
as miniSEED records.  The usual intent is to run the
[ringserver](https://github.com/iris-edu/ringserver) 
as a SeedLink server.

## Details

This program creates miniSEED data records from selected time
series packets in an Antelope ORB and transmits then to a ringserver
via the DataLink protocol.  The ringserver program is then commonly
configured to serve the data via the SeedLink protocol.

Due to the differences between ORB packet lengths and miniSEED
records, the program maintains a small internal buffer for each time
series.  This allows the program to create full and efficient miniSEED
records, but also introduces a slight delay or latency in the data
transmission.  The amount of delay depends on the sample rate of the
data and the compressability of the series, for most purposes it is
inconsequential.  There are options to pre-maturely flush the data
buffers, before they can be guaranteed to be full, in order to
reduce the latency added by this program.

By default, if the original packet from the ORB is already a
512, 256 or 128-byte miniSEED record it is not repackaged and no
delay is incurred.

See the [orb2ringserver man page](orb2ringserver.md) for further information.
