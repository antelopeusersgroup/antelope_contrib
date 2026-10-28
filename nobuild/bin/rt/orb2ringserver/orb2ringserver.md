# <p >orb2ringserver 
###  copy data from an orb to a ringserver as miniSEED</p>

1. [Name](#)
1. [Synopsis](#synopsis)
1. [Support](#support)
1. [Description](#description)
1. [Options](#options)
1. [Example](#example)
1. [Usage](#usage)
1. [Bugs And Caveats](#bugs-and-caveats)
1. [Author](#author)

## <a id='synopsis'>Synopsis</a>

<pre >
<b>orb2ringserver</b>  [-v]
    [-f <i>flush-latency</i>]
    [-F <i>flush-rate[@flush-duration</i>]]
    [-l <i>reclen</i>]
    [-R <i>reconnect-interval</i>]
    [-m <i>match</i>]
    [-r <i>reject</i>]
    [-s <i>pktid</i>]
    [-S <i>statefile[:pkts]</i>]
    <i>orbserver</i> <i>ringserver</i>
</pre>

## <a id='support'>Support</a>

<p >Contributed: NO BRTT support -- please contact author.</p>

## <a id='description'>Description</a>

<p ><b>orb2ringserver</b> collects selected time series packets from an orbserver(1), converts the data to 512-byte miniSEED records and transmits the records to a ringserver (via DataLink).  If the orb packet already contains a 512-byte miniSEED record it is forwarded as-is.  The ringserver program is commonly configured to serve the data via the SeedLink protocol.</p>

<p >Due to the differences between ORB packet lengths and miniSEED records, the program maintains a small internal buffer for each time series.  This allows the program to create full and efficient miniSEED records, but also introduces a slight delay or latency in the data transmission.  The amount of delay depends on the sample rate of the data and the compressability of the series, for most purposes it is inconsequential.  If the original packet from the ORB is already a 512-byte miniSEED record no delay is incurred.</p>

<p >The orbserver and ringserver are specified as <i>host</i>:<i>port</i>. Both host and port may be empty (blank), in which case <i>host</i> defaults to <b>localhost</b>, and port defaults to <b>6510</b> (for the orbserver) or <b>16000</b> (for the ringserver).  The orbserver port may be either an explicit number, or an alphanumeric name which is looked up in the file <i>$ANTELOPE/data/pf/orbserver_names.pf</i>.</p>

## <a id='options'>Options</a>

<b>-v</b>

<p style="padding-left: 30px;">Increase the verbosity, this flag may be specified multiple times.</p>

<b>-f latency</b>

<p style="padding-left: 30px;">Flush data from internal buffers when no new packets have arrived for <i>latency</i> seconds, default is 500 seconds.  The latency check is applied to each channel/stream individually.  This flushing mechanism ensures that data do not remain in the buffers when the channel has stopped flowing.  The mechanism can be disabled by specifying a value of 0.</p>

<b>-F rate[@duration]</b>

<p style="padding-left: 30px;">Perform a fast flush from internal buffers for data channels where <i>rate</i> (specified in samples per second) is >= data sampling rate. If a <i>duration</i> (in seconds) is specified the fast flush will occur when at least that duration of data has been accumulated.  This option will almost certainly result in the creation of unfilled data records, but allows controlling latency of data streams added by this program.  The mechanism is turned off by default (a value of zero).</p>

<b>-l reclen</b>

<p style="padding-left: 30px;">Specify the miniSEED record length to create, with a default of 512 bytes.  Other common options are 256 or 128 for reduced latency. Note that this option does not effect data that are already miniSEED records, which are forwarded directly.</p>

<b>-I [21I]</b>

<p style="padding-left: 30px;">Specify the miniSEED encoding to use for 32-bit integers.  A value of <i>2</i> means Steim-2 compression, <i>1</i> means Steim-1 compression, and <i>I</i> means uncompressed integers.  The default encoding for 32-bit integers is to first try Steim-2, then Steim-1 and finally use uncompressed.</p>

<b>-R interval</b>

<p style="padding-left: 30px;">If the connection to the destination ringserver is broken attempt to reconnect at <i>interval</i> in seconds, default is 60 seconds.  If the interval is 0 no reconnections are attempted and the program will exit if the connection is broken.</p>

<b>-m match</b>

<b>-r reject</b>

<p style="padding-left: 30px;">Copy only packets which <i>match</i> the regular expression <i>match</i> and do not <i>match</i> the regular expression <i>reject</i>.</p>

<b>-s pktid|time</b>

<p style="padding-left: 30px;">Start after the particular <i>pktid</i> or <i>time</i>.  Normally the input is positioned to the most recent packet on the orbserver or positioned based on a state file, and packets after that packet are received.  If a <i>pktid</i> or <i>time</i> is specified, and <i>no pre-existing state file is used for positioning</i>, the input is positioned to the first packet after the specified packet ID or time, and packets after that time are received.  The <i>time</i> may be specified in a wide variety of formats understood by str2epoch(3) or epoch(1).  If the format includes spaces, you must enclose the time <i>in</i> quotes.  If the option is all digits it is assumed to be a packet ID, otherwise a time.</p>

<b>-S statefile[:pkts]</b>

<p style="padding-left: 30px;">If the -S option is specified the time and <i>pktid</i> of the last packet received are saved to the <i>statefile</i> file whenever the program is terminated.  When the program is restarted, (if the -S option is specified), this file will be read and used to resume the connection such that there are no gaps.  If a <i>pkts</i> integer is specified, the <i>statefile</i> will be written every number of <i>pkts</i> processes to mitigate issues related to unexpected crashes.</p>

## <a id='example'>Example</a>

<pre >
%<b> orb2orb -m 'IU.*' -S state/orb2ringserver -v localhost:6510 localhost:16000</b>
orb2ringserver: orb2ringserver version: 1.0
orb2ringserver: Connected to orbserver at localhost:6510
orb2ringserver: Resuming ORB position from state file
orb2ringserver: Connected to ringserver at localhost:16000
 ...
</pre>

## <a id='usage'>Usage</a>

<p >This program generates 512-byte miniSEED records by default with the expectation that the destination ringserver can easily be configured to serve data via the SeedLink protocol.  The ringserver nor the DataLink protocol are limited to miniSEED or 512-bytes, but this is expected to be the most common use for this software.</p>

<pre >
The ringserver software is available from:
<b>https://github.com/iris-edu/ringserver</b>
</pre>

## <a id='bugs-and-caveats'>Bugs And Caveats</a>

<p >The internal buffering of time series data can add latency to the data streams.  The amount of latency added depends on the sample rate and compression of the time series.  For relatively high rate data (>20 sps) the amount of latency added is quite small for most purposes.</p>

<p >Normally when a termination signal is received the program will flush it's internal buffers and send all remaining data to the destination ringserver.  This process will likely create "unfilled" miniSEED records, this is a very minor issue in most cases.</p>

<p >If a termination signal is received while the connection to the destination ringserver is broken the probgram will exit without flusing it's internal buffer.  Additionally, the read position of the orbserver is saved as the last orb packet received, which is not necessarily related to the last data transmitted to the ringsever.  In short, this scenario will likely generate a gap in all time series.</p>

## <a id='author'>Author</a>

<pre >
Chad Trabant
IRIS Data Management Center
</pre>


(man page 2020-05-13)
