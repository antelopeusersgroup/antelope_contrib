#
# This is eworm2orb's parameter file

#  Basic Earthworm setup:
#
MyModuleId         MOD_EWORM2ORB  # module id for this instance of eworm2orb 
RingName           TRACE_RING   # shared memory ring for input/output
LogFile            1           # 0 to completely turn off disk log file
HeartBeatInterval  15          # seconds between heartbeats

# List the message logos to grab from transport ring
#              Installation       Module          Message Types
GetTracesFrom  INST_WILDCARD    MOD_WILDCARD    TYPE_TRACEBUF
#  orb server IP[:port]
Orbname        nordic.giseis.alaska.edu

Compress       1               # Whether to compress the data with Harvey's first dif/generic compression
Network        IU              # Network of stations to export (optional)
Timecorr	0		# Whether to correct timestamps per SITE_DB
