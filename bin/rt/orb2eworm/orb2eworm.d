#
# This is orb2eworm's parameter file

#  Basic Earthworm setup:
#
MyModuleId         MOD_ORB2EWORM  # module id for this instance of orb2eworm 
RingName           TRACE_RING   # shared memory ring for input/output
LogFile            1           # 0 to completely turn off disk log file
HeartBeatInterval  15          # seconds between heartbeats

OrbName        nordic      # Name of orbserver to which to connect
ChannelSelect  ""          # Regular-expression match for data streams to import
ChannelReject  ""          # Regular-expression match for data streams not to import
