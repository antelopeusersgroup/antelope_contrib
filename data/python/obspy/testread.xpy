import getopt
# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
# Import ObsPy modules
import obspy as obspy

"""
    This only serves to verify if obspy is working somehow
"""


msfile = "/opt/antelope/data/db/demo/wf/2016/013/201601316.w"

st = obspy.read(msfile)
print(st)    
