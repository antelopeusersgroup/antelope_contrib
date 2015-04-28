"""
Read a data file from Dreger's code
and index the data in a wfdisc table.

Juan Reyes
"""

import sys
import antelope.datascope as datascope

list_of_files = sys.argv

if not len(list_of_files) == 4: 
    sys.exit( '\nusage: %s station_name data_file database\n' % list_of_files[0] ) 

# Get Arguments
station = list_of_files[1]
data_file = list_of_files[2]
database = list_of_files[3]

print "Add station %s" % station
print "Add file %s" % data_file
print "Add to database %s" % database

seg_type = 'D'  # D = displacement
                # V = velocity
                # A = acceleration

data_type = 'as' # as = free-format ascii

db = datascope.dbopen(database, 'r+')
wfdisc = db.lookup(table='wfdisc')

data_file = os.path.relpath( data_file )

print "\nReading file %s" % data_file

filename = os.path.basename( data_file )
directory = os.path.dirname( data_file )

fo = open(data_file, "r")
print "open: ", fo.name

# Number of channels
try:
    total_chans = int( fo.readline().strip() )
    print "\tTotal channels [%s]" % total_chans
except:
    sys.exit( "\tNOT VALID FILE [%s]" % data_file )

if total_chans == 3:
    chan_names = ["T", "R", "Z"]
elif total_chans == 8:
    chan_names = ["TSS","TDS","XSS","XDS","XDD","ZSS","ZDS","ZDD"]
else:
    sys.exit("More channels than names configured [%s]" % chan_names)

# Data Format
try:
    data_format = fo.readline().strip()
except:
    sys.exit( "\tNOT VALID FILE [%s]" % data_file )



for chan in range(total_chans):
    try:
        header1 = fo.readline().strip().split()
        header2 = fo.readline().strip().split()
    except:
        sys.exit( "\tNOT VALID FILE [%s]" % data_file )

    try:
        # Total points
        total_points = int( header2[0] )
        print "\t\tTotal points [%s]" % total_points

        # Samplerate
        samplerate =  float( header2[1] )
        print "\t\tSampelrate [%s]" % samplerate
    except:
        sys.exit( "\tNOT VALID FILE [%s]" % data_file )

    if not total_points or not samplerate:
        sys.exit( "\tNOT VALID FILE [%s]" % data_file )

    # Read Data
    cache = []
    start_byte = fo.tell() + 1
    endtime = total_points * samplerate

    keys = ('sta', 'chan', 'time', 'endtime', 'nsamp', 'samprate',
            'segtype', 'datatype', 'dir', 'dfile', 'foff')
    vals = (station, chan_names[chan], 0, endtime, total_points, samplerate,
            seg_type, data_type, directory, filename, start_byte )

    keyvals = zip(keys, vals)
    print "\t\t\tAdding entry to wfdisc [%s] " % wfdisc.addv(*keyvals)


    while (total_points > 0):
        row = fo.readline().strip()
        try:
            data_row = row.split()
            print [  float(s) for s in data_row ]
            cache.extend( [  float(s) for s in data_row ] )
            total_points -= len( data_row )
        except:
            print [ row ]
            total_points -= 6

db.close()
