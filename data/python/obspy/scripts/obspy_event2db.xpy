import getopt

try:
    from obspy import read_events
except Exception as __:
    print(
        "cannot load obspy module. Maybe you need to install obspy :(pip install obspy)?"
    )

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")

# import zamg.utilities as zu
# import zamg.missing as zm
import zamg.obspy2db as o2d


def usage(progname):
    print(progname, "[-vd] file [file2 [file3 ...]] db")


progname = sys.argv[0].split("/")[-1]
elog.init(progname)
pfname = progname


catalogformat = None  # None means automatic format detection
verbose = False
debug = False
opts = []
args = []
try:
    opts, args = getopt.getopt(sys.argv[1:], "df:op:v", "")
except getopt.GetoptError:
    usage(progname)
    elog.die("Illegal option")
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = True
    elif o == "-d":
        debug = True
    elif o == "-f":
        catalogformat = a
    elif o == "-o":
        overwrite = True
    elif o == "-p":
        pfname = a


if len(args) < 2:
    usage(progname)
    sys.exit(1)

pf = stock.pfread(pfname)
sta_trans = pf["sta_trans"]
chan_trans = pf["chan_trans"]
params = {}
params["sta_trans"] = sta_trans
params["chan_trans"] = chan_trans

dbname = args[-1]
db = ds.dbopen(dbname, "r+")

for ndx in range(len(args) - 1):
    filename = args[ndx]
    try:
        cat = read_events(filename, format=catalogformat )
    except Exception as e:
        print("problem reading event file %s" % filename)
        print("maybe the automatic detection of the format did not work?")
        print("you can specify the format with the parameter -f")
        print(
            "possible formats are 'NORDIC', 'QUAKEML', 'IMS10BULLETIN' (aka isf1.0) and so on"
        )
        print("check the ObsPy documentation for details:")
        print(
            "https://docs.obspy.org/packages/autogen/obspy.core.event.catalog.read_events.html"
        )
        continue
    o2d.catalog2db(cat, db, params)

sys.exit(0)
