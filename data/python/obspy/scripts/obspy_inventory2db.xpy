import getopt

try:
    from obspy import read_inventory
except Exception as __:
    print(
        "cannot load obspy module. Maybe you need to install obspy: (pip install obspy) ?"
    )

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")

# import zamg.utilities as zu
# import zamg.missing as zm
import zamg.obspy2db as o2db


def usage(progname):
    print(progname, "[-vd] [-l level] [-f fmt] file [file2 [file3 ...]] db")
    print()
    print("\t-l network|station|channel|response (defaults to channel)")


progname = sys.argv[0].split("/")[-1]
elog.init(progname)
pfname = progname


inventoryformat = None  # None means automatic format detection
level = "response"  # default?
verbose = False
debug = False
opts = []
args = []
try:
    opts, args = getopt.getopt(sys.argv[1:], "df:l:op:v", "")
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
        inventoryformat = a
    elif o == "-l":
        level = a.lower()
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
params["verbose"] = verbose
params["debug"] = debug
params["level"] = level

dbname = args[-1]
db = ds.dbopen(dbname, "r+")

for ndx in range(len(args) - 1):
    filename = args[ndx]
    try:
        inventory = read_inventory(filename, format=inventoryformat, level=level)
    except Exception as __:
        print("problem reading inventory file %s" % filename)
        print("maybe the automatic detection of the format did not work ?")
        print("You can specify the format with the parameter -f")
        print(
            "possible formats are 'INVENTORYXML', 'RESP', 'SC3ML', 'SEED', 'STATIONTXT', 'STATIONXML' or 'XSEED'"
        )
        print("check the ObsPy documentation for details:")
        print(
            "https://docs.obspy.org/packages/autogen/obspy.core.inventory.inventory.read_inventory.html#obspy.core.inventory.inventory.read_inventory"
        )
        continue

    o2db.inventory2db(inventory, db, params)

sys.exit(0)
