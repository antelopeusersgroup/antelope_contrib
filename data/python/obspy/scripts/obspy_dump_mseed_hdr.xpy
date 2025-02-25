import getopt

try:
    from obspy import read as op_read
except Exception as __:
    print(
        "cannot load obspy module. Maybe you need to install obspy (pip install obyps)?"
    )

from obspy.io.mseed.util import get_flags as op_get_flags

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog


def usage(progname):
    print(progname, "[-v] miniseed")


progname = sys.argv[0].split("/")[-1]
elog.init(progname)


verbose = False
opts = []
args = []
try:
    opts, args = getopt.getopt(sys.argv[1:], "f:vp:h:os:", "")
except getopt.GetoptError:
    usage(progname)
    elog.die("Illegal option")
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = True


if len(args) < 1 or len(args) > 1:
    usage(progname)
    sys.exit(1)

filename = args[0]
st = op_read(filename)
print(st[0].stats)
myflags = op_get_flags(filename)
for flag_name in myflags.keys():
    flags = myflags[flag_name]
    print("flag:", flag_name)
    if isinstance(flags, dict):
        for k, v in sorted(flags.items()):
            print("\t", k, v)
    else:
        print("\t", flags)
