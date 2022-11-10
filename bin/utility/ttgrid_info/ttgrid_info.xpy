
import getopt
from antelope.buvector import *
from antelope import _ttgrid


def usage(progname):
    print("usage: %s [-g] [-N] filename [gridname]" % progname)

progname = sys.argv[0].split("/")[-1]
pfname = progname
    
showstations = True
showgridpoints=False

verbose = 0
opts = []
args = []
try:
    opts, args = getopt.getopt(sys.argv[1:], "vNg", "")
except getopt.GetoptError:
    print("illegal option")
    usage(progname)
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = 1
    elif o == "-N":
        showstations = False
    elif o == "-g":
        showgridpoints = True    

if len(args) > 2 or len(args) < 1:
    usage(progname)
    sys.exit(1)

filename = args[0]
ttgrids = _ttgrid._ttgrid_mapfile(filename)
if ttgrids is None:
    print("displayttgrid: ttgrid_mapfile error")
    sys.exit(1)

if len(args) > 1:
    gridname = args[1]
else:
    print(' '.join(ttgrids))
    sys.exit(0)

found= False
for grid in ttgrids:
    if gridname == grid:
        found = True
        break

if not found:
    print("displayttgrid: Cannot find grid %s in %s" % (gridname, filename))
    sys.exit(1)

if showstations:
    stations = _ttgrid._ttgrid_get(gridname, 'stations')
    if stations is None:
        print("displayttgrid: ttgrid_get (%s,'stations') error" % gridname)
        sys.exit(1)

    (rc, npts, ny, x, y, label) = buvector_get_points (stations, -1, True)    
    for i in range(npts):
        print("%s %.2f %.2f" % (label[i], x[i], y[0][i]))

if showgridpoints:
    gridpoints = _ttgrid._ttgrid_get(gridname, 'sources')
    if gridpoints is None:
        print("displayttgrid: ttgrid_get (%s,'sources') error" % gridname)
        sys.exit(1)

    (rc, npts, ny, x, y ) = buvector_get_points (gridpoints, -1, False)    
    for i in range(npts):
        print("%.2f %.2f" % (x[i], y[0][i]))
