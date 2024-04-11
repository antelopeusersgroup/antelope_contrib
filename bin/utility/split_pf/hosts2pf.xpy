"""
@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Jun 14, 2022
@version     1.0
@license     MIT-style license

"""

# Import Antelope modules

import antelope.stock as stock
import antelope.elog as elog
import getopt


def usage(progname):
    print(progname, "[-v] hostfile pfname")


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)

    verbose = False
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vs:f:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-h":
            usage(progname)
            sys.exit(0)

    if len(args) > 2 or len(args) < 2:
        usage(progname)
        sys.exit(1)

    hostfile = args[0]
    pfname = args[1]

    pf = stock.ParameterFile()
    stations = {}
    lines = [line.strip() for line in open(hostfile)]
    for line in lines:
        if line.startswith("#"):
            next
        info = line.split()
        if len(info) < 2:  # need at least ip and name
            next
        ip = info[0]
        name = info[1]
        if name.endswith("-backup"):
            sta = name.replace("-backup", "")
            if sta not in stations:
                stations[sta] = {}
            stations[sta]["backup"] = ip
        else:
            sta = name
            if sta not in stations:
                stations[sta] = {}
            stations[sta]["primary"] = ip
    pf.update(dict(stations=stations))
    pf.pfwrite(pfname)


if __name__ == "__main__":
    status = main()
    sys.exit(status)
