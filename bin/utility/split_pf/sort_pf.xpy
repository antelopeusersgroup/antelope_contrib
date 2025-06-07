"""
@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Jun 14, 2022
@version     1.1
@license     MIT-style license

"""

from operator import itemgetter

# Import Antelope modules
import antelope.stock as stock
import antelope.elog as elog
import antelope.datascope as ds
import getopt


def usage(progname):
    print(progname, "[-v] [-s expr] [-r] pfname dbmaster")


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)

    verbose = False
    opts = []
    args = []
    mode = "orbrtd"
    reverse_sort = False
    sort_expression = "sta"  # alphabetical
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vrs:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-s":
            sort_expression = a
        elif o == "-r":
            reverse_sort = True
        elif o == "-h":
            usage(progname)
            sys.exit(0)

    if len(args) > 2 or len(args) < 2:
        usage(progname)
        sys.exit(1)

    pfname = args[0]
    dbname = args[1]

    etime = stock.now()
    db = ds.dbopen(dbname, "r")
    dbsite = db.lookup(table="site")
    dbsite = dbsite.subset("offdate == NULL || offdate > %f" % etime)
    dbsite = dbsite.sort(sort_expression)
    pfitems = []
    pf = stock.pfread(pfname)
    if pf.has_key("sources"):
        mode = "sources"
        nsc_list = pf["sources"]
    elif pf.has_key("traces"):
        mode = "traces"
        nsc_list = pf["traces"]
    else:
        elog.die("input parameterfile must be either for orbrtd or orbmonrtd_dep")

    for myline in nsc_list:
        try:
            nsc, __ = myline.split(None, maxsplit=1)
        except ValueError as ve:
            nsc = myline
        if not "_" in nsc:
            continue
        net, sta, __ = nsc.split("_", maxsplit=2)
        try:
            order = dbsite.find("sta == '%s'" % sta, first=-1)
        except Exception as __:
            order = -1
            elog.notify(
                "station %s not found, we assume it comes very early in the sorting"
            )
        pfitems.append(dict(sta=sta, order=order, pfstuff=myline))

    slist = sorted(pfitems, key=itemgetter("order"), reverse=reverse_sort)
    print("%s &Tbl{" % mode)  # start a sorted list
    for myitem in slist:
        print("\t", myitem["pfstuff"])
    print("}")  # end the sorted list


if __name__ == "__main__":
    status = main()
    sys.exit(status)
