""" Open URL from database entry
supposed to work from commandline, as a plugin to dbloc2 and dbe_dep

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     June 11, 2020
@modified    April 16, 2022
@version     2.2
@license     MIT-style license

"""
import getopt
import io
import math
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import webbrowser
import urllib.parse as up


def deg_to_dms(deg, type="lat"):
    decimals, number = math.modf(deg)
    d = int(number)
    m = int(decimals * 60)
    s = (deg - d - m / 60) * 3600.00
    compass = {"lat": ("N", "S"), "lon": ("E", "W")}
    compass_str = compass[type][0 if d >= 0 else 1]
    return "{}º{}'{:.2f}\"{}".format(abs(d), abs(m), abs(s), compass_str)


def usage(progname):
    print(progname, "[-v] [-p pfname] -t template database[.table] [recno]")


def main():

    progname = sys.argv[0].split("/")[-1]
    elog.init()

    verbose = False
    action = ""
    recno = -1
    dboutname = ""
    template_name = "default"
    pfname = progname

    try:
        opts, args = getopt.getopt(sys.argv[1:], "hp:t:v", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        sys.exit(1)

    for option, argument in opts:
        if option == "-v":
            verbose = True
        elif option == "-h":
            usage(progname)
            sys.exit(0)
        elif option == "-p":
            pfname = argument
        elif option == "-t":
            template_name = argument
        else:
            print("unknown option %s" % option)
            usage(progname)
            sys.exit(1)

    if len(args) < 1 or len(args) > 2:
        usage(progname)
        sys.exit(1)

    if len(args) > 1:
        recno = int(args[1])

    dbname = args[0]
    my_db = ds.dbopen_database(dbname, "r")

    if recno > -1:
        my_db.record = recno
    else:
        my_db.record = 0

    try:
        [my_lat, my_lon] = my_db.getv("lat", "lon")
    except Error as __:
        elog.die("problem reading coordinates from database")

    try:
        import antelope.sysinfo as sysinfo
    except ImportError:
        my_os = sys.platform
    else:
        __, my_os = sysinfo.my_os()

    pf_dict = stock.pfread(pfname)

    my_browser = pf_dict["browser"][my_os.lower()]
    default_template = pf_dict["default"]
    if template_name == "default":
        template_name = default_template
    url_template = pf_dict["templates"][template_name]
    s1 = deg_to_dms(my_lat, type="lat")
    s2 = deg_to_dms(my_lon, type="lon")
    qs = "%s %s" % (s1, s2)
    my_latlon = up.quote_plus(qs)
    # print(s1," ",s2, " ",es)
    # sys.exit(1)

    my_url = url_template.format(lat=my_lat, lon=my_lon, latlon=my_latlon)

    mybrowser = webbrowser.get(my_browser)
    mybrowser.open(my_url, new=2, autoraise=True)


if __name__ == "__main__":
    status = main()
    sys.exit(status)
