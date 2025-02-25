"""

SEISAN STATIONS to sitetable

@author      Nikolaus Horn
@created     April 6, 2023
@version     1.0

"""

import getopt

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock


def usage():
    print(sys.argv[0], "[-v] [-o ondate] filename db")


def main():
    verbose = 0
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vo:", "")
    except getopt.GetoptError:
        print("illegal option")
        usage()
        sys.exit(2)

    ondate = stock.yearday(stock.str2epoch("2000-01-01"))
    for o, a in opts:
        if o == "-v":
            verbose = 1
        elif o == "-o":
            ondate = stock.yearday(stock.str2epoch(a))

    if len(args) > 2 or len(args) < 2:
        usage()
        sys.exit(1)

    if len(args) > 0:
        filename = args[0]

    if len(args) > 1:
        dbname = args[1]

    db = ds.dbopen(dbname, "r+")

    dbs = db.lookup(table="site")
    dbq = db.lookup(table="site", record="dbNULL")

    [offdate_null] = dbq.getv("offdate")

    lines = [line.strip() for line in open(filename)]
    recno = -1
    staline = False
    modelline = False
    for line in lines:
        if line.startswith("RESET"):
            continue
        elif modelline and line.strip() == "":
            continue
        elif not staline and line.strip() == "":
            staline = True
        elif staline and line.strip() == "":
            modelline = True
            staline = False
        elif staline:
            #  KBA 4704.70N 1320.68E 1721
            #  OSG 6029.80N  252.55E-110
            #  KRUC4903.71N 1623.71E 341

            sta = line[0:4]
            sta = sta.strip()
            latdeg = line[4:6]
            latmin = line[6:11]
            latsign = line[11:12]
            londeg = line[12:15]
            lonmin = line[15:20]
            lonsign = line[20:21]
            elev_m = line[21:]
            lat = int(latdeg) + float(latmin) / 60.0
            lon = int(londeg) + float(lonmin) / 60.0

            if elev_m.strip() == "":
                elev = 0.0
            else:
                elev = float(elev_m) / 1000.0

            if verbose:
                print("add %s %.1f %.2f %.0f" % (sta, lat, lon, elev))
            recno = dbs.addnull()
            dbs.record = recno
            dbs.putv(
                ("sta", sta),
                ("lat", lat),
                ("lon", lon),
                ("elev", elev),
                ("staname", sta),
                ("ondate", ondate),
            )
    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
