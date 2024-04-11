"""
add some helpfule quality indicators to the database

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Feb 18, 2021
@version     1.0
@license     MIT-style license

"""

import getopt
import math
import copy
import statistics as stat
import antelope.datascope as ds
import antelope.orb as orb
import antelope.Pkt as Pkt
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")
import zamg.utilities as zu


def usage(progname):
    print(progname, "[-n] [-F] [-S statefile] db [dbout]")


def max_gap2(c_az):
    """ secondary azimuth gap. I.e. azimuth gap with one station left out"""
    l = len(c_az)
    if l < 2:
        return -1.0
    elif l == 2:
        return 360.0
    c_az.sort()
    max_g2 = 0.0
    n = l - 1  # pop later will remove one item
    for x in range(l):
        az = c_az.copy()  # be careful. Without copying, the input would be modified
        az.pop(x)

        max_g = az[0] + 360.0 - az[n - 1]
        diff = 0.0
        for i in range(1, n):
            diff = az[i] - az[i - 1]
            if diff > max_g:
                max_g = diff
        if max_g > max_g2:
            max_g2 = max_g
    return max_g2


def max_gap(az):
    """ azimuth gap. """
    az.sort()
    n = len(az)
    if n == 0:
        return -1.0
    elif n == 1:
        return 360.0

    max_g = az[0] + 360.0 - az[n - 1]
    diff = 0.0
    for i in range(1, n):
        diff = az[i] - az[i - 1]
        if diff > max_g:
            max_g = diff
    return max_g


def main():
    progname = sys.argv[0].split("/")[-1]
    pfname = progname
    elog.init()

    opts = []
    args = []
    eventdbname = ""
    evid = ""
    forceall = False
    statefile = ""
    mysf = None
    verbose = False

    try:
        opts, args = getopt.getopt(sys.argv[1:], "vhF:S:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        sys.exit(1)

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-h":
            usage(progname)
            sys.exit(0)
        elif o == "-S":
            statefile = a
        elif o == "-F":
            forceall = True
        else:
            print("unknown option %s" % o)
            usage(progname)
            sys.exit(1)

    if len(args) < 1 or len(args) > 2:
        usage(progname)
        sys.exit(1)

    # pfarr = stock.pfread(pfname)
    # output_format = pfarr["output_format"]

    dbname = args[0]
    if len(args) > 1:
        dboutname = args[1]
        dbout = ds.dbopen(dboutname, "r+")
        db = ds.dbopen(dbname, "r")
    else:
        db = ds.dbopen(dbname, "r+")
        dbout = db

    dborigqual = dbout.lookup(table="origqual")

    dbq = db.lookup(table="site", record="dbNULL")
    [latNULL, lonNULL] = dbq.getv("lat", "lon")
    dbq = db.lookup(table="origerr", record="dbNULL")
    [sdobsNULL] = dbq.getv("sdobs")
    dbq = db.lookup(table="origqual", record="dbNULL")
    [gtNULL] = dbq.getv("gt")
    dbo = db.lookup(table="origin")
    dbo = db.lookup(table="origin")
    dbj = dbo.join("assoc")
    dbj = dbj.join(
        "site",
        outer=True,
        pattern1=["sta", "time"],
        pattern2=["sta", "ondate::offdate"],
    )

    #dbj = dbj.subset("time >_2017-10-01_")

    if statefile != "":
        last_orid, last_time = zu.init_statefile(statefile)
        dbj = dbj.subset("orid > %d" % last_orid)
        if dbj.record_count == 0:
            if verbose:
                elog.notify("nothing to do")
            return 0

        dbs = dbj.sort(["orid", "sta"])
        dbg = dbs.group("orid")
        n_origins = dbg.record_count
    if not forceall and dborigqual.record_count > 0:
        last_orid = dborigqual.ex_eval("max(orid)")
        dbj = dbj.subset("orid>%d" % last_orid)
        dbs = dbj.sort(["orid", "sta"])
        dbg = dbs.group("orid")
        n_origins = dbg.record_count
    else:
        dbs = dbj.sort(["orid", "sta"])
        dbg = dbs.group("orid")
        n_origins = dbg.record_count

    if verbose:
        elog.notify("will process %d origins" % n_origins)
    for dbg.record in range(n_origins):
        [r1, r2] = dbg.get_range()
        dbs.record = r1
        [orid, olat, olon] = dbs.getv("orid", "origin.lat", "origin.lon")
        mindist = 20000
        maxdist = 0
        azgap = 360
        azgap2 = 360
        gt = gtNULL
        nsta_def = 0
        nsta30 = 0
        nsta125 = 0
        nsta250 = 0
        nsta400 = 0
        sqrsum = 0.0
        n_times = 0
        stas = []
        azimuths = []
        for dbs.record in range(r1, r2):
            [sta, dtype, delta, timeres, timedef, esaz, slat, slon, dtype] = dbs.getv(
                "sta",
                "dtype",
                "delta",
                "timeres",
                "timedef",
                "esaz",
                "site.lat",
                "site.lon",
                "dtype",
            )
            if timedef == "d":
                if slat != latNULL and slon != lonNULL:
                    dkm = zu.haversine_distance(olat, slat, olon, slon, degrees=False)
                else:
                    dkm = dbs.ex_eval("deg2km(%f)" % delta)
                dkm = dbs.ex_eval("deg2km(%f)" % delta)
                n_times += 1
                sqrsum += timeres * timeres
                if not sta in stas:
                    nsta_def += 1
                    stas.append(sta)
                    azimuths.append(esaz)

                    if dkm < mindist:
                        mindist = dkm
                    if dkm > maxdist:
                        maxdist = dkm
                    if dkm <= 30.0:
                        nsta30 += 1
                        nsta125 += 1
                        nsta250 += 1
                        nsta400 += 1
                    elif dkm <= 125.0:
                        nsta125 += 1
                        nsta250 += 1
                        nsta400 += 1
                    elif dkm <= 250.0:
                        nsta250 += 1
                        nsta400 += 1
                    elif dkm <= 400.0:
                        nsta400 += 1
        azgap2 = max_gap2(azimuths)
        azgap = max_gap(azimuths)
        if n_times > 0:
            tres_rms = math.sqrt(sqrsum / n_times)
            #Bondár, Myers et. al. (2004)
            if nsta250 >= 10 and nsta30 > 0 and azgap < 110.0 and azgap2 < 160.0:
                gt = 5
            if dtype == "g":
                if n_times > 3:
                    sdobs = math.sqrt(sqrsum / (n_times - 3))
                else:
                    sdobs = sdobsNULL
            else:
                if n_times > 4:
                    sdobs = math.sqrt(sqrsum / (n_times - 4))
                else:
                    sdobs = sdobsNULL
            try:
                recno = dborigqual.addv(
                    ("orid", orid),
                    ("mindist", mindist),
                    ("maxdist", maxdist),
                    ("azgap", azgap),
                    ("azgap2", azgap2),
                    ("gt", gt),
                    ("nsta_def", nsta_def),
                    ("nsta30", nsta30),
                    ("nsta125", nsta125),
                    ("nsta250", nsta250),
                    ("nsta400", nsta400),
                    ("tres_rms", tres_rms),
                    ("sdobs", sdobs),
                )
            except Exception as __:
                pass
        else:
            # if verbose:
            #    elog.notify("no defining arrivals for  processing %d origins" % n_origins)
            pass

    if statefile != "":
        zu.update_statefile(statefile, orid, stock.now())
        if verbose:
            elog.notify("updated statefile with orid %d" % orid)
    if verbose:
        elog.notify("done processing %d origins" % n_origins)
    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
