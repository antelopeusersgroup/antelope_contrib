"""
Network Performance Monitoring
- is the network operational?
show a grid of minimum detectable magnitudes

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Jan 2, 2017
@modified    Sep 13, 2018
@version     1.3
@license     MIT-style license

"""

import getopt
import numpy as np
import math

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
# Import contributed module
sys.path.append(os.environ['ANTELOPE'] + "/contrib/data/python")

import zamg.utilities as zu

def usage(progname):
    print(
        progname,
        "[-v] [-n nsta] [-i sta[,sta2]] [-s snr] [-r rmsdb | -R rms] [-S sta/lat/lon/rms] [-f gridfilename] dbname [outfile]",
    )


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init()

    verbose = False
    pfname = progname
    archive = 0
    local = 0
    ndays = 1
    opts = []
    args = []
    rmsdbname = ""
    rmstime = ""
    gridfilename = ""
    plotfilename = ""
    after = 0
    before = 0
    x_stations = []
    stations_to_ignore = []
    station_rms = []
    nsta_reqd = 4
    default_rms = 4.0
    snr = 4.0
    nx = ny = 100
    rms_latency = 120  # two minutes ago
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vf:h:i:n:p:r:R:s:S:t:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        return 2

    for o, a in opts:
        if o == "-p":
            pfname = a

    pf = stock.pfread(pfname)
    nsta_reqd = int(pf["nsta"])
    nx = int(pf["nx"])
    ny = int(pf["ny"])
    default_rms = float(pf["default_rms"])
    c0 = float(pf["c0"])
    c1 = float(pf["c1"])
    magtype = pf["magtype"]
    rms_latency = float(pf["rms_latency"])
    rms_filter_spec = pf["rms_filter_spec"]
    stations_used = pf["stations_used"]
    stations_ignored = pf["stations_ignored"]
    station_rms = pf["station_rms"]

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-h":
            histdbname = a
        elif o == "-i":
            i_list = a.replace("/", ",").split(",")
            stations_to_ignore = stations_to_ignore + i_list
        elif o == "-n":
            nsta_reqd = int(a)
        elif o == "-p":
            pfname = a
        elif o == "-r":
            rmsdbname = a
        elif o == "-R":
            default_rms = float(a)
        elif o == "-s":
            snr = float(a)
        elif o == "-S":
            sta_lst = a.replace(",", "/").split("/")
            if len(sta_lst) == 4:
                x_stations.append(
                    {
                        "sta": sta_lst[0],
                        "lat": float(sta_lst[1]),
                        "lon": float(sta_lst[2]),
                        "rms": float(sta_lst[3]),
                    }
                )
            else:
                print(
                    "an additional station must be specified as staname,lat,lon and rms, separated by '/'\ne.g.: -S Sta1/46.2/15.3/20"
                )
                usage(progname)
        elif o == "-t":
            rmstime = a
        elif o == "-f":
            gridfilename = a

    if len(args) > 2 or len(args) < 1:
        usage(progname)
        sys.exit(1)

    dbname = args[0]
    import matplotlib

    if len(args) > 1:
        matplotlib.use("Agg")
        plotfilename = args[1]
    import matplotlib.pyplot as plt

    db = ds.dbopen(dbname, "r")
    dbsite = db.lookup(table="site")
    # the problem with "offdate==NULL" is that some stations, especially for projects with fixed lifetime,
    #   have an offdate in the future
    # dbsite=dbsite.subset("offdate == NULL")
    dbsite = dbsite.subset(
        "lat != NULL && lon != NULL"
    )  # only sites with coordinates...
    dbsite = dbsite.sort(
        "sta", unique=True
    )  # similar to offdate == NULL, and always ok
    # might improve by offdate==NULL || offdate >= OFFDATE_NOW
    if len(stations_used) > 0:
        dbsite = dbsite.subset("sta=~/%s" % "|".join(stations_used))
        if verbose:
            print("use the following stations: %s" % "|".join(stations_used))

    if len(stations_to_ignore) > 0:
        dbsite = dbsite.subset("sta !~/%s/" % "|".join(stations_to_ignore))
        if verbose:
            print("ignoring the following stations: %s" % "|".join(stations_to_ignore))
    elif len(stations_ignored) > 0:
        dbsite = dbsite.subset("sta !~/%s/" % "|".join(stations_ignored))
        if verbose:
            print("ignoring the following stations: %s" % "|".join(stations_ignored))

    nsites = dbsite.record_count
    if nsites < 1:
        print("no site left after subset")
        sys.exit(1)

    if rmsdbname != "":
        rdb = ds.dbopen(rmsdbname, "r")
        dbrms = rdb.lookup(table="wfrms")
        if rmstime == "":
            time = stock.now() - rms_latency
        else:
            time = stock.str2epoch(rmstime)

        dbrms = dbrms.subset("time < %f && (time + twin) > %f" % (time, time))
        dbrms = dbrms.subset('filter =="%s"' % rms_filter_spec)
        if dbrms.record_count < 1:
            print(
                "no record left in rms db. Maybe the filter specification '%s' is wrong?"
                % rms_filter_spec
            )

    slats = []
    slons = []
    stanames = []
    rmss = []

    off_stanames = []
    off_lats = []
    off_lons = []

    number_sites = 0
    for dbsite.record in range(nsites):
        [sta, slat, slon, selev] = dbsite.getv("sta", "lat", "lon", "elev")
        if sta in stations_to_ignore:
            if verbose:
                print("ignore station %s" % sta)
        else:
            if rmsdbname == "":
                rms_found = False
                if len(station_rms) > 0:
                    if sta in list(station_rms.keys()):
                        sta_rms = float(station_rms[sta])
                        rms_found = True
                        rmss.append(sta_rms * snr)
                        if verbose:
                            print("station rms for station %s found, using this" % sta)
                if not rms_found:
                    rmss.append(default_rms * snr)
                    if verbose:
                        print(
                            "no station rms for station %s found, using default rms"
                            % sta
                        )
                slats.append(slat)
                slons.append(slon)
                stanames.append(sta)
            else:
                rms_found = False
                try:
                    myrec = dbrms.find("sta=~/%s/" % sta)
                except Exception as e:
                    if len(station_rms) > 0:
                        if sta in list(station_rms.keys()):
                            sta_rms = float(station_rms[sta])
                            rms_found = True
                            if verbose:
                                print(
                                    "no rms in db but in station-rms for station %s"
                                    % sta
                                )
                    if not rms_found:
                        off_stanames.append(sta)
                        off_lats.append(slat)
                        off_lons.append(slon)
                        if verbose:
                            print(
                                "no rms and no station_rms for station %s found, skiping that station"
                                % sta
                            )
                        continue
                if rms_found:
                    rmss.append(sta_rms * snr)
                    if verbose:
                        print("use default station-rms for %s" % sta)
                else:
                    dbrms.record = myrec
                    [observed_rms] = dbrms.getv("rms")
                    rmss.append(observed_rms * snr)
                    if verbose:
                        print("found rms for %s in database" % sta)
                slats.append(slat)
                slons.append(slon)
                stanames.append(sta)
            number_sites += 1
    for xs in x_stations:
        slats.append(xs["lat"])
        slons.append(xs["lon"])
        stanames.append(xs["sta"])
        rmss.append(xs["rms"] * snr)
        number_sites += 1

    stalat = np.asarray(slats)
    stalon = np.asarray(slons)
    starms = np.asarray(rmss)

    if "ymin" in list(pf.keys()) and pf["ymin"] != "":
        latmin = float(pf["ymin"])
    else:
        latmin = math.floor(min(stalat))
    if "ymax" in list(pf.keys()) and pf["ymax"] != "":
        latmax = float(pf["ymax"])
    else:
        latmax = math.ceil(max(stalat))
    if "xmin" in list(pf.keys()) and pf["xmin"] != "":
        lonmin = float(pf["xmin"])
    else:
        lonmin = math.floor(min(stalon))
    if "xmax" in list(pf.keys()) and pf["xmax"] != "":
        lonmax = float(pf["xmax"])
    else:
        lonmax = math.ceil(max(stalon))

    lats = np.linspace(latmin, latmax, num=ny)
    lons = np.linspace(lonmin, lonmax, num=nx)
    LA, LO = np.meshgrid(lats, lons)

    dist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    gdist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    hdist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    mag = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    for i in range(number_sites):
        # distance in degrees
        dist[i] = zu.spherical_distance(stalat[i], stalon[i], LA, LO, degrees=True)
        mag[i] = c0 + np.log10(starms[i]) + c1 * np.log10(dist[i])
        # convert to kilometers,
        dist[i] *= 111.195

    dist = np.sort(dist, axis=0)
    mag = np.sort(mag, axis=0)
    plt.figure()
    plt.subplot(2, 1, 1)
    contour = plt.contourf(LO, LA, dist[nsta_reqd - 1, :, :])
    n_off = len(off_stanames)
    if n_off > 0:
        plt.plot(off_lons, off_lats, "y.")
        for i in range(n_off):
            plt.text(off_lons[i], off_lats[i], off_stanames[i])
    plt.plot(stalon, stalat, "r+")
    for i in range(number_sites):
        if (
            stalon[i] > lonmin
            and stalon[i] < lonmax
            and stalat[i] > latmin
            and stalat[i] < latmax
        ):
            plt.text(stalon[i], stalat[i], stanames[i])
    plt.xlim(lonmin, lonmax)
    plt.ylim(latmin, latmax)
    plt.colorbar(contour)

    plt.title("minimum distance to %dth station (km)" % nsta_reqd)

    plt.subplot(2, 1, 2)
    mag_contour = plt.contourf(LO, LA, mag[nsta_reqd - 1, :, :])
    if n_off > 0:
        plt.plot(off_lons, off_lats, "y.")
        for i in range(n_off):
            plt.text(off_lons[i], off_lats[i], off_stanames[i])

    plt.plot(stalon, stalat, "r+")
    for i in range(number_sites):
        if (
            stalon[i] > lonmin
            and stalon[i] < lonmax
            and stalat[i] > latmin
            and stalat[i] < latmax
        ):
            plt.text(stalon[i], stalat[i], stanames[i])
    plt.xlim(lonmin, lonmax)
    plt.ylim(latmin, latmax)
    plt.colorbar(mag_contour)
    plt.title("%s detection threshold using %d stations" % (magtype, nsta_reqd))

    if plotfilename != "":
        plt.savefig(plotfilename)
        if verbose:
            elog.log("plotfile saved to %s" % plotfilename)
    else:
        plt.show()

    if gridfilename != "":

        np.savetxt(
            gridfilename,
            np.transpose(
                [
                    np.reshape(LO, LO.size),
                    np.reshape(LA, LA.size),
                    np.reshape(mag[nsta_reqd - 1, :, :], LA.size),
                    np.reshape(dist[nsta_reqd - 1, :, :], LA.size),
                ]
            ),
            delimiter=",",
            newline=os.linesep,
            fmt=["%.5f", "%.5f", "%.5f", "%.3f"],
            header="lon, lat, mag, distkm",
        )
        # np.savetxt(gridfilename, np.transpose([LO,LA,mag[nsta_reqd -1,:,:]]), fmt='%.6f')
        if verbose:
            elog.log("gridfile saved to %s" % gridfilename)

    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
