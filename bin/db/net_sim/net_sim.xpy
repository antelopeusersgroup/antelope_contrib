"""
Network Performance Monitoring
- is the network operational?
show a grid of minimum detectable magnitudes

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Jan 2, 2017
@modified    Mar 26, 2024
@version     1.3
@license     MIT-style license

"""

import getopt
import math
import numpy as np
import matplotlib
import matplotlib.pyplot as plt

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

# Import contributed module
sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")

import zamg.utilities as zu


def usage(progname):
    print(
        progname,
        "[-v] [-p pf] [-t time] [-D dist_title] [-M mag_title] [-n nsta] [-i sta[,sta2]]\n\t[-s snr] [-r rmsdb | -R rms] [-S sta/lat/lon/rms] [-Y] [-L] [-f gridfilename] dbname [outfile]",
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
    title_fontsize = "small"
    label_fontsize = "small"
    dist_title_template = "minimum distance to {nsta}th station (km)"
    mag_title_template = "{magtype} magnitude threshold using {nsta} stations"
    plot_dist = True
    plot_mags = True
    plot_borders = False
    plot_cities = False
    plot_stanames = True
    plot_stas = True

    try:
        opts, args = getopt.getopt(sys.argv[1:], "D:M:LYvf:i:n:p:r:R:s:S:t:dmcb", "")
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
    if pf.has_key("station_symbol"):
        station_symbol = pf["station_symbol"]
    else:
        station_symbol = "r+"

    if pf.has_key("cities_dbname") and pf["cities_dbname"] != "":
        plot_cities = True
        dbn = pf["cities_dbname"]
        cities = ds.dbopen(dbn, "r")
        cities = cities.lookup(table="places")
        if pf.has_key("cities_subset") and pf["cities_subset"] != "":
            expr = pf["cities_subset"]
            cities = cities.subset(expr)
    if pf.has_key("borders_dbname") and pf["borders_dbname"] != "":
        plot_borders = True
        dbn = pf["borders_dbname"]
        borders = ds.dbopen(dbn, "r")
        borders = borders.lookup(table="polygon")
        if pf.has_key("borders_subset") and pf["borders_subset"] != "":
            expr = pf["borders_subset"]
            borders = borders.subset(expr)

    # also good: if "title_fontsize" in list(pf.keys()) and pf["title_fontsize"] != "":
    if pf.has_key("title_fontsize") and pf["title_fontsize"] != "":
        sz_str = pf["title_fontsize"]
        if sz_str.isnumeric():
            title_fontsize = int(sz_str)
        else:
            title_fontsize = sz_str

    if pf.has_key("label_fontsize") and pf["label_fontsize"] != "":
        sz_str = pf["label_fontsize"]
        if sz_str.isnumeric():
            label_fontsize = int(sz_str)
        else:
            label_fontsize = sz_str

    params = {
        "legend.fontsize": label_fontsize,
        #'figure.figsize': (15, 5),
        "axes.labelsize": label_fontsize,
        "axes.titlesize": title_fontsize,
        "xtick.labelsize": label_fontsize,
        "ytick.labelsize": label_fontsize,
    }
    plt.rcParams.update(params)

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-d":
            plot_dist = False
        elif o == "-D":
            dist_title_template = a
        elif o == "-M":
            mag_title_template = a
        elif o == "-m":
            plot_mags = False
        elif o == "-c":
            plot_cities = False
        elif o == "-b":
            plot_borders = False
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
        elif o == "-Y":
            plot_stas = False
        elif o == "-L":
            plot_stanames = False

    if len(args) > 2 or len(args) < 1:
        usage(progname)
        sys.exit(1)

    dbname = args[0]

    if len(args) > 1:
        matplotlib.use("Agg")
        plotfilename = args[1]

    db = ds.dbopen_database(dbname, "r")
    if db.table < 0:
        dbsite = db.lookup(table="site")
    else:
        dbsite = db

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
    # this here is acceptable since stations shall not move by more than a few meters,
    # otherwise a different station name should be used
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
        print("sta: %s" % sta)
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

    n_off = len(off_stanames)

    stalat = np.asarray(slats)
    stalon = np.asarray(slons)
    starms = np.asarray(rmss)

    if pf.has_key("ymin") and pf["ymin"] != "":
        latmin = float(pf["ymin"])
        print("ymin")
    else:
        latmin = math.floor(min(stalat))

    if pf.has_key("ymax") and pf["ymax"] != "":
        latmax = float(pf["ymax"])
    else:
        latmax = math.ceil(max(stalat))

    if pf.has_key("xmin") and pf["xmin"] != "":
        lonmin = float(pf["xmin"])
    else:
        lonmin = math.floor(min(stalon))

    if pf.has_key("xmax") and pf["xmax"] != "":
        lonmax = float(pf["xmax"])
    else:
        lonmax = math.ceil(max(stalon))

    number_contours = None
    mindist = None
    maxdist = None
    minmag = None
    maxmag = None
    if pf.has_key("number_contours") and pf["number_contours"] != "":
        number_contours = int(pf["number_contours"])
    if pf.has_key("mindist") and pf["mindist"] != "":
        mindist = float(pf["mindist"])
    if pf.has_key("maxdist") and pf["maxdist"] != "":
        maxdist = float(pf["maxdist"])
    if pf.has_key("minmag") and pf["minmag"] != "":
        minmag = float(pf["minmag"])
    if pf.has_key("maxmag") and pf["maxmag"] != "":
        maxmag = float(pf["maxmag"])

    lats = np.linspace(latmin, latmax, num=ny)
    lons = np.linspace(lonmin, lonmax, num=nx)
    LA, LO = np.meshgrid(lats, lons)

    dist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    # gdist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    # hdist = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    mag = np.empty([number_sites, LA.shape[0], LA.shape[1]])
    for i in range(number_sites):
        # distance in degrees
        dist[i] = zu.spherical_distance(stalat[i], stalon[i], LA, LO, degrees=True)
        mag[i] = c0 + np.log10(starms[i]) + c1 * np.log10(dist[i])
        # convert to kilometers,
        dist[i] *= 111.195

    # distances to each station sorted, so
    #    dist[0] is the distance to the closest station,
    #    dist[1] is the distance to the second closest station ...
    dist = np.sort(dist, axis=0)
    if verbose:
        min_stadist = np.min(dist[0])
        max_sta_dist = np.max(dist[nsta_reqd - 1])
        elog.notify(
            "station spacings form %.1f km to %.1f km (%d stations)"
            % (min_stadist, max_sta_dist, nsta_reqd)
        )
    mag = np.sort(mag, axis=0)

    if plot_borders:
        import zamg.polygon as p

        pdata = p.readpolygon(borders)
    if plot_cities:
        clats = []
        clons = []
        cstrings = []
        cities = cities.subset(
            "lat > %f && lat < %f && lon > %f && lon < %f"
            % (latmin, latmax, lonmin, lonmax)
        )
        for cities.record in range(cities.record_count):
            [lat, lon, city] = cities.getv("lat", "lon", "place")
            clats.append(lat)
            clons.append(lon)
            cstrings.append(city)
        clat = np.asarray(clats)
        clon = np.asarray(clons)

    plt.figure()
    if plot_dist and plot_mags:
        plt.subplot(2, 1, 1)
    if plot_dist:
        if number_contours and mindist and maxdist:
            levels = np.linspace(mindist, maxdist, number_contours)
            contour = plt.contourf(
                LO, LA, dist[nsta_reqd - 1, :, :], levels=levels, extend="both"
            )
        else:
            contour = plt.contourf(LO, LA, dist[nsta_reqd - 1, :, :])
        if n_off > 0:
            plt.plot(off_lons, off_lats, "y.")
            for i in range(n_off):
                plt.text(off_lons[i], off_lats[i], off_stanames[i])
        if plot_stas:
            plt.plot(stalon, stalat, station_symbol)
        if plot_stanames:
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

        title_string = dist_title_template.format(nsta=nsta_reqd)
        plt.title(title_string, fontsize=title_fontsize)
        if plot_cities:
            plt.plot(clon, clat, "k8")
            for i in range(len(cstrings)):
                plt.text(clon[i], clat[i], cstrings[i])
        if plot_borders:
            for i in range(len(pdata)):
                ndata = np.array(pdata[i])
                plon, plat = ndata.T
                plt.plot(plon, plat, "red")

    if verbose:
        min_mag = np.min(mag[0])
        max_mag = np.max(mag[nsta_reqd - 1])
        elog.notify(
            "magnitude thresholds from %.1f to %.1f (%d stations)"
            % (min_mag, max_mag, nsta_reqd)
        )
    if plot_dist and plot_mags:
        plt.subplot(2, 1, 2)
    if plot_mags:
        if number_contours and minmag and maxmag:
            levels = np.linspace(minmag, maxmag, number_contours)
            mag_contour = plt.contourf(
                LO, LA, mag[nsta_reqd - 1, :, :], levels=levels, extend="both"
            )
        else:
            mag_contour = plt.contourf(LO, LA, mag[nsta_reqd - 1, :, :])
        if n_off > 0:
            plt.plot(off_lons, off_lats, "y.")
            for i in range(n_off):
                plt.text(off_lons[i], off_lats[i], off_stanames[i])

        if plot_stas:
            plt.plot(stalon, stalat, station_symbol)
        if plot_stanames:
            for i in range(number_sites):
                if (
                    stalon[i] > lonmin
                    and stalon[i] < lonmax
                    and stalat[i] > latmin
                    and stalat[i] < latmax
                ):
                    plt.text(stalon[i], stalat[i], stanames[i])
        if plot_borders:
            for i in range(len(pdata)):
                ndata = np.array(pdata[i])
                plon, plat = ndata.T
                plt.plot(plon, plat, "red")
        if plot_cities:
            plt.plot(clon, clat, "k8")
            for i in range(len(cstrings)):
                plt.text(clon[i], clat[i], cstrings[i])
        plt.xlim(lonmin, lonmax)
        plt.ylim(latmin, latmax)
        plt.colorbar(mag_contour)
        title_string = mag_title_template.format(magtype=magtype, nsta=nsta_reqd)
        plt.title(
            title_string,
            fontsize=title_fontsize,
        )
    plt.tight_layout(pad=3.0)

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
