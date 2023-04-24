"""
Create HTML for seismic event(s)

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     2015-01-08
@modified    2022-04-10
@version     1.1
@license     MIT-style license

"""

layer_template = "var %s = new L.LayerGroup();"
marker_template = (
    """L.marker([%f,%f],{icon:%s,title:'%s'}).bindPopup('%s').addTo(%s);"""
)
prefor_marker_template = """L.marker([%f,%f],{icon:%s,title:'%s',zIndexOffset:1000}).bindPopup('%s').addTo(%s);"""
icon_template = """var %s = new LeafIcon({iconUrl: '%s'});"""
marker_html_template = "%s - %s<p>%.2f %.2f %.0fm<p>%s"
origin_marker_html_template = "%s %s %s<p>%.2f %.2f %.0fkm<p>%d %d<p>%s"

origin_tr_template = "<tr><td>%s</td><td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%.2f</td><td>%.2f</td><td>%.1fkm</td><td>%s</td><td>%d</td><td>%d</td><td>%s</td></tr>"
origin_th_template = "<tr><td>evid</td><td>orid</td><td>time</td><td>mag</td><td>etype</td><td>review</td><td>lat</td><td>lon</td><td>depth</td><td>auth</td><td>nass</td><td>ndef</td><td>lddate</td></tr>"
sta_th_template = "<tr><td>sta</td><td>phase</td><td>time</td><td>def</td><td>tres</td><td>auth</td><td>amp</td><td>per</td><td>snr</td></tr>"
sta_tr_template = "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%.2f</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>"
mag_th_template = (
    "<tr><td>sta</td><td>type</td><td>mag</td><td>dist</td><td>azi</td><td>dmag</td>"
)
mag_tr_template = (
    "<tr><td>%s</td><td>%s</td><td>%.1f</td><td>%.2f</td><td>%.0f</td><td>%s</td></tr>"
)

info_template = """
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
<meta name="format-detection" content="telephone=no">
<title>%s</title>
</head>
<body>
<div id="Text">
%s
<br />
%s
</div>
</body>
</html>
"""

# Import Antelope modules

import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import numpy as np
import getopt
import codecs


def usage(progname):
    print(progname, "[-v] [-d dirout] [-s sitedb] [-p pfname] [-o] dbname evid")


def magsize(number):
    size = 12
    if number < 1.0:
        size = 10
    else:
        size = number * number * 1.2 + 9
    if size > 30:
        size = 30
    return size


def jsDate(epoch):
    year = stock.epoch2str(epoch, "%Y")
    js_month = int(stock.epoch2str(epoch, "%m")) - 1
    js_day = int(stock.epoch2str(epoch, "%d")) - 1
    time_string = stock.epoch2str(epoch, "%H,%M,%S")
    datetime_string = "%s,%d,%d,%s" % (year, js_month, js_day, time_string)
    return datetime_string


def get_bin_index(bins, val):
    for i in range(1, len(bins)):
        if val < bins[i]:
            return i - 1
    return len(bins) - 2


def main():
    progname = sys.argv[0].split("/")[-1]
    pfname = progname
    elog.init()

    verbose = False
    archive = 0
    local = 0
    ndays = 1
    opts = []
    args = []
    sitedbname = ""
    histdbname = ""
    dirout = ""
    after = 0
    before = 0
    id_is_orid = False
    try:
        opts, args = getopt.getopt(sys.argv[1:], "d:vp:h:os:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        return 2

    for o, a in opts:
        if o == "-v":
            verbose = True
        if o == "-o":
            id_is_orid = True
        elif o == "-p":
            pf = a
        elif o == "-d":
            dirout = a
        elif o == "-s":
            sitedbname = a

    if len(args) > 2 or len(args) < 2:
        usage(progname)
        sys.exit(1)

    dbname = args[0]

    timenow = stock.now()
    creation_time = stock.epoch2str(timenow, "%d. %m. %Y %H:%M")
    pf = stock.pfread(pfname)

    logo_url = pf["logo_url"]
    logo_alt = pf["logo_alt"]
    leaflet_css = pf["leaflet_css"]
    leaflet_js = pf["leaflet_js"]
    leaflet_draw_css = pf["leaflet_draw_css"]
    leaflet_draw_js = pf["leaflet_draw_js"]
    leaflet_measurecontrol_css = pf["leaflet_measurecontrol_css"]
    leaflet_measurecontrol_js = pf["leaflet_measurecontrol_js"]
    title = pf["title"]
    html_template = pf["html_template"]
    prefor_icon = pf["prefor_icon"]
    origin_icon = pf["origin_icon"]
    defining_icon = pf["defining_icon"]
    nondefining_icon = pf["nondefining_icon"]
    unassoc_icon = pf["unassoc_icon"]

    if id_is_orid:
        orid = int(args[1])
        filebase = "evmap_orid_%010d" % orid
    else:
        evid = int(args[1])
        filebase = "evmap_evid_%010d" % evid

    if not dirout == "":
        filebase = os.path.join(dirout, filebase)
    db = ds.dbopen(dbname, "r")
    my_tables = db.query(ds.dbSCHEMA_TABLES)
    dborigin = db.lookup(table="origin")
    dbevent = db.lookup(table="event")
    dbassoc = db.lookup(table="assoc")
    dbarrival = db.lookup(table="arrival")
    dbq = db.lookup(table="arrival", record="dbNULL")
    [amp_null, per_null, snr_null] = dbq.getv("amp", "per", "snr")
    dbnetmag = db.lookup(table="netmag")
    dbnetmag = dbnetmag.sort(["orid", "magtype"])
    dbstamag = db.lookup(table="stamag")
    dbstamag = dbstamag.sort(["sta", "magtype"])

    if sitedbname == "":
        dbsite = db.lookup(table="site")
    else:
        dbsite = ds.dbopen(sitedbname, "r")
        dbsite = dbsite.lookup(table="site")

    dbsite = dbsite.sort("sta", unique=True)
    dbsitematch = dbsite.lookup(table="site", record="dbSCRATCH")
    sitematcher = dbsitematch.matches(dbsite, "sta")

    if id_is_orid:
        dbp = dborigin.subset("orid==%d" % orid)
        if dbp.record_count < 1:
            elog.complain("no origin found for orid %d" % orid)
            return 1
        dbp = dbp.join("event")
    else:
        dbp = dborigin.subset("evid==%d" % evid)
        if dbp.record_count < 1:
            elog.complain("no origin found for evid %d" % evid)
            return 1
        dbp = dbp.join("event", pattern1="orid", pattern2="prefor")

    nevents = dbp.record_count
    if nevents < 1:
        elog.die("no records after join with event")
        return 1

    dbmagmatch = db.lookup(table="origin", record="dbSCRATCH")
    magmatcher = dbmagmatch.matches(dbnetmag, "orid")

    dbstamagmatch = db.lookup(table="assoc", record="dbSCRATCH")
    stamagmatcher = dbstamagmatch.matches(dbstamag, ["sta", "orid"])
    # print "we have %d origins"  % nevents

    layers = []
    layer_names = []
    layer_descriptions = []

    markers = []
    icons = []

    outstr = []
    np_str = []
    if not id_is_orid:
        dbnp = dbevent.subset("evid==%d" % evid)
        dbnp = dbnp.join("origin")
        dbnp = dbnp.subset("orid != prefor")
        n_nonpref = dbnp.record_count
        if verbose:
            elog.notify("non_prefors: %d" % n_nonpref)
        if n_nonpref > 0:
            icon_name = "oI"
            icons.append(icon_template % (icon_name, origin_icon))
            this_layer = "origin"
            layer_names.append(this_layer)
            layers.append(layer_template % this_layer)
            layer_descriptions.append('"Origin": %s' % this_layer)
            for dbnp.record in range(n_nonpref):
                [
                    otime,
                    olat,
                    olon,
                    depth,
                    oauth,
                    etype,
                    orid,
                    evid,
                    review,
                    onass,
                    ondef,
                    olddate,
                ] = dbnp.getv(
                    "time",
                    "lat",
                    "lon",
                    "depth",
                    "origin.auth",
                    "etype",
                    "orid",
                    "evid",
                    "review",
                    "nass",
                    "ndef",
                    "origin.lddate",
                )
                tooltipstr = "no mag"
                dbmagmatch.putv(("orid", orid))
                magrecs = magmatcher()
                mag = -99.0
                mags_arr = []
                if len(magrecs) > 0:
                    mb = -99.0
                    ml = -99.0
                    ms = -99.0
                    mw = -99.0
                    for magindex in range(len(magrecs)):
                        dbnetmag.record = magrecs[magindex]
                        [mag, magt, nmorid] = dbnetmag.getv(
                            "magnitude", "magtype", "orid"
                        )
                        magt = magt.lower()
                        mags_arr.append("%s:%.1f" % (magt, mag))
                        if magt == "mb":
                            mb = mag
                        elif magt == "ms":
                            ms = mag
                        elif magt == "ml":
                            ml = mag
                        elif magt == "mw" or magt == "mww":
                            mw = mag

                    if mw > -99.0:
                        mag = mw
                        tooltipstr = "mw %.1f" % mw
                    elif ml > -99.0:
                        mag = ml
                        tooltipstr = "ml %.1f" % ml
                    elif ms > -99.0:
                        mag = ms
                        tooltipstr = "ms %.1f" % ms

                ts = stock.epoch2str(otime, "%Y-%m-%d %H:%M:%S.%s")
                ldd_ts = stock.epoch2str(olddate, "%Y-%m-%d %H:%M:%S.%s")
                if mag > -1:
                    mysize = magsize(mag)
                else:
                    mysize = 15
                ss = "%d,%d" % (mysize, mysize)
                marker_html = (
                    "%s <b>%s</b><br /><b>%s</b> %.2f %.2f %.0fkm<br />%s<br /> orid: %d evid: %d"
                    % (
                        ts,
                        tooltipstr,
                        etype,
                        olat,
                        olon,
                        depth,
                        "".join(mags_arr),
                        orid,
                        evid,
                    )
                )
                markers.append(
                    marker_template
                    % (
                        olat,
                        olon,
                        "oI",
                        "%s %s" % (ts, tooltipstr),
                        marker_html,
                        "origin",
                    )
                )
                np_str.append(
                    origin_tr_template
                    % (
                        " ",
                        orid,
                        ts,
                        " ".join(mags_arr),
                        etype,
                        review,
                        olat,
                        olon,
                        depth,
                        oauth,
                        onass,
                        ondef,
                        ldd_ts,
                    )
                )

    icon_name = "pI"
    icons.append(icon_template % (icon_name, prefor_icon))
    this_layer = "prefor"
    layer_names.append(this_layer)
    layers.append(layer_template % this_layer)
    if id_is_orid:
        layer_descriptions.append('"selected Origin": %s' % this_layer)
    else:
        layer_descriptions.append('"Prefor": %s' % this_layer)

    dbp.record = 0
    [
        ptime,
        plat,
        plon,
        pdepth,
        pauth,
        etype,
        orid,
        evid,
        pmb,
        pms,
        pml,
        review,
        pnass,
        pndef,
        plddate,
    ] = dbp.getv(
        "time",
        "lat",
        "lon",
        "depth",
        "origin.auth",
        "etype",
        "orid",
        "evid",
        "mb",
        "ms",
        "ml",
        "review",
        "nass",
        "ndef",
        "origin.lddate",
    )
    ts = stock.epoch2str(ptime, "%Y-%m-%d %H:%M:%S.%s")
    ldd_ts = stock.epoch2str(plddate, "%Y-%m-%d %H:%M:%S.%s")
    preforid = orid
    dbmagmatch.putv(("orid", orid))
    magrecs = magmatcher()
    mag = -99.0
    mags_arr = []
    tooltipstr = "%s no mag" % ts
    mb = -99.0
    ml = -99.0
    ms = -99.0
    mw = -99.0
    if pmb > -90.0:
        mb = pmb
    if pml > -90.0:
        ml = pml
    if pms > -90.0:
        ms = pms

    if len(magrecs) > 0:
        for dbnetmag.record in magrecs:
            [mag, magt, nmorid] = dbnetmag.getv("magnitude", "magtype", "orid")
            magt = magt.lower()
            mags_arr.append("%s:%.1f" % (magt, mag))
            if magt == "mb":
                mb = mag
            elif magt == "ms":
                ms = mag
            elif magt == "ml":
                ml = mag
            elif magt == "mw" or magt == "mww":
                mw = mag

        if mw > -99.0:
            mag = mw
            tooltipstr = "%s mw %.1f" % (ts, mw)
        elif ml > -99.0:
            mag = ml
            tooltipstr = "%s ml %.1f" % (ts, ml)
        elif ms > -99.0:
            mag = ms
            tooltipstr = "%s ms %.1f" % (ts, ms)
        elif mb > -99.0:
            mag = mb
            tooltipstr = "%s mb %.1f" % (ts, mb)

    if mag > -1:
        mysize = magsize(mag)
    else:
        mysize = 15
    ss = "%d,%d" % (mysize, mysize)
    htmlfilename = "%s_evinfo.html" % filebase
    marker_html = (
        '<b>%s</b><br /><b>%s</b> %.2f %.2f %.0fkm<br />%s<br />orid: %d evid: %d<br />Auth: %s<br /><a href="%s">more information</a>'
        % (
            tooltipstr,
            etype,
            plat,
            plon,
            pdepth,
            "".join(mags_arr),
            orid,
            evid,
            pauth,
            htmlfilename,
        )
    )
    markers.append(
        prefor_marker_template
        % (plat, plon, "pI", "%s %s" % (ts, tooltipstr), marker_html, "prefor")
    )

    outstr.append('<img src="%s"> Prefor         %d<br/>' % (prefor_icon, orid))
    outstr.append("<table border=2 cellpadding=5 cellspacing=3>")
    outstr.append(origin_th_template)
    outstr.append(
        origin_tr_template
        % (
            "%d" % evid,
            orid,
            ts,
            " ".join(mags_arr),
            etype,
            review,
            plat,
            plon,
            pdepth,
            pauth,
            pnass,
            pndef,
            ldd_ts,
        )
    )
    if n_nonpref > 0:
        outstr.append("".join(np_str))
    outstr.append("</table>")

    used_stas = []
    has_def = False
    has_assoc = False
    mag_str = []

    dba = dbp.join("assoc")
    dba = dba.join("arrival")
    dbps = dba.sort(["sta", "time"])
    dbpg = dbps.group("sta")
    n_arrivals = dbps.record_count
    n_stas = dbpg.record_count
    if verbose:
        elog.notify("%d arrivals from %d stations" % (n_arrivals, n_stas))
    if n_arrivals > 0:
        mag_str.append("<table border=2 cellpadding=3>")
        mag_str.append(mag_th_template)
        outstr.append("<br /><table border=1 cellpadding=4>")
        outstr.append(sta_th_template)
        for dbpg.record in range(dbpg.record_count):
            [r1, r2] = dbpg.get_range()
            dbps.record = r1
            [sta, delta, esaz] = dbps.getv("sta", "delta", "esaz")
            used_stas.append(sta)
            dbstamagmatch.putv(("sta", sta), ("orid", orid))
            stamagrecords = stamagmatcher()
            first_stamag = True
            if len(stamagrecords) > 0:
                for dbstamag.record in stamagrecords:
                    [stamagtype, stamag] = dbstamag.getv("magtype", "magnitude")
                    magdiff = " "
                    if stamagtype.lower() == "mb" and mb > -90.0:
                        magdiff = mb - stamag
                    if stamagtype.lower() == "ml" and ml > -90.0:
                        magdiff = ml - stamag
                    if stamagtype.lower() == "ms" and ms > -90.0:
                        magdiff = ms - stamag
                    if (
                        stamagtype.lower() == "mw" or stamagtype.lower() == "mww"
                    ) and mw > -90.0:
                        magdiff = mw - stamag
                    if first_stamag:
                        mag_str.append(
                            mag_tr_template
                            % (sta, stamagtype, stamag, delta, esaz, magdiff)
                        )
                        first_stamag = False
                    else:
                        mag_str.append(
                            mag_tr_template
                            % (" ", stamagtype, stamag, delta, esaz, magdiff)
                        )

            dbsitematch.putv(("sta", sta))
            siterecords = sitematcher()
            if len(siterecords) > 0:
                dbsite.record = siterecords[0]
                [stalat, stalon, staname] = dbsite.getv("lat", "lon", "staname")
                staname = staname.replace("'", "\\'")
                stastr = []
                stastr.append("%s - %s" % (sta, staname))
                sta_def = False
                sta_phases = []
                first_phase_here = True
                for dbps.record in range(r1, r2):
                    [
                        atime,
                        iphase,
                        phase,
                        timeres,
                        timedef,
                        aauth,
                        aamp,
                        aper,
                        asnr,
                    ] = dbps.getv(
                        "arrival.time",
                        "iphase",
                        "phase",
                        "timeres",
                        "timedef",
                        "arrival.auth",
                        "amp",
                        "per",
                        "snr",
                    )
                    sta_phases.append(phase)
                    ts = stock.epoch2str(atime, "%H:%M:%S.%s")
                    if aamp != amp_null:
                        amp_str = "%.1f" % aamp
                    else:
                        amp_str = "&nbsp;"

                    if aper != per_null:
                        per_str = "%.1f" % aper
                    else:
                        per_str = "&nbsp;"

                    if asnr != snr_null:
                        snr_str = "%.1f" % asnr
                    else:
                        snr_str = "&nbsp;"

                    if first_phase_here:
                        outstr.append(
                            sta_tr_template
                            % (
                                sta,
                                phase,
                                ts,
                                timedef,
                                timeres,
                                aauth,
                                amp_str,
                                per_str,
                                snr_str,
                            )
                        )
                        first_phase_here = False
                    else:
                        outstr.append(
                            sta_tr_template
                            % (
                                " ",
                                phase,
                                ts,
                                timedef,
                                timeres,
                                aauth,
                                amp_str,
                                per_str,
                                snr_str,
                            )
                        )
                    if timedef == "d":
                        has_def = True
                        sta_def = True
                        stastr.append(
                            "<br />%s <b>%6s</b>(%6s) d %.2f"
                            % (ts, phase, iphase, timeres)
                        )
                    else:
                        has_assoc = True
                        stastr.append(
                            "<br />%s %6s(%6s) n %.2f" % (ts, phase, iphase, timeres)
                        )

                tooltipstr = "%s %s %.2f (%.1fkm)" % (
                    sta,
                    " ".join(sta_phases),
                    delta,
                    delta * 111.37,
                )
                if sta_def:
                    markers.append(
                        marker_template
                        % (stalat, stalon, "dI", tooltipstr, "".join(stastr), "def")
                    )
                else:
                    markers.append(
                        marker_template
                        % (stalat, stalon, "nI", tooltipstr, "".join(stastr), "non")
                    )

        outstr.append("</table>")
        mag_str.append("</table>")

    if has_def:
        icon_name = "dI"
        icons.append(icon_template % (icon_name, defining_icon))
        this_layer = "def"
        layer_names.append(this_layer)
        layers.append(layer_template % this_layer)
        layer_descriptions.append('"defining Arrivals": %s' % this_layer)
    if has_assoc:
        icon_name = "nI"
        icons.append(icon_template % (icon_name, nondefining_icon))
        this_layer = "non"
        layer_names.append(this_layer)
        layers.append(layer_template % this_layer)
        layer_descriptions.append('"associated Arrivals": %s' % this_layer)

    if len(used_stas) > 0:
        sta_expr = "sta!~/%s/" % "|".join(used_stas)
        dbsite = dbsite.subset(sta_expr)

    n_sites_left = dbsite.record_count
    if n_sites_left > 0:
        icon_name = "xI"
        icons.append(icon_template % (icon_name, unassoc_icon))
        this_layer = "xxx"
        layers.append(layer_template % this_layer)
        layer_descriptions.append('"unused Stations": %s' % this_layer)
        for dbsite.record in range(n_sites_left):
            [sta, stalat, stalon, staname] = dbsite.getv("sta", "lat", "lon", "staname")
            staname = staname.replace("'", "\\'")
            marker_html = "%s - %s<br />%.2f %.2f" % (sta, staname, stalat, stalon)
            markers.append(
                marker_template
                % (stalat, stalon, "xI", "%s - %s" % (sta, staname), marker_html, "xxx")
            )

    if id_is_orid:
        titlestring = "Origin %d" % orid
    else:
        titlestring = "Event %d - prefor %d" % (orid, preforid)

    my_html = html_template.format(
        title=title,
        leaflet_css=leaflet_css,
        leaflet_js=leaflet_js,
        leaflet_draw_css=leaflet_draw_css,
        leaflet_draw_js=leaflet_draw_js,
        leaflet_measurecontrol_css=leaflet_measurecontrol_css,
        leaflet_measurecontrol_js=leaflet_measurecontrol_js,
        icons="".join(icons),
        layers="".join(layers),
        markers="".join(markers),
        layer_names=",".join(layer_names),
        layer_descriptions=",".join(layer_descriptions),
        center_lat=plat,
        center_lon=plon,
        logo_url=logo_url,
        logo_alt=logo_alt,
        creation_time=creation_time,
    )
    htmlfilename = "%s.html" % filebase
    with open(htmlfilename, "w", encoding="utf8") as myfile:
        myfile.write(my_html)

    if verbose:
        elog.log("file done %s" % htmlfilename)

    my_html = info_template % (titlestring, "".join(outstr), "".join(mag_str))
    htmlfilename = "%s_evinfo.html" % filebase
    with open(htmlfilename, "w", encoding="utf8") as myfile:
        myfile.write(my_html)
    if verbose:
        elog.log("file done %s" % htmlfilename)
    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
