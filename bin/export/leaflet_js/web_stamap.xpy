"""
Stations

Create HTML page with station map

@author      Nikolaus Horn <horn.niko@gmail.com>
@created     2013-01-18
@modified    2016-08-31
@version     1.0
@license     MIT-style license
@credits     a few ideas borrowed from Rob Newmans db2kml_py

"""

layer_template = "var %s = new L.LayerGroup();"
# L.marker([48.2, 16.3],{title:'Wien'}).bindPopup('This is Vienna, AT.').addTo(cities);
marker_template = (
    """L.marker([%f,%f],{icon:%s,title:'%s'}).bindPopup('%s').addTo(%s);"""
)
icon_template = """var %s = new LeafIcon({iconUrl: '%s'});"""
marker_html_template = "%s - %s<p>%.2f %.2f %.0fm<p>%s"

import getopt

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog


def usage(progname):
    print(progname, "[-v] [-f filename] [-p pfname] dbname")


def main():
    progname = sys.argv[0].split("/")[-1]
    pfname = progname
    elog.init(progname)

    verbose = 0
    archive = 0
    local = 0
    ndays = 1
    htmlfilename = ""
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "f:vp:", "")
    except getopt.GetoptError:
        elog.notify("illegal option")
        usage(progname)
        sys.exit(2)

    for o, a in opts:
        if o == "-v":
            verbose = 1
        elif o == "-d":
            ndays = int(a)
        elif o == "-f":
            htmlfilename = a
        elif o == "-p":
            pfname = a

    if len(args) > 1 or len(args) < 1:
        usage(progname)
        sys.exit(1)

    if len(args) > 0:
        dbname = args[0]

    timenow = stock.now()
    creation_time = stock.epoch2str(timenow, "%d. %m. %Y %H:%M")
    strtimenow = stock.epoch2str(timenow, "%Y-%m-%d:%H:%M.%S")
    pf = stock.pfread(pfname)
    if htmlfilename == "":
        htmlfilename = pf["filebase"] + ".html"
    logo_url = pf["logo_url"]
    logo_alt = pf["logo_alt"]
    leaflet_css = pf["leaflet_css"]
    leaflet_js = pf["leaflet_js"]
    leaflet_draw_css = pf["leaflet_draw_css"]
    leaflet_draw_js = pf["leaflet_draw_js"]
    leaflet_measurecontrol_css = pf["leaflet_measurecontrol_css"]
    leaflet_measurecontrol_js = pf["leaflet_measurecontrol_js"]
    dontshow = pf["dontshow"]
    title = pf["title"]
    gis_layers = pf["layers"]
    html_template = pf["html_template"]

    db = ds.dbopen(dbname, "r")
    dbsite = db.lookup(table="site")
    dbsnetsta = db.lookup(table="snetsta")
    dbsite = dbsite.subset(
        "(offdate == NULL || offdate > now())"
    )  # some temporary deployments know station closure in advance
    dbsite = dbsite.subset("sta !~ /%s/" % dontshow)

    layers = []
    layer_names = []
    markers = []
    layer_descriptions = []
    icons = []

    center_lat = dbsite.ex_eval("sum(lat) / count()")
    center_lon = dbsite.ex_eval("sum(lon) / count()")

    layer_index = 0
    for gis_layer in gis_layers:
        # print gis_layer
        this_icon = gis_layer["icon"]
        this_description = gis_layer["description"]
        this_stations = gis_layer["stations"]
        icon_name = "Icon_%d" % layer_index
        icons.append(icon_template % (icon_name, this_icon))
        this_layer = "l_%d" % layer_index
        if len(layer_names) < 1:
            layer_names.append(this_layer)
        layers.append(layer_template % this_layer)
        layer_descriptions.append('"%s" : %s' % (this_description, this_layer))
        if this_stations != "":
            dbs = dbsite.subset("sta =~ /%s/" % this_stations)
            dbsite = dbsite.subset("sta !~ /%s/" % this_stations)
        else:
            dbs = dbsite
        layer_index += 1
        for dbs.record in range(dbs.record_count):
            [ondate, lat, lon, elev, sta] = dbs.getv(
                "ondate",
                "lat",
                "lon",
                "elev",
                "sta",
            )
            try:
                [staname] = dbs.getv("staname")
            except Exception as __:
                elog.notify("problem reading staname for %s" % sta)
                staname = ""

            staname = staname.replace("'", "\\'")
            marker_html = marker_html_template % (
                sta,
                staname,
                lat,
                lon,
                elev * 1000.0,
                ondate,
            )
            markers.append(
                marker_template % (lat, lon, icon_name, sta, marker_html, this_layer)
            )

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
        layer_names=",".join(layer_names),
        layer_descriptions=",".join(layer_descriptions),
        markers="".join(markers),
        center_lat=center_lat,
        center_lon=center_lon,
        logo_url=logo_url,
        logo_alt=logo_alt,
        creation_time=creation_time,
    )
    with open(htmlfilename, "w", encoding="utf8") as myfile:
        myfile.write(my_html)

    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
