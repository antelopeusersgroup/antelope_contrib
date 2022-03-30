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

# note we need to escape percents %-> %% 
html_template = """
<!DOCTYPE html>
<html>
<head>
	<title>%s</title>
	<meta charset="utf-8" />
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<link rel="stylesheet" href="%s" />
    <link rel="stylesheet" href="/leaflet/leaflet.draw.css" />
    <link rel="stylesheet" href="/leaflet/leaflet.measurecontrol.css" />
	<script src="%s"></script>
    <script src="/leaflet/leaflet.draw.js"></script>
    <script src="/leaflet/leaflet.measurecontrol.min.js"></script>
    <style>
        body { padding:0; margin:0; }
        html, body, #map { height:100%%; }
    .logospace{position: relative; right: 0px; top: -5px; }
    .txtspace{position: relative; right: 0px;top: -5px; background:white; }
    @media all and (max-device-width: 480px) { .logospace{display: none; } }
    </style>        
</head>
<body>
	<div id="map"></div>
	<script>
        var LeafIcon = L.Icon.extend({
            options: {
                iconSize:     [26, 26],
                iconAnchor:   [12, 25],
                popupAnchor:  [0, -25]
                }
            });
        %s    

        %s

        %s

        var osm_Attr='&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> Contributors',
            esri_Attr='&copy; <a href="http://www.esri.com/">Esri</a> i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
            BMAT_Attr='Datenquelle: <a href="http://www.basemat.at/">basemap.at</a>';

        var normal =  L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
                {maxZoom: 18,   attribution: osm_Attr});    
        var zamg =    L.tileLayer('http://wmsx.zamg.ac.at/osmtiles/{z}/{x}/{y}.png',
                {maxZoom: 8,   attribution: osm_Attr});    
        var mq_zamg = L.tileLayer('http://wmsx.zamg.ac.at/osmtiles/mapquest/{z}/{x}/{y}.jpg',
                {maxZoom: 8,   attribution: osm_Attr});
        var ESRI =    L.tileLayer('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
                {maxZoom: 18,   attribution: esri_Attr});    
        var BMAT = L.tileLayer('https://maps{s}.wien.gv.at/basemap/geolandbasemap/normal/google3857/{z}/{y}/{x}.{format}',
                {maxZoom: 19,   format:'png', attribution: BMAT_Attr,
                subdomains: ["", "1", "2", "3", "4"], format: 'png',
                bounds: [[46.35877, 8.782379], [49.037872, 17.189532]] });  
        var BasemapAT_orthofoto = L.tileLayer('https://maps{s}.wien.gv.at/basemap/bmaporthofoto30cm/normal/google3857/{z}/{y}/{x}.{format}', {
                maxZoom: 19, attribution: 'Datenquelle: <a href="www.basemap.at">basemap.at</a>', 
                subdomains: ["", "1", "2", "3", "4"], format: 'jpeg',
                bounds: [[46.35877, 8.782379], [49.037872, 17.189532]] });

		var map = L.map('map', {
			center: [%.2f, %.2f],
			zoom: 6,
			layers: [normal,%s]
		});

		var baseLayers = {
			"OpenstreetMap": normal,
			"ZAMG hosted OSM": zamg,
			"Mapquest/ZAMG": mq_zamg,
			"ESRI": ESRI,
            "Basemap.at": BMAT,
            "Basemap Photo": BasemapAT_orthofoto
		};

		var overlays = {
			%s
		};

		L.control.layers(baseLayers, overlays).addTo(map);
        L.Control.measureControl().addTo(map);
        var logospace= new L.Control();
        logospace.onAdd = function(map) {
            this._div = L.DomUtil.create('div', 'logospace');
            this.update();
            return this._div;
        };
        logospace.update = function () {
            this._div.innerHTML = '<img src="/images/zamg_logo_vert.png" alt="ZAMG Logo">';
            this._div.innerHTML = '<img src="%s" alt="%s">';
        };
        logospace.addTo(map);
        var txtspace= new L.Control( {position: 'bottomleft'} );
        txtspace.onAdd = function(map) {
            this._div = L.DomUtil.create('div', 'txtspace');
            this.update();
            return this._div;
        };
        txtspace.update = function () {
            this._div.innerHTML = 'Last Update: <b>%s</b>';
        };
        txtspace.addTo(map);
	</script>
</body>
</html>
"""

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
    dontshow = pf["dontshow"]
    title = pf["title"]
    gis_layers = pf["layers"]

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
                "ondate", "lat", "lon", "elev", "sta",
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

    my_html = html_template % (
        title,
        leaflet_css,
        leaflet_js,
        "".join(icons),
        "".join(layers),
        "".join(markers),
        center_lon, center_lat,
        ",".join(layer_names),
        ",".join(layer_descriptions),
        logo_url,
        logo_alt,
        creation_time,
    )
    outstr = "".join(my_html)
    with open(htmlfilename, "w", encoding="utf8") as myfile:
        myfile.write(outstr)

    return 0

if __name__ == "__main__":
    status = main()
    sys.exit(status)
