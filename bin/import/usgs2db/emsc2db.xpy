"""
@author      Nikolaus Horn <Nikolaus.Horn@zamg.ac.at>
@created     2013-11-25
@modified    2023-03-03
@version     1.2
@license     MIT-style license
@credits     ZAMG for my visit to EGU 2014
"""


import getopt
import requests
import json
import warnings

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import antelope.orb as orb
import antelope.Pkt as Pkt

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")
import zamg.utilities as zu


def usage(progname):
    # print(progname, "[-v] [-h] [-AB] [-P prefix] [-O|-o orb] [-p proxy_url] [-a auth] [-k keydb] [-u url] dbname")
    print(
        progname,
        "[-v] [-h] [-AB] [-P prefix] [-o|-O orb] [-p proxy_url] [-a auth] [-k keydb] [-u url] [-s schema] dbname",
    )


progname = sys.argv[0].split("/")[-1]
elog.init(progname)
if progname == "usgs2db":
    BASE_URL = (
        "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
    )
    auth = "USGS"
    help_text = """USGS provides at most 1 month of data on the following URL:
http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.geojson.
The default is to retrieve all events from the last hour, regardless of magnitude"""
else:
    BASE_URL = "http://www.seismicportal.eu/fdsnws/event/1/query?limit=10&format=json"
    auth = "EMSC"
    help_text = """EMSC provides at most 20000 events at once on the following URL:
http://www.seismicportal.eu/fdsnws/event/1/query?limit=20000&format=json.
The default here is to retrieve only the most recent 10 events"""

verbose = False
debug = False
problem = False
archive_all = True  # for pfpackets, if True then the value for archive_if_not_associated in the packet is set to yes
opts = []
args = []
keydbname = "keydb"
keyschema = "idmatch1.0"
proxy_url = ""
mag_also_to_mb = False
orbname = ""
orbpkttype = "db"  # or pf for /pf/orb2dbt - format
prefix = ""
dbschema = "css3.0"
try:
    opts, args = getopt.getopt(sys.argv[1:], "a:ABshk:o:O:p:P:u:vd", "")
except getopt.GetoptError:
    elog.die("illegal option")
    usage(progname)
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = True
    elif o == "-d":
        verbose = True
        debug = True
    elif o == "-a":
        auth = a
    elif o == "-A":
        archive_all = False
    elif o == "-u":
        BASE_URL = a
    elif o == "-k":
        keydbname = a
    elif o == "-B":
        mag_also_to_mb = True
    elif o == "-o":
        orbname = a
        orbpkttype = "db"
    elif o == "-O":
        orbname = a
        orbpkttype = "pf"
    elif o == "-P":
        prefix = a
    elif o == "-p":
        proxy_url = a
    elif o == "-s":
        dbschema = a
    elif o == "-h":
        usage(progname)
        print(help_text)
        sys.exit(0)

if len(args) > 1 or len(args) < 1:
    usage(progname)
    sys.exit(1)

if len(args) > 0:
    dbname = args[0]

db = zu.create_dbdesc(dbname, dbschema)
dborigin = db.lookup(table="origin")
dbevent = db.lookup(table="event")
dbnetmag = db.lookup(table="netmag")

dbq = db.lookup(table="origin", field="ml", record="dbNULL")
[mlnull] = dbq.getv("ml")
dbq = db.lookup(table="event", field="evname", record="dbNULL")
evname_width = dbq.query("dbFIELD_SIZE")
dbq = db.lookup(table="origin", field="auth", record="dbNULL")
auth_width = dbq.query("dbFIELD_SIZE")
dbq = db.lookup(table="netmag", field="magtype", record="dbNULL")
magtype_width = dbq.query("dbFIELD_SIZE")

kdb = ds.dbopen(keydbname, "r+")
descname = kdb.query("dbDATABASE_FILENAME")
if os.path.exists(descname):
    schemaname = kdb.query("dbSCHEMA_NAME")
    if schemaname != keyschema:
        elog.die(
            "keydb %s has wrong schema %s, should be %s"
            % (keydbname, schemaname, keyschema)
        )
        sys.exit(1)
else:
    kdb.close()
    ds.dbcreate(keydbname, keyschema)

    kdb = ds.dbopen(keydbname, "r+")
try:
    idmatch = kdb.lookup(table="idmatch")
except Exception as e:
    elog.die("fatal problem with key database:", e)

if proxy_url != "":
    if proxy_url.startswith("https"):
        proxy = {"https": proxy_url}
    else:
        proxy = {"http": proxy_url}
    if debug:
        elog.notify("try to retrieve %s\nvia proxy %s" % (BASE_URL, proxy))
    with warnings.catch_warnings():
        warnings.simplefilter(
            "ignore"
        )  # ignore silly warnings on SSL verification, especially needed on 5.9
        try:
            req = requests.get(BASE_URL, proxies=proxy, verify=False, timeout=30)
            req.raise_for_status()
        except requests.exceptions.HTTPError as herr:
            elog.die("problem requesting data from %s:%s" % (BASE_URL, herr))
        except requests.exceptions.Timeout:
            elog.die("timeout requesting data from %s" % BASE_URL)
        except requests.exceptions.TooManyRedirects:
            elog.die("too many retries requesting data from %s" % BASE_URL)
        except requests.exceptions.RequestException as e:
            elog.die("fatal problem requesting data from %s" % BASE_URL)
        except:
            elog.die("unspecific problem requesting data from %s" % BASE_URL)
else:
    if debug:
        elog.notify("try to retrieve %s" % BASE_URL)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            req = requests.get(BASE_URL, verify=False, timeout=30)
            req.raise_for_status()
        except requests.exceptions.HTTPError as herr:
            elog.die("problem requesting data from %s:%s" % (BASE_URL, herr))
        except requests.exceptions.Timeout:
            elog.die("timeout requesting data from %s" % BASE_URL)
        except requests.exceptions.TooManyRedirects:
            elog.die("too many retries requesting data from %s" % BASE_URL)
        except requests.exceptions.RequestException as e:
            elog.die("fatal problem requesting data from %s" % BASE_URL)
        except:
            elog.die("unspecific problem requesting data from %s" % BASE_URL)
req.encoding = "utf8"  # maybe not necessary...
obj = req.json()
data = obj["features"]
i = len(data)
if debug:
    elog.debug("retrieved %d events" % i)

if orbname != "":
    myorb = orb.orbopen(orbname, permissions="w&")

for index in range(i):
    fdata = data[index]
    unid = fdata["id"]
    geom_type = fdata["type"]
    geometry = fdata["geometry"]
    coordinates = geometry["coordinates"]
    lon = float(coordinates[0])
    lat = float(coordinates[1])
    depth = float(coordinates[2])
    # EMSC correctly specifies depth as a negative number :-)
    if progname == "emsc2db":
        depth *= -1.0
    properties = fdata["properties"]
    mb = ms = ml = mlnull
    time = (
        status
    ) = (
        cdi
    ) = place = code = felt = mag = magtype = net = evtype = auth_str = source_id = ""
    ml = mb = ms = mlnull
    for propk, propv in properties.items():
        if propk == "time":
            try:
                etime = float(propv) / 1000.0
            except ValueError:
                dt = propv.replace("T", " ")
                dt2 = dt.replace("Z", " ")
                etime = stock.str2epoch(dt2)
        elif propk == "mag":
            mag = float(propv)
        elif propk == "depth":
            depth = float(propv)
        elif propk.lower() == "magtype":
            magtype = str(propv)
        elif propk == "place":
            evname = str(propv)
        elif propk == "flynn_region":
            evname = str(propv)
        elif propk == "cdi":
            if propv is not None:
                cdi = float(propv)
                inull = float(propv)
        elif propk == "felt":
            felt = propv
        elif propk == "net":
            net = str(propv)
        elif propk == "auth":
            auth_str = str(propv)
        # elif propk == "unid": #emsc repeats the id in Features as it "unid"
        #    unid = str(propv)
        # elif propk == "code": # usgs calls id code
        #    code = str(propv)
        # elif propk == "source_id":
        #    source_id = str(propv)
        elif propk == "updated":
            updated = propv / 1000.0
        elif propk == "lastupdate":
            dt = propv.replace("T", " ")
            dt2 = dt.replace("Z", " ")
            updated = stock.str2epoch(dt2)
        elif propk == "place":
            place = str(propv)

    # push M to mb, seems to make sense to Niko at least...
    lmt = magtype.lower()
    if lmt == "m":
        magtype = "mb"
    elif lmt == "ml":
        ml = mag
    elif lmt == "mb":
        mb = mag
    elif lmt == "ms":
        ms = mag
    # save mag to origin.mb to for the sake of alarming using orb_quake_alarm
    if mag_also_to_mb:
        mb = mag

    gr = stock.grnumber(lat, lon)
    sr = stock.srnumber(lat, lon)
    jdate = stock.epoch2str(etime, "%Y%j")

    # fkey = str("%s%s" % (net, code))
    # if net != "" and code != "":
    #    unid = "%s%s" % (net, code)
    if debug:
        elog.notify("check id %s" % unid)

    kmatch = idmatch.lookup(table="idmatch", record="dbSCRATCH")
    try:
        kmatch.putv(("fkey", unid))
    except Exception as e:
        elog.die("problem writing key %s to matcher :", (unid, e))

    matcher = kmatch.matches(idmatch, "fkey")
    rec_list = matcher()
    new_event = False
    evid = 0
    updated_event = False
    if len(rec_list) > 1:
        elog.notify("found too many keys for %s, sth strange goes on here" % unid)
    if len(rec_list) > 0:
        for rec in rec_list:
            idmatch.record = rec
            [ftime, kname, kval] = idmatch.getv("ftime", "keyname", "keyvalue")
            if kname == "evid":
                evid = kval
                if updated > ftime:
                    new_event = False
                    updated_event = True
                else:
                    updated_event = False

    else:
        new_event = True

    if new_event:
        problem = False
        if verbose:
            elog.notify("new event %s" % unid)
        evid = dborigin.nextid("evid")
        orid = dborigin.nextid("orid")
        magid = dborigin.nextid("magid")
        try:
            orecno = dborigin.addv(
                ("time", etime),
                ("lat", lat),
                ("lon", lon),
                ("depth", depth),
                ("evid", evid),
                ("orid", orid),
                ("jdate", jdate),
                ("mb", mb),
                ("ml", ml),
                ("ms", ms),
                ("nass", 0),
                ("ndef", 0),
                ("auth", auth),
                ("grn", gr),
                ("srn", sr),
            )
        except Exception as __:
            problem = True
            if verbose:
                elog.notify(
                    "problem adding origin for event at %s" % stock.strtime(etime)
                )
        else:
            dborigin.record = orecno

        if not problem:
            try:
                erecno = dbevent.addv(
                    ("evid", evid),
                    ("prefor", orid),
                    ("evname", zu.string_maxbytes(evname, evname_width)),
                    ("auth", auth),
                )
            except Exception as __:
                problem = True
                if verbose:
                    elog.notify(
                        "problem adding event for events at %s" % stock.strtime(etime)
                    )
            else:
                dbevent.record = erecno

        if not problem:
            try:
                nmrecno = dbnetmag.addv(
                    ("evid", evid),
                    ("orid", orid),
                    ("magid", magid),
                    ("magnitude", mag),
                    ("magtype", zu.string_maxbytes(magtype, magtype_width)),
                    ("auth", auth),
                )
            except Exception as __:
                problem = True
                if verbose:
                    elog.notify(
                        "problem adding netmap for event at %s" % stock.strtime(etime)
                    )
            else:
                dbnetmag.record = nmrecno
                if lmt == "mb":
                    dborigin.putv(("mbid", magid))
                elif lmt == "ms":
                    dborigin.putv(("msid", magid))
                elif lmt == "ml":
                    dborigin.putv(("mlid", magid))
        if not problem:
            if orbname != "" and orbpkttype == "db":
                pkt = Pkt.Packet()
                pkt.srcname = Pkt.SrcName(srcname_string="%s/db/origin" % prefix)
                pkt.db = (
                    dborigin.database,
                    dborigin.table,
                    dborigin.field,
                    dborigin.record,
                )
                try:
                    pkttype, packet, srcname, pkttime = pkt.stuff()
                except Exception:
                    elog.complain("orbstuff error (origin) on %s\n" % unid)
                else:
                    try:
                        myorb.put(srcname, pkttime, packet)
                    except Exception:
                        elog.complain("orbput error (origin) on %s\n" % unid)
                    else:
                        if debug:
                            elog.notify("output db/origin pkt for %s" % unid)

                pkt = Pkt.Packet()
                pkt.srcname = Pkt.SrcName(srcname_string="%s/db/event" % prefix)
                pkt.db = (
                    dbevent.database,
                    dbevent.table,
                    dbevent.field,
                    dbevent.record,
                )
                try:
                    pkttype, packet, srcname, pkttime = pkt.stuff()
                except Exception:
                    elog.complain("orbstuff error (event) on %s\n" % unid)
                else:
                    try:
                        myorb.put(srcname, pkttime, packet)
                    except Exception:
                        elog.complain("orbput error (event) on %s\n" % unid)
                    else:
                        if debug:
                            elog.notify("output db/event pkt for %s" % unid)

                pkt = Pkt.Packet()
                pkt.srcname = Pkt.SrcName(srcname_string="%s/db/netmag" % prefix)
                pkt.db = (
                    dbnetmag.database,
                    dbnetmag.table,
                    dbnetmag.field,
                    dbnetmag.record,
                )
                try:
                    pkttype, packet, srcname, pkttime = pkt.stuff()
                except Exception:
                    elog.complain("orbstuff error (netmag) on %s\n" % unid)
                else:
                    try:
                        myorb.put(srcname, pkttime, packet)
                    except Exception:
                        elog.complain("orbput error (netmag) on %s\n" % unid)
                    else:
                        if debug:
                            elog.notify("output db/netmag pkt for %s" % unid)
            try:
                idmatch.addv(
                    ("fkey", unid),
                    ("keyname", "evid"),
                    ("keyvalue", evid),
                    ("ftime", updated),
                )
            except Exception:
                problem = True
                if verbose:
                    elog.notify(
                        "problem adding id for event at %s" % stock.strtime(etime)
                    )
    elif updated_event:
        if verbose:
            elog.notify("updated event %s" % unid)
        idmatch.putv(("ftime", updated))
        kmatch = db.lookup(table="event", record="dbSCRATCH")
        kmatch.putv(("evid", evid))
        evmatcher = kmatch.matches(dbevent, "evid")
        evlist = evmatcher()
        if len(evlist) > 1:
            elog.notify("strange, found a few matching events for evid %d " % evid)
        if len(evlist) > 0:
            dbevent.record = evlist[0]
            [prefor] = dbevent.getv("prefor")
            kmatch = db.lookup(table="origin", record="dbSCRATCH")
            kmatch.putv(("orid", prefor))
            ormatcher = kmatch.matches(dborigin, "orid")
            orlist = ormatcher()
            if len(orlist) > 1:
                elog.notify("strange, found a few origins for orid %d" % prefor)
            if len(orlist) > 0:
                dborigin.record = orlist[0]
                dborigin.putv(
                    ("time", etime),
                    ("lat", lat),
                    ("lon", lon),
                    ("depth", depth),
                    ("jdate", jdate),
                )
                if magtype.lower() == "ml":
                    dborigin.putv(("ml", mag))
                elif magtype.lower() == "mb":
                    dborigin.putv(("mb", mag))
                elif magtype.lower() == "ms":
                    dborigin.putv(("ms", mag))
                if orbname != "":
                    if orbpkttype == "db":
                        pkt = Pkt.Packet()
                        # points already to right row # dborigin.record = orecno
                        pkt.srcname = Pkt.SrcName(
                            srcname_string="%s/db/origin" % prefix
                        )
                        pkt.db = (
                            dborigin.database,
                            dborigin.table,
                            dborigin.field,
                            dborigin.record,
                        )
                        pkttype, packet, srcname, pkttime = pkt.stuff()
                        myorb.put(srcname, pkttime, packet)

                kmatch = db.lookup(table="netmag", record="dbSCRATCH")
                kmatch.putv(("orid", prefor))
                magmatcher = kmatch.matches(dbnetmag, "orid")
                maglist = magmatcher()
                if len(maglist) > 1:
                    elog.notify("strange, found a few netmags for origin %d" % prefor)
                if len(maglist) > 0:
                    dbnetmag.record = maglist[0]
                    dbnetmag.putv(
                        ("magnitude", mag), ("magtype", magtype), ("auth", auth)
                    )
                    if orbname != "":
                        if orbpkttype == "db":
                            pkt = Pkt.Packet()
                            # points already to right row # dborigin.record = orecno
                            pkt.srcname = Pkt.SrcName(
                                srcname_string="%s/db/origin" % prefix
                            )
                            pkt.db = (
                                dbnetmag.database,
                                dbnetmag.table,
                                dbnetmag.field,
                                dbnetmag.record,
                            )
                            pkttype, packet, srcname, pkttime = pkt.stuff()
                            myorb.put(srcname, pkttime, packet)

    if (
        not problem
        and (new_event or updated_event)
        and orbname != ""
        and orbpkttype != "db"
    ):
        dbevent.get(scratch=True)  # read data into scratch record
        # because it's safe to manipulate the scratch record later
        dbevent.record = ds.dbSCRATCH
        dbevent.putv(("evid", 1), ("prefor", 2), ("commid", -1))

        dborigin.get(scratch=True)  # read data into scratch record
        dborigin.record = ds.dbSCRATCH
        dborigin.putv(
            ("evid", 1),
            ("orid", 2),
            ("commid", -1),
            ("mbid", -1),
            ("msid", -1),
            ("mlid", -1),
        )

        dbnetmag.get(scratch=True)
        dbnetmag.record = ds.dbSCRATCH
        dbnetmag.putv(
            ("magid", -1),
            ("orid", 2),
            ("evid", 1),
            ("commid", -1),
        )
        if lmt == "mb":
            dborigin.putv(("mbid", 3))
            dbnetmag.putv(("magid", 3))
        elif lmt == "ms":
            dborigin.putv(("msid", 3))
            dbnetmag.putv(("magid", 3))
        elif lmt == "ml":
            dborigin.putv(("mlid", 3))
            dbnetmag.putv(("magid", 3))
        pfstring = []
        pfstring.append("schema css3.0\n")
        if archive_all:
            pfstring.append("archive_if_not_associated yes\n")
        else:
            pfstring.append("archive_if_not_associated no\n")
        pfstring.append("event &Literal{\n")
        pfstring.append(dbevent.get())
        pfstring.append("}\n")
        pfstring.append("origin &Literal{\n")
        pfstring.append(dborigin.get())
        pfstring.append("}\n")
        pfstring.append("magnitude_update yes\n")
        pfstring.append("netmag &Literal{\n")
        pfstring.append(dbnetmag.get())
        pfstring.append("}\n")

        pfstring = "".join(pfstring)
        pfout = stock.ParameterFile(type=stock.PFARR, auto_convert=False)
        try:
            pfout.pfcompile(pfstring)
        except Exception:
            elog.complain("pfcompile() error on %s\n" % pfstring)
        else:
            srcname = ""
            pkt = Pkt.Packet()
            pkt.time = stock.now()
            pkt.srcname = Pkt.SrcName(srcname_string="%s/pf/orb2dbt" % prefix)
            pkt.pf = pfout
            try:
                pkttype, packet, srcname, pkttime = pkt.stuff()
            except Exception:
                elog.complain("stuff Pkt error on %s\n" % unid)
            else:
                try:
                    myorb.put(srcname, pkttime, packet)
                except Exception:
                    elog.complain("orbput error on %s\n" % unid)
                else:
                    if debug:
                        elog.notify("output pfpkt for %s" % unid)


sys.exit(0)
