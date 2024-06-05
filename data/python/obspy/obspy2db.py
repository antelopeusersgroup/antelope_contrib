import os
import sys
import datetime  # to convert datetime used by obspy to epoch used by Antelope

# try:
#    # from obspy import read as op_read
#    from obspy.io.nordic.core import read_nordic as op_read_nordic
# except Exception as __:
#    print(
#        "cannot load ObsPy module. Maybe you need to install ObsPy (pip install obspy)?"
#    )

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog


sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")

import zamg.utilities as zu

# import zamg.missing as zm


def event_type2etype(event_type, event_type_certainty="known"):
    etype = "-"
    if event_type == "landslide":
        etype = "kl"
    elif event_type == "explosion":
        etype = "kx"
    elif event_type == "quarry blast":
        etype = "km"
    elif event_type == "earthquake":
        etype = "eq"
    elif event_type == "nuclear explosion":
        etype = "kn"
    elif event_type == "rock burst":
        etype = "kr"
    if event_type_certainty == "suspected":
        etype[0] = "s"
    return etype


def fix_stachan(sta, chan, params):
    if "sta_trans" in params:
        if sta in params["sta_trans"].keys():
            sta = params["sta_trans"][sta]
    if "chan_trans" in params:
        if chan in params["chan_trans"].keys():
            chan = params["chan_trans"][chan]

    return sta, chan


def obspy_datetime2epoch(dt):
    """
    convert obspy datetime to epoch.
    By default, the function "timestamp" somehow takes the local time offset into account.
    This is trivially simple, but easy to forget. Therefore we need this function :-)
    """

    try:
        # black magic to force local timezone (local to this conversion) to be UTC
        et = dt.datetime.replace(tzinfo=datetime.timezone.utc).timestamp()
    except Exception as __:
        et = -9999999999.99900

    return et


def inventory2db(inventory, db, params):
    verbose = False
    debug = False
    level = "response"
    if "verbose" in params:
        verbose = params["verbose"]
    if "debug" in params:
        debug = params["debug"]
    if "level" in params:
        level = params["level"]

    dbnetwork = db.lookup(table="network")
    for net in inventory:
        netcode = net.code
        if verbose or debug:
            print(netcode)
        dbnetwork = db.lookup(table="network")
        recno = dbnetwork.addv(
            ("net", netcode),
            ("netname", netcode + " seismic network"),
            ("auth", "Niko"),
        )
        if level in ["station", "channel", "response"]:
            dbsite = db.lookup(table="site")
            dbaffiliation = db.lookup(table="affiliation")
            dbq = dbsite.lookup(record="dbNULL")
            [ondate_NULL, offdate_NULL] = dbq.getv("ondate", "offdate")
            for sta in net.stations:
                if debug:
                    print(sta)
                fsta = sta.code
                elev_m = sta.elevation
                lat = sta.latitude
                lon = sta.longitude
                # this automagically adds entries to the table 'snetsta'
                css_sta = db.map_seed_netsta(netcode, fsta)

                ## translate NULL from one system to another
                # if sta.creation_date is not None:
                #    ondate = stock.yearday(obspy_datetime2epoch(sta.creation_date))
                # else:
                #    ondate = ondate_NULL
                # if sta.termination_date is not None:
                #    offdate = stock.yearday(obspy_datetime2epoch(sta.termination_date))
                # else:
                #    offdate = offdate_NULL

                # translate NULL from one system to another
                if sta.start_date is not None:
                    ondate = stock.yearday(obspy_datetime2epoch(sta.start_date))
                else:
                    ondate = ondate_NULL
                if sta.end_date is not None:
                    offdate = stock.yearday(obspy_datetime2epoch(sta.end_date))
                else:
                    offdate = offdate_NULL
                try:
                    recno = dbsite.addv(
                        ("sta", css_sta),
                        ("ondate", ondate),
                        ("offdate", offdate),
                        ("lat", lat),
                        ("lon", lon),
                        ("elev", elev_m / 1000.0),
                    )
                except Exception as siteproblem:
                    elog.flush(False, 0)
                    elog.notify("forcing site %s\n%s" % (sta, siteproblem))
                    try:
                        recno = dbsite.addnull()
                        dbsite.record = recno
                        dbsite.putv(
                            ("sta", css_sta),
                            ("ondate", ondate),
                            ("offdate", offdate),
                            ("lat", lat),
                            ("lon", lon),
                            ("elev", elev_m / 1000.0),
                        )
                    except Exception as sp2:
                        elog.notify(
                            "problem forcing the addition of site %s\n%s"
                            % (sta, siteproblem)
                        )
                        continue  # hop to the next site

                try:
                    recno = dbaffiliation.addv(("net", netcode), ("sta", css_sta))
                except Exception as __:
                    pass  # ignore this problem. Duplicate site entries only need one entry to the table affiliation.
                if verbose:
                    print(css_sta)

                if level in ["channel", "response"]:
                    # add sitechan, sensor, instrument, calibration, stage, schanloc
                    dbsensor = db.lookup(table="sensor")
                    dbq = dbsensor.lookup(record="dbNULL")
                    [time_NULL, endtime_NULL, inid_NULL, chanid_NULL] = dbq.getv(
                        "time", "endtime", "inid", "chanid"
                    )

                    for chan in sta.channels:
                        if debug:
                            print(chan)
                        fchan = chan.code
                        loc = chan.location_code

                        # this call already adds an entry to the table schanloc
                        css_chan = db.map_seed_chanloc(fsta, fchan, loc)
                        print(css_chan)
                        az = chan.azimuth
                        dip = chan.dip
                        depth = chan.depth
                        elev = chan.elevation
                        chanlat = chan.latitude
                        chanlon = chan.longitude
                        samprate = chan.sample_rate
                        description = []
                        if chan.description is not None:
                            description.append(chan.description)
                        if chan.sensor is not None:
                            if chan.sensor.type is not None:
                                description.append(chan.sensor.type)
                        # chan.pre_amplifier
                        if chan.data_logger is not None:
                            if chan.data_logger.type is not None:
                                description.append(chan.data_logger.type)
                            if chan.data_logger.resource_id is not None:
                                description.append(chan.data_logger.resource_id)
                        if len(description) > 0:
                            desc = zu.string_maxbytes(" ".join(description), 50)

                        if chan.start_date is not None:
                            time = obspy_datetime2epoch(chan.start_date)
                            ondate = stock.yearday(time)
                        else:
                            time = time_NULL
                            ondate = ondate_NULL
                        if chan.end_date is not None:
                            endtime = obspy_datetime2epoch(chan.end_date)
                            offdate = stock.yearday(endtime)
                        else:
                            endtime = endtime_NULL
                            offdate = offdate_NULL
                        if level == "response":
                            pass
                        # add instrument and such
                        else:
                            inid = inid_NULL

                        # add, then retrieve chanid
                        dbsitechan = db.lookup(table="sitechan")
                        try:
                            recno = dbsitechan.addv(
                                ("sta", css_sta),
                                ("chan", css_chan),
                                ("ondate", ondate),
                                ("offdate", offdate),
                                ("ctype", "n"),
                                # ("edepth", 0.0),
                                ("hang", az),
                                ("vang", dip),
                                ("descrip", desc),
                            )
                        except Exception as msg1:
                            print("here we have msg1:", msg1)
                            # brute force
                            chanid = dbsitechan.nextid("chanid")
                            recno = dbsitechan.addnull()
                            dbsitechan.record = recno
                            try:
                                dbsitechan.putv(
                                    ("sta", css_sta),
                                    ("chan", css_chan),
                                    ("ondate", ondate),
                                    ("chanid", chanid),
                                    ("offdate", offdate),
                                    ("ctype", "n"),
                                    # ("edepth", 0.0),
                                    ("hang", az),
                                    ("vang", dip),
                                    ("descrip", desc),
                                )
                            except Exception as msg2:
                                print(
                                    "%s %s (%s::%s):problem forcing to sitechan:%s"
                                    % (css_sta, css_chan, ondate, offdate, msg2)
                                )
                                chanid = chanid_NULL
                            else:
                                dbsitechan.record = recno
                                [chanid] = dbsitechan.getv("chanid")
                        else:
                            dbsitechan.record = recno
                            [chanid] = dbsitechan.getv("chanid")

                        try:
                            recno = dbsensor.addv(
                                ("sta", css_sta),
                                ("chan", css_chan),
                                ("time", time),
                                ("endtime", endtime),
                                ("inid", inid_NULL),
                                ("chanid", chanid),
                            )
                        except Exception as msg3:
                            print(msg3)

                            # need to add instrument
                            # otherwise use bogus instrument (inid -1)


def catalog2db(catalog, db, params):
    dbevent = db.lookup(table="event")
    verbose = False
    debug = False
    if "verbose" in params:
        verbose = params["verbose"]
    if "debug" in params:
        debug = params["debug"]
    for event in catalog.events:
        if debug:
            print(event)
        evid = dbevent.nextid("evid")
        creation_info = event.creation_info
        evauth = creation_info.author
        dbevent = db.lookup(table="event")
        dborigin = db.lookup(table="origin")
        dbarrival = db.lookup(table="arrival")
        dbassoc = db.lookup(table="assoc")
        arids = {}
        stas = {}

        for arrival in event.picks:
            if debug:
                print(arrival)
            atime = obspy_datetime2epoch(arrival.time)
            aiphase = arrival.phase_hint
            wfid = arrival.waveform_id
            sta = wfid.station_code
            chan = wfid.channel_code
            sta, chan = fix_stachan(sta, chan, params)
            res_id = arrival.resource_id  # arrival id internal to ObsPy
            arid = db.nextid("arid")
            arrival_record = dbarrival.addnull()
            dbarrival.record = arrival_record
            try:
                dbarrival.putv(
                    ("arid", arid),
                    ("sta", sta),
                    ("time", atime),
                    ("chan", chan),
                    ("iphase", aiphase),
                )
            except Exception as e:
                print("problem adding arrival:%s" % e)
                # sys.exit(1) maybe continue is ok. Late we check for arids, therefore no problem if sth. not found
                continue
            arids[res_id] = arid  # keep arid for the assoc table
            stas[res_id] = sta  # keep sta for the assoc table

        magids = {}
        mb = -999.00
        ml = -999.00
        ms = -999.00
        mbid = -1
        mlid = -1
        msid = -1

        netmag_records = []
        dbnetmag = db.lookup(table="netmag")
        for magnitude in event.magnitudes:
            magid = dbnetmag.nextid("magid")
            mmagnitude = magnitude.mag
            mmtype = magnitude.magnitude_type
            lt = mmtype.lower()
            if lt == "mb":
                mb = mmagnitude
                mbid = magid
            elif lt == "ml":
                ml = mmagnitude
                mlid = magid
            elif lt == "ms":
                ms = mmagnitude
                msid = magid

            mid = magnitude.resource_id
            mkeys = magnitude.keys()
            if "mag_errors" in mkeys:
                merrs = magnitude.mag_errors

            mnsta = -1
            if "station_count" in mkeys:
                if magnitude.station_count is not None:
                    mnsta = magnitude.station_count

            netmag_record = dbnetmag.addnull()
            dbnetmag.record = netmag_record
            netmag_records.append(netmag_record)
            dbnetmag.putv(
                ("magid", magid),
                ("evid", evid),
                ("magtype", mmtype),
                ("magnitude", mmagnitude),
                ("nsta", mnsta),
            )

        for origin in event.origins:
            otime = obspy_datetime2epoch(origin.time)
            olat = origin.latitude
            olon = origin.longitude
            if "depth" in origin:
                odepth = origin.depth / 1000.0
            nass = 0
            ndef = 0
            orid = dborigin.nextid("orid")
            for netmag_record in netmag_records:
                dbnetmag.record = netmag_record
                dbnetmag.putv(("orid", orid))

            # ObsPy and datascope have different database models,
            # here we need to follow ObsPy's logic
            for assoc in origin.arrivals:
                pick_id = assoc.pick_id
                if pick_id in arids:
                    arid = arids[pick_id]
                    sta = stas[pick_id]
                    adist = -1.000
                    if assoc.distance is not None:
                        adist = assoc.distance
                    aphase = assoc.phase
                    seaz = assoc.azimuth
                    atres = -999.00
                    if assoc.time_residual is not None:
                        atres = assoc.time_residual
                    aweight = -1.000
                    if assoc.time_residual is not None:
                        aweight = assoc.time_weight

                    nass += 1
                    timedef = "n"
                    if aweight is not None:
                        if aweight > 0.0:
                            timedef = "d"
                            ndef += 1

                    assoc_record = dbassoc.addnull()
                    dbassoc.record = assoc_record
                    dbassoc.putv(
                        ("orid", orid),
                        ("arid", arid),
                        ("phase", aphase),
                        ("delta", adist),
                        ("timeres", atres),
                        ("timedef", timedef),
                        ("wgt", aweight),
                    )
                else:
                    if verbose:
                        print("pick %s not found" % pick_id)
                    arid = -1

                # adelta=assoc.
                # iphase=assoc.

            # evid=db.nextid("evid")
            origin_record = dborigin.addnull()
            dborigin.record = origin_record
            dborigin.putv(
                ("orid", orid),
                ("evid", evid),
                ("time", otime),
                ("lat", olat),
                ("lon", olon),
                ("depth", odepth),
                ("nass", nass),
                ("ndef", ndef),
                ("mb", mb),
                ("mbid", mbid),
                ("ml", ml),
                ("mlid", mlid),
                ("ms", ms),
                ("msid", msid),
            )
        evid_record = dbevent.addv(("evid", evid), ("prefor", orid))

    dbevent = db.lookup(table="event")
