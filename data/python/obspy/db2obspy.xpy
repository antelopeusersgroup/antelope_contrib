# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")
import zamg.missing as zm

import numpy as np

# Import ObsPy modules
import obspy as obspy
import obspy.imaging
from obspy.core.stream import Stream
from obspy.core.trace import Trace
from obspy.core.trace import Stats
from obspy.core.utcdatetime import UTCDateTime
from obspy.geodetics import FlinnEngdahl


from obspy.core.event import (
    Amplitude,
    Arrival,
    Axis,
    Catalog,
    Comment,
    ConfidenceEllipsoid,
    CreationInfo,
    DataUsed,
    Event,
    EventDescription,
    FocalMechanism,
    Magnitude,
    MomentTensor,
    NodalPlane,
    NodalPlanes,
    Origin,
    OriginQuality,
    OriginUncertainty,
    Pick,
    PrincipalAxes,
    QuantityError,
    ResourceIdentifier,
    StationMagnitude,
    Tensor,
    WaveformStreamID,
)


def main():
    elog.init()
    dbname = "/opt/antelope/data/db/demo/demo"
    dbname = "/net/oew2/oewdata/db/oew2"
    db = ds.dbopen(dbname, "r")
    wf = db.lookup(table="wfdisc")
    snet = db.lookup(table="snetsta")

    sta = "TRO"
    sta = "CONA"
    chan = "HHZ"
    timestr = "2022-08-29 00:00:00"
    etime = stock.str2epoch(timestr)

    t1 = stock.now()
    stream = stachan2stream(sta, chan, etime, etime + 86400.0, db)
    t2 = stock.now()
    print("it took %s to retrieve the waveforms" % stock.strtdelta(t2 - t1))
    print(stream)

    # print(db)
    t1 = stock.now()
    mycat = db2catalog(db, prefor_only=True)
    t2 = stock.now()
    print("it took %s to retrieve the catalog" % stock.strtdelta(t2 - t1))
    print(mycat)
    dbe = db.lookup(table="event")
    dbs = dbe.subset("evid == 1")
    mycat = db2catalog(dbs, prefor_only=True)
    print(mycat)

    xmlfile = "demo.xml"
    xmlfile = "stationxml.OE_CONA"
    # fig = stream[0].spectrogram(log=True)
    print("we have a figure")
    # fig = stream[0].plot()
    print("we have another figure")
    fig = ppsd(stream, sta, chan, xmlfile)
    print("we have another figure")


def ppsd(stream, sta: str, chan: str, stationXML_filename: str):
    from obspy.signal.spectral_estimation import PPSD

    inv = obspy.read_inventory(stationXML_filename)
    trace = stream.select(station=sta, channel=chan)[0]
    ppsd = PPSD(trace.stats, inv)
    ppsd.add(stream)
    print(ppsd.times_processed)
    ppsd.plot()


def db2catalog(db, prefor_only=True):
    """
    return ObsPy Catalog object from event data in antelope database

    input is either a subset containing event ids or a pointer to the database only.
    In the latter case, all events are returned
    """
    id_base = "smi:zamg.ac.at"  # smi stands for 'seismological meta-information'
    if db.table < 0:
        db = db.lookup(table="event")
    elif db.query(ds.dbTABLE_IS_VIEW):
        # find out if the view contains event ids
        try:
            db.record = 0
            [evid] = db.getv("evid")
        except Exception as __:
            elog.notify("problem retrieving event id")
            return None
    if db.record_count < 1:
        elog.notify("no events in input database")
        return None
    fe = FlinnEngdahl()

    dborigin = db.lookup(table="origin")
    orid_key = db.lookup(table="origin", record="dbSCRATCH")
    prefor_matcher = orid_key.matches(dborigin, "orid")
    dbnetmag = db.lookup(table="netmag")
    netmag_matcher = orid_key.matches(dbnetmag, "orid")

    now = stock.now()
    catalog = Catalog()
    with ds.freeing(db.sort("evid", unique=True)) as dbview:
        number_events = dbview.record_count
        this_id = "/".join(
            (id_base, "catalogue", "zamg", stock.epoch2str(now, "%Y-%m-%d-%H-%M"))
        )
        catalog.resource_id = ResourceIdentifier(id=this_id)
        catalog.description = "created from Antelope database"
        catalog.comments = ""
        catalog.creation_info = CreationInfo(creation_time=UTCDateTime())
        for event_record in dbview.iter_record():
            try:
                [evid, prefor, evname, ev_auth, commid] = event_record.getv(
                    "evid", "prefor", "evname", "auth", "commid"
                )
            except Exception as __:
                elog.complain("problem retrieving event metadata")
                next

            orid_key.putv(("orid", prefor))
            prefor_records = prefor_matcher()
            if len(prefor_records) > 0:
                dborigin.record = prefor_records[0]
                try:
                    [
                        p_orid,
                        p_time,
                        p_lat,
                        p_lon,
                        p_depth,
                        p_nass,
                        p_ndef,
                        p_auth,
                        p_commid,
                    ] = dborigin.getv(
                        "orid",
                        "time",
                        "lat",
                        "lon",
                        "depth",
                        "nass",
                        "ndef",
                        "auth",
                        "commid",
                    )
                except Exception as __:
                    elog.complain("problem retrieving origin metadata")
                    next

                event = Event()
                this_id = "/".join((id_base, "event", str(evid)))
                event.resource_id = ResourceIdentifier(id=this_id)

                # fe_region_name=stock.srname(p_lat, p_lon) # only works on Linux
                fe_region_name = fe.get_region(p_lon, p_lat)
                description = EventDescription(type="region name", text=fe_region_name)
                event.event_descriptions.append(description)

                # fe_region_number=stock.grnumber(p_lat, p_lon)
                fe_region_number = fe.get_number(p_lon, p_lat)
                description = EventDescription(
                    type="Flinn-Engdahl region", text=fe_region_number
                )
                event.event_descriptions.append(description)

                origin = Origin()
                this_id = "/".join((id_base, "origin", str(p_orid)))
                origin.resource_id = ResourceIdentifier(id=this_id)
                origin.creation_info = CreationInfo()
                origin.latitude = p_lat
                origin.longitude = p_lon
                origin.depth = p_depth * 1000.0  # depth in meters
                origin.depth_type = "from location"
                origin.quality = OriginQuality()
                # origin.quality.associated_station_count = station_number
                # origin.quality.standard_error = standard_dev
                # associated_phase_count can be incremented in records 'P ' and 'S '
                origin.quality.associated_phase_count = p_nass
                # depth_phase_count can be incremented in record 'S '
                origin.quality.depth_phase_count = 0
                origin.origin_type = "hypocenter"
                origin.region = fe_region_name
                event.origins.append(origin)

                netmag_records = netmag_matcher()
                if len(netmag_records) > 0:
                    for netmag_record in netmag_records:
                        dbnetmag.record = netmag_record
                        mag = Magnitude()
                        [
                            magid,
                            magtype,
                            magnitude,
                            mag_nsta,
                            mag_uncertainty,
                            mag_auth,
                        ] = dbnetmag.getv(
                            "magid",
                            "magtype",
                            "magnitude",
                            "nsta",
                            "uncertainty",
                            "auth",
                        )

                        this = "/".join((id_base, "magnitude", str(evid), magtype))
                        mag.resource_id = ResourceIdentifier(id=this_id)
                        mag.creation_info = CreationInfo(agency_id="ZAMG-Vienna")
                        mag.mag = magnitude
                        mag.magnitude_type = magtype
                        mag.station_count = mag_nsta
                        mag.origin_id = origin.resource_id
                        event.magnitudes.append(mag)
                catalog.append(event)

    return catalog


def stachan2stream(sta: str, chan: str, time: float, endtime: float, db):
    """
    return ObsPy Stream containing wavforms
    """
    default_snet = "OE"
    default_fchan = ""

    stream = Stream()
    antelope_trace = db.trloadchan(time, endtime, sta, chan)
    with ds.trfreeing(antelope_trace):
        antelope_trace.trsplice()
        antelope_trace.trsplit()
        for antelope_trace.record in range(antelope_trace.record_count):
            trace = Trace()  # create empty trace object
            stats = Stats()  # for this object we need metadata as a stats object
            data = antelope_trace.trdata()
            [trace_start, t_net, t_sta, t_chan, t_samprate] = antelope_trace.getv(
                "time", "net", "sta", "chan", "samprate"
            )
            snet = db.lookup(table="snetsta")
            has_snetsta = snet.query(ds.dbTABLE_PRESENT)
            if has_snetsta:
                snet_match = db.lookup(table="snetsta", record="dbSCRATCH")
                snet_matcher = snet_match.matches(snet, kpattern="sta", tpattern="sta")
                snet_match.putv(("sta", t_sta))
                snet_records = snet_matcher()
                if len(snet_records) > 0:
                    snet.record = snet_records[0]
                    [snet, fsta] = snet.getv("snet", "fsta")
                else:
                    snet = default_snet
            schanloc = db.lookup(table="schanloc")
            has_schanloc = schanloc.query(ds.dbTABLE_PRESENT)
            if has_schanloc:
                fchan_match = db.lookup(table="schanloc", record="dbSCRATCH")
                fchan_matcher = fchan_match.matches(
                    schanloc, kpattern=["sta", "chan"], tpattern=["sta", "chan"]
                )
                fchan_match.putv(("sta", t_sta), ("chan", t_chan))
                schan_records = fchan_matcher()
                if len(schan_records) > 0:
                    schanloc.record = schan_records[0]
                    [fchan] = schanloc.getv("fchan")
                else:
                    fchan = default_fchan

            stats.network = snet  # this is bullshit, we should read the network from a schansta table
            stats.station = fsta
            stats.channel = fchan
            stats.sampling_rate = t_samprate
            stats.starttime = UTCDateTime(trace_start)

            dbname = db.query(ds.dbDATABASE_NAME)
            [calib, calper, segtype] = zm.dbget_calib(
                t_sta, t_chan, trace_start, dbname
            )
            stats.calib = calib
            trace.stats = stats
            trace.data = np.array(data)
            stream += trace
    return stream


if __name__ == "__main__":
    status = main()
    sys.exit(status)
