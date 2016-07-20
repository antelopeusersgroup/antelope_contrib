from os.path import splitext
from os import getpid
from time import sleep
import re
import logging
from multiprocessing import Manager, Process, Queue
from collections import deque
from Queue import Empty
from numpy import arange,\
                  array
from antelope.orb import Orb,\
                         OrbAfterError,\
                         OrbIncompleteException,\
                         OrbReapError,\
                         OrbSeekError,\
                         OrbSelectError
from antelope.Pkt import Packet,\
                         PktChannel,\
                         SrcName
from antelope.datascope import dbopen,\
                               Dbptr,\
                               dbTABLE_PRESENT,\
                               freeing,\
                               trdestroying,\
                               trnew
from antelope.stock import epoch2str,\
                           pfin,\
                           pfread,\
                           now

from ztools.shear.pick import shear_pick

logger = logging.getLogger(__name__)

class TraceAddError(Exception):
    def __init__(self):
        pass

class SourceInventory:
    """
    An object to keep inventory of packet source names on an ORB.
    """
    def __init__(self, input, type):
        if type == 'orb':
            self.type = 'orb'
            self.orb = input
        elif type == 'db':
            self.type = 'db'
            self.tbl_sitechan = input.lookup(table='sitechan')
            if not self.tbl_sitechan.query(dbTABLE_PRESENT):
                logger.error('No entries found in the sitechan table, '\
                        'please update database. Exiting...')
                exit(-1)
        self.inventory = {}

    def get_sources(self, net, sta, chan, time=None):
        if self.type == 'orb':
            if net not in self.inventory:
                self.inventory[net] = {}
            if sta not in self.inventory[net]:
                self.inventory[net][sta] = {}
                self.inventory[net][sta]['chans'] = []
                self.inventory[net][sta]['triplets'] = []
            if chan not in self.inventory[net][sta]['chans']:
                orb = Orb(self.orb)
                orb.connect()
#deal with simplex data
                nsrc = orb.select('{}_{}_{}.{}/.*'.format(net, sta, chan[:2], chan[3:]))
                if nsrc == 3:
                    triplet = []
                    pktid, srcname, pkttime, pktbuf = orb.reap()
                    pkt = Packet(srcname=srcname, time=pkttime, packet=pktbuf)
                    samprate = pkt.channels[0].samprate
                    for i in range(nsrc):
                        srcname = orb.sources()[1][i]['srcname']
                        my_chan = '_'.join(srcname.split('/')[0].split('_')[2:])
                        if my_chan not in self.inventory[net][sta]['chans']:
                            self.inventory[net][sta]['chans'] += [my_chan]
                            self.inventory[net][sta][my_chan] = (my_chan, srcname)
                            triplet += [my_chan]
                    triplet = tuple(sort_chans(triplet) + [False, samprate])
                    self.inventory[net][sta]['triplets'] += [triplet]
#deal with multiplexed data
#look for and unpack packets multiplexed by station
                elif orb.select('{}_{}/.*'.format(net, sta)) > 0:
                    triplet = []
                    sources = orb.sources()[1]
                    for i in range(len(sources)):
                        orb.select(sources[i]['srcname'])
                        pktid, srcname, pkttime, pktbuf = orb.reap()
                        pkt = Packet(srcname=srcname, time=pkttime, packet=pktbuf)
                        for pktchan in pkt.channels:
                            if pktchan.chan not in self.inventory[net][sta]['chans']\
                                    and pktchan.chan[:2] == chan[:2]\
                                    and pktchan.chan[3:] == chan[3:]:
                                self.inventory[net][sta]['chans'] += [pktchan.chan]
                                self.inventory[net][sta][pktchan.chan] = (pktchan.chan,
                                                                        srcname)
                                samprate = pktchan.samprate
                                triplet += [pktchan.chan]
                    if len(triplet) != 3:
                        logger.error("Could not find sources for all 3 components "\
                                "of waveform data for {}_{}_{}.{}".format(net,
                                                                        sta,
                                                                        chan[:2],
                                                                        chan[3:]))
                    else:
                        triplet = tuple(sort_chans(triplet) + [True, samprate])
                        self.inventory[net][sta]['triplets'] += [triplet]
#look for and unpack packets multiplexed by network
                elif orb.select('{}/.*'.format(net)) > 0:
                    triplet = []
                    sources = orb.sources()[1]
                    for i in range(len(sources)):
                        orb.select(sources[i]['srcname'])
                        pktid, srcname, pkttime, pktbuf = orb.reap()
                        pkt = Packet(srcname=srcname, time=pkttime, packet=pktbuf)
                        for pktchan in pkt.channels:
                            if pktchan.sta == sta and pktchan.chan not in\
                                    self.inventory[net][sta]['chans']\
                                    and pktchan.chan[:2] == chan[:2]\
                                    and pktchan.chan[3:] == chan[3:]:
                                self.inventory[net][sta]['chans'] += [pktchan.chan]
                                self.inventory[net][sta][pktchan.chan] = (pktchan.chan,
                                                                        srcname)
                                samprate = pktchan.samprate
                                triplet += [pktchan.chan]
                    if len(triplet) != 3:
                        logger.error("Could not find sources for all 3 components "\
                                "of waveform data for {}_{}_{}.{}".format(net,
                                                                        sta,
                                                                        chan[:2],
                                                                        chan[3:]))
                    else:
                        triplet = tuple(sort_chans(triplet) + [True, samprate])
                        self.inventory[net][sta]['triplets'] += [triplet]
                orb.close()
            sources = []
            for triplet in self.inventory[net][sta]['triplets']:
                if chan in triplet:
                    sources += [triplet[3], triplet[4]]
                    for i in range(3):
                        sources += [self.inventory[net][sta][triplet[i]]]
            if len(sources) == 0:
                logger.error("Could not find sources for {}_{}_{}.{}".format(net,
                                                                            sta,
                                                                            chan[:2],
                                                                            chan[3:]))
                return None
            return tuple(sources)
        elif self.type == 'db':
            if sta not in self.inventory:
                self.inventory[sta] = {}
            if chan not in self.inventory[sta]:
                self.inventory[sta] = {}
            for triplet in self.inventory[sta]:
                if chan in triplet:
                    for i in range(len(self.inventory[sta][triplet])):
                        if time > self.inventory[sta][triplet][i]['ondate'] and\
                                (time < self.inventory[sta][triplet][i]['offdate']\
                                or self.inventory[sta][triplet][i]['offdate'] == -1):
                            return triplet
            view = self.tbl_sitechan.subset("sta =~ /{}/ && chan =~ /{}.{}/ "\
                    "&& ondate < _{}_ && (offdate > _{}_ || "\
                    "offdate == -1)".format(sta,
                                            chan[:2],
                                            chan[3:],
                                            time,
                                            time))
            if view.record_count != 3:
                logger.error("Could not determine appropriate triplet for "\
                        "{}:{} at {}".format(sta,
                                                chan,
                                                epoch2str(time, "%Y%j %H:%M:%S.%s")))
                view.free()
                return None
            triplet, ondates, offdates = [], [], []
            for record in view.iter_record():
                chan, ondate, offdate = record.getv('chan', 'ondate', 'offdate')
                triplet += [chan]
                ondates += [ondate]
                offdates += [offdate]
            triplet = tuple(sort_chans(triplet))
            ondate = max(ondates)
            offdate = min(offdates)
            if triplet not in self.inventory[sta]:
                self.inventory[sta][triplet] = []
            self.inventory[sta][triplet] += [{'ondate': ondate,
                                                'offdate': offdate}]
            return triplet

class DataBackEndObject:
    """
    An object to abstract the back end data storage architecture.

    This object allows client code to interface with ORBs and databases
    with identical syntax.
    """
    def __init__(self, wf_src, det_src):
        """
        Constructor requires a waveform source [wf_src] and a detection
        source [det_src].

        wf_src and det_src can each be either a path to a database or
        an ORB specified as $HOST:$PORT.
        """
        global pfile
        self.wf_src = wf_src
        self.det_src = det_src
        wf_match = re.match(re.compile(".*:.*"), wf_src)
        det_match = re.match(re.compile(".*:.*"), det_src)
        if wf_match:
            self.wf_src_type = 'orb'
            self.source_inventory = SourceInventory(wf_src, type='orb')
        else:
            self.wf_src_type = 'db'
            self.wf_db = dbopen(wf_src, 'r+' if wf_src == det_src else 'r')
            self.source_inventory = SourceInventory(self.wf_db, type='db')
        if det_match:
            self.det_src_type = 'orb'
            self.det_orb_in = Orb(det_src)
            self.det_orb_in.connect()
            self.det_orb_in.select('/db/detection')
            self.det_orb_in.position('newest')
            self.det_orb_out = Orb(det_src)
            self.det_orb_out.connect()
        else:
            self.det_src_type = 'db'
            self.det_db = self.wf_db if wf_src == det_src else dbopen(det_src, 'r+')
            self.tbl_detection_out = self.det_db.lookup(table='detection')
            self.tbl_detection_in = self.tbl_detection_out.subset(
                    "state =~ /{}/".format(pfile['p_state']))
            self.tbl_detection_in = self.tbl_detection_in.sort("time")

    def __enter__(self):
        """
        Allows for use with context managers.
        """
        return self

    def __exit__(self, type, value, traceback):
        """
        Ensures proper clean-up upon exiting from a context manager
        block.
        """
        if self.wf_src_type == 'orb':
            self.wf_orb.close()
        elif self.wf_src_type == 'db':
            self.wf_db.close()
        if self.det_src_type == 'orb':
            self.det_orb_in.close()
            self.det_orb_out.close()
        elif self.det_src_type == 'db' and self.det_src != self.wf_src:
            self.det_db.close()

    def get_next_detection(self):
        """
        Return the next P-wave detection from the back end detection
        source.
        """
        if self.det_src_type == 'orb':
            try:
                while True:
                    try:
                        pktbuf = self.det_orb_in.reap()[3]
                    except OrbReapError:
                        #logger.error("OrbReapError - check status of ORB.")
                        raise
                    det = Detection
                    return Detection(pktbuf)
            except OrbIncompleteException:
                return None
        elif self.det_src_type == 'db':
            if self.tbl_detection_in.record < 0:
                self.tbl_detection_in.record = 0
            else:
                self.tbl_detection_in.record += 1
            if self.tbl_detection_in.record == self.tbl_detection_in.record_count:
                return None
            else:
                srcid, tagname, sta, chan, time,\
                arid, state, filter, snr = self.tbl_detection_in.getv('srcid',
                                                                      'tagname',
                                                                      'sta',
                                                                      'chan',
                                                                      'time',
                                                                      'arid',
                                                                      'state',
                                                                      'filter',
                                                                      'snr')
                return Detection(srcid=srcid,
                                 tagname=tagname,
                                 sta=sta,
                                 chan=chan,
                                 time=time,
                                 arid=arid,
                                 state=state,
                                 filter=filter,
                                 snr=snr)

    def get_wfdata(self, detection):
        """
        Return the three-component waveforms, for a time window around
        the given detection, from the back end waveform source.
        """
        global pfile
        function_start_time = now()
        timeout=20
        net = detection.srcid[:2]
        sta = detection.sta
        chan = detection.chan
        time = detection.time
        logger.debug("Retrieving waveform data for detection on {}:{} at "\
                "{}".format(sta, chan, epoch2str(time, "%Y%j %H:%M:%S.%s")))
        if self.wf_src_type == 'orb':
            wf_orb = Orb(self.wf_src)
            wf_orb.connect()
            try:
                wf_orb.seek(wf_orb.after(time\
                        - pfile['start_lead'] - 2 * pfile['max_packet_length']))
            except (OrbAfterError, OrbSeekError) as err:
                logger.debug("Failed to retrieve data for detection on {}:{} "\
                        "at {}\n\t{}".format(sta,
                                           chan,
                                           epoch2str(time, "%Y%j %H:%M:%S.%s"),
                                           err))
                wf_orb.close()
                return None
            sources = self.source_inventory.get_sources(net,
                                                      sta,
                                                      chan)
            if sources == None:
                wf_orb.close()
                logger.debug("Failed to retrieve data for detection on {}:{} "\
                        "at {}\n\tNo sources.".format(sta,
                                                     chan,
                                                     epoch2str(time, "%Y%j %H:%M:%S.%s")))
                return None
            else:
                multiplexed, samprate, src1, src2, src3 = sources
            tr3c = Trace3C(sta,
                           src1[0],
                           src2[0],
                           src3[0],
                           time - pfile['start_lead'],
                           time + pfile['end_lag'],
                           samprate)
            if not multiplexed:
                try:
                    wf_orb.select('{}|{}|{}'.format(src1[1], src2[1], src3[1]))
                except OrbSelectError as err:
                    logger.debug("Failed to retrieve data for detection on {}:{} "\
                            "at {}\n\t{}".format(sta,
                                            chan,
                                            epoch2str(time, "%Y%j %H:%M:%S.%s"),
                                            err))
                    wf_orb.close()
                    return None
                while not tr3c.is_full:
                    if now() - function_start_time > timeout:
                        logger.debug("Failed to retrieve data for detection on {}:{} "\
                                "at {}\n\tData retrieval timed out.".format(sta,
                                                                            chan,
                                                                            epoch2str(time, "%Y%j %H:%M:%S.%s")))
                        wf_orb.close()
                        return None
                    try:
                        pktid, srcname, pkttime, pktbuf = wf_orb.reap(timeout=timeout)
                    except OrbIncompleteException as err:
                        logger.debug("Failed to retrieve data for detection on {}:{} "\
                                "at {}\n\tWaveform packet reaping timed "\
                                "out.\n\t{}".format(sta,
                                                    chan,
                                                    epoch2str(time, "%Y%j %H:%M:%S.%s"),
                                                    err))
                        wf_orb.close()
                        return None
                    pkt = Packet(srcname=srcname, time=pkttime, packet=pktbuf)
                    for pktchan in pkt.channels:
                        tr3c += pktchan
            else:
                try:
                    wf_orb.select('{}'.format(src1[1]))
                except OrbSelectError as err:
                    logger.debug("Failed to retrieve data for detection on "\
                            "{}:{} at {}\n\t{}".format(sta,
                                                       chan,
                                                       epoch2str(time, "%Y%j %H:%M:%S.%s"),
                                                       err))
                    wf_orb.close()
                    return None
                while not tr3c.is_full:
                    if now() - function_start_time > timeout:
                        logger.debug("Failed to retrieve data for detection on {}:{} "\
                                "at {}\n\tData retrieval timed out.".format(sta,
                                                                            chan,
                                                                            epoch2str(time, "%Y%j %H:%M:%S.%s")))
                        wf_orb.close()
                        return None
                    try:
                        pktid, srcname, pkttime, pktbuf = wf_orb.reap(timeout=timeout)
                    except OrbIncompleteException as err:
                        logger.debug("Failed to retrieve data for detection on {}:{} "\
                                "at {}\n\tWaveform packet reaping timed "\
                                "out.\n\t{}".format(sta,
                                                    chan,
                                                    epoch2str(time, "%Y%j %H:%M:%S.%s"),
                                                    err))
                        wf_orb.close()
                        return None
                    pkt = Packet(srcname=srcname, time=pkttime, packet=pktbuf)
                    for pktchan in pkt.channels:
                        if pktchan.sta == sta and pktchan.chan[:2] == chan[:2]\
                                and pktchan.chan[3:] == chan[3:]:
                            tr3c += pktchan
            wf_orb.close()
            trs = []
            for my_tr in (tr3c.tr_z,
                            tr3c.tr_h1,
                            tr3c.tr_h2):
                tr_data = my_tr.tr
                with trdestroying(trnew('/tmp/test_tr')) as tr:
                    tr = tr.lookup(table='trace')
                    tr.record = tr.addnull()
                    tr.putv(('net', 'XX'),
                            ('sta', 'YYY'),
                            ('chan', 'ZZZ'),
                            ('time', my_tr.time),
                            ('endtime', my_tr.endtime),
                            ('nsamp', len(tr_data)),
                            ('samprate', my_tr.samprate))
                    tr.trputdata(tr_data)
                    tr.trfilter(pfile['filter'])
                    trs += [tr.trdata()]
            tr3c.tr_z.tr = array(trs[0])
            tr3c.tr_h1.tr = array(trs[1])
            tr3c.tr_h2.tr = array(trs[2])
            return tr3c
        elif self.wf_src_type == 'db':
            tstart = time - pfile['start_lead']
            tend = time + pfile['end_lag']
            traces = []
            logger.debug("Getting data for {} {} - {}".format(sta,
                                                              epoch2str(tstart,
                                                                        "%D %H:%M:%S.%s"),
                                                              epoch2str(tend,
                                                                        "%D %H:%M:%S.%s")))
            triplet = self.source_inventory.get_sources(net,
                                                        sta,
                                                        chan,
                                                        time)
            if triplet == None:
                return None
            for chan in triplet:
                logger.debug("Getting data for {}:{} {} - {}".format(sta,
                                                                     chan,
                                                                     epoch2str(tstart,
                                                                               "%D %H:%M:%S.%s"),
                                                                     epoch2str(tend,
                                                                               "%D %H:%M:%S.%s")))
                with trdestroying(self.wf_db.trloadchan(tstart, tend, sta, chan)) as tr:
                    if tr.record_count == 0:
                        logger.error("Could not load data for {}:{} "\
                                "{} - {}".format(sta,
                                                 chan,
                                                 epoch2str(tstart,
                                                           "%D %H:%M:%S.%s"),
                                                 epoch2str(tend,
                                                           "%D %H:%M:%S.%s")))
                        return None
                    tr.trfilter(pfile['filter'])
                    tr.record = 0
                    try:
                        time, samprate = tr.getv('time', 'samprate')
                    except DbgetvError:
                        logger.error("Could not get value 'samprate' for {}:{} "\
                                "{} - {}".format(sta,
                                                chan,
                                                epoch2str(tstart, "%D %H:%M:%S.%s"),
                                                epoch2str(tend, "%D %H:%M:%S.%s")))
                        return None
                    data = []
                    for segment in tr.iter_record():
                        tmp_data = list(segment.trdata())
                        data += list(segment.trdata())
                    tr.trfree()
                data = array(data)
                traces += [Trace(data, sta, chan, time, samprate)]
            tr3c = Trace3C(sta,
                           triplet[0],
                           triplet[1],
                           triplet[2],
                           time - pfile['start_lead'],
                           time + pfile['end_lag'],
                           traces[0].samprate)
            if traces[0].nsamp != traces[1].nsamp or\
                    traces[0].nsamp != traces[2].nsamp:
                return None
            tr3c += traces[0]
            tr3c += traces[1]
            tr3c += traces[2]
            return tr3c

    def write_detection(self, detection):
        """
        Write a detection to the back end detection source.
        """
        global pfile
        logger.debug("Writing S-wave detection on {}:{} at "\
                "{}.".format(detection.sta,
                             detection.chan,
                             epoch2str(detection.time, "%Y%j %H:%M:%S.%s")))
        if self.det_src_type == 'orb':
            srcname = SrcName(srcname_string='/db/detection')
            if pfile['schema'] == 'css3.1':
                pktbuf = "\x01css3.1\x00{:64} {:8} {:14} {:15} {:18.6f} {:12d} {:3} "\
                        "{:30} {:10.5g} {:17.5f}\n\x00".format(detection.srcid,
                                                            detection.tagname,
                                                            detection.sta,
                                                            detection.chan,
                                                            detection.time,
                                                            detection.arid,
                                                            detection.state,
                                                            detection.filter,
                                                            detection.snr,
                                                            now())
            elif pfile['schema'] == 'css3.0':
                pktbuf = "\x01css3.0\x00{:64} {:8} {:6} {:8} {:17.5f} {:8d} {:3} "\
                        "{:30} {:10.5g} {:17.5f}\n\x00".format(detection.srcid,
                                                            detection.tagname,
                                                            detection.sta,
                                                            detection.chan,
                                                            detection.time,
                                                            detection.arid,
                                                            detection.state,
                                                            detection.filter,
                                                            detection.snr,
                                                            now())
            self.det_orb_out.put(srcname='/db/detection', time=now(), packet=pktbuf)
        elif self.det_src_type == 'db':
            self.tbl_detection_out.record = self.tbl_detection_out.addnull()
            self.tbl_detection_out.putv(('srcid', detection.srcid),
                                        ('tagname', detection.tagname),
                                        ('sta', detection.sta),
                                        ('chan', detection.chan),
                                        ('time', detection.time),
                                        ('arid', detection.arid),
                                        ('state', detection.state),
                                        ('filter', detection.filter),
                                        ('snr', detection.snr))

class Detection:
    """
    A class to hold detection parametric data.
    """
    def __init__(self, *args, **kwargs):
        """
        Constructor requires either a packet buffer from an ORB
        or explicit keyword arguments.
        """
        if len(args) == 1 and type(args[0]) == str:
            args = self.parse_pktbuf(args[0])
            self.srcid = args[0]
            self.tagname = args[1]
            self.sta = args[2]
            self.chan = args[3]
            self.time = args[4]
            self.arid = args[5]
            self.state = args[6]
            self.filter= args[7]
            self.snr = args[8]
            self.lddate = args[9]
        else:
            if 'srcid' in kwargs:
                self.srcid = kwargs['srcid']
            else:
                self.srcid = '-'
            if 'tagname' in kwargs:
                self.tagname = kwargs['tagname']
            else:
                self.tagname = '-'
            if 'sta' in kwargs:
                self.sta = kwargs['sta']
            else:
                self.sta = '-'
            if 'chan' in kwargs:
                self.chan = kwargs['chan']
            else:
                self.chan = '-'
            if 'time' in kwargs:
                self.time = kwargs['time']
            else:
                self.time = -9999999999.999000
            if 'arid' in kwargs:
                self.arid = kwargs['arid']
            else:
                self.arid = -1
            if 'state' in kwargs:
                self.state = kwargs['state']
            else:
                self.state = '-'
            if 'filter' in kwargs:
                self.filter = kwargs['filter']
            else:
                self.filter = '-'
            if 'snr' in kwargs:
                self.snr = kwargs['snr']
            else:
                self.snr = -1.00
            if 'lddate' in kwargs:
                self.lddate = kwargs['lddate']

    def parse_pktbuf(self, pktbuf):
        """
        Parse fields from a packet buffer in either css3.0 or css3.1
        format.
        """
        global pfile
        if pfile['schema'] == 'css3.0':
            pktbuf = pktbuf.split('\x00')[1]
            srcid = pktbuf[:64].rstrip()
            tagname = pktbuf[65:73].rstrip()
            sta = pktbuf[74:80].rstrip()
            chan = pktbuf[81:89].rstrip()
            time = float(pktbuf[90:107])
            arid = int(pktbuf[108:116])
            state = pktbuf[117:120].rstrip()
            filter= pktbuf[121:151].rstrip()
            snr = float(pktbuf[152:162])
            lddate = float(pktbuf[163:180])
        elif pfile['schema'] == 'css3.1':
            pktbuf = pktbuf.split('\x00')[1]
            srcid = pktbuf[:64].rstrip()
            tagname = pktbuf[65:73].rstrip()
            sta = pktbuf[74:88].rstrip()
            chan = pktbuf[89:104].rstrip()
            time = float(pktbuf[105:123])
            arid = int(pktbuf[124:136])
            state = pktbuf[137:140].rstrip()
            filter= pktbuf[141:171].rstrip()
            snr = float(pktbuf[172:182])
            lddate = float(pktbuf[183:200])
        return (srcid, tagname, sta, chan, time, arid, state, filter, snr, lddate)

class Trace:
    """
    A class to hold trace data and appropriate metadata.
    """
    def __init__(self, tr, sta, chan, time, samprate):
        self.tr = tr
        self.sta = sta
        self.chan = chan
        self.time = time
        self.samprate = samprate
        self.nsamp = len(self.tr)
        self.dt = 1.0 / samprate
        self.endtime = time + self.dt * (self.nsamp - 1)

    def trim(self, n):
        self.tr = self.tr[:n]
        self.nsamp = len(self.tr)
        self.endtime = self.time + self.det * (self.nsamp - 1)

class EmptyTrace(Trace):
    """
    A sub-class of the Trace class that can be initialized with null
    data.
    """
    def __init__(self, sta, chan, time, endtime, samprate):
        tr = [None for i in range(int((endtime - time) * samprate))]
        self.is_full = False
        self.percent_full = 0.0
        Trace.__init__(self,
                       tr,
                       sta,
                       chan,
                       time,
                       samprate)

    def __add__(self, pktchan):
        """
        Method to allow use of the + operator to fill an EmptyTrace object
        with trace data as PktChannel object are acquired from an ORB.
        """
        if pktchan.sta != self.sta:
            raise TraceAddError()
        if pktchan.chan != self.chan:
            raise TraceAddError()
        if pktchan.samprate != self.samprate:
            raise TraceAddError()
        pktchan.endtime = pktchan.time + (pktchan.nsamp - 1) * (1. / pktchan.samprate)
        if pktchan.endtime >= self.time and pktchan.time <= self.endtime:
            if pktchan.time <= self.time:
                istart = 0
                nsamp_chop = int(round((self.time - pktchan.time) / self.dt))
                tr = pktchan.data[nsamp_chop:]
                self.time = pktchan.time + nsamp_chop * self.dt
            elif pktchan.time > self.time:
                istart = int(round((pktchan.time - self.time) / self.dt))
                tr = pktchan.data
            if pktchan.endtime > self.endtime:
                iend = self.nsamp
                nsamp_chop = int(round((pktchan.endtime - self.endtime) / self.dt))
                tr = tr[:-nsamp_chop]
            elif pktchan.endtime == self.endtime:
                iend = self.nsamp
            elif pktchan.endtime < self.endtime:
                iend = self.nsamp - int(round((self.endtime - pktchan.endtime) / self.dt))
            self.tr[istart:iend] = tr
        if None not in self.tr:
            self.is_full = True
        self.percent_full = 100 *\
                float(len([d for d in self.tr if d != None])) / len(self.tr)
        return self

class Trace3C():
    """
    A class to hold three component trace data.
    """
    def __init__(self, sta, chan_z, chan_h1, chan_h2, time, endtime, samprate):
        """
        Constructor requires station name, 3 channel names, start and
        end time of trace data and sample rate.

        Constructor initializes three EmptyTrace objects which are
        subsequently filled as data is reaped from an ORB.
        """
        self.sta = sta
        self.time = time
        self.endtime = endtime
        self.samprate = samprate
        self.tr_z = EmptyTrace(sta, chan_z, time, endtime, samprate)
        self.tr_h1 = EmptyTrace(sta, chan_h1, time, endtime, samprate)
        self.tr_h2 = EmptyTrace(sta, chan_h2, time, endtime, samprate)
        self.is_full = False

    def __add__(self, dataobj):
        """
        Method to allow use of + operator to fill Trace3C object by
        adding PktChannel objects.
        """
        if isinstance(dataobj, PktChannel):
            if dataobj.sta != self.sta:
                return self
            if dataobj.chan != self.tr_z.chan and\
                    dataobj.chan != self.tr_h1.chan and\
                    dataobj.chan != self.tr_h2.chan:
                return self
            if dataobj.samprate != self.samprate:
                return self
            if dataobj.chan == self.tr_z.chan:
                self.tr_z += dataobj
            elif dataobj.chan == self.tr_h1.chan:
                self.tr_h1 += dataobj
            elif dataobj.chan == self.tr_h2.chan:
                self.tr_h2 += dataobj
            if self.tr_z.is_full and self.tr_h1.is_full and self.tr_h2.is_full:
                self.is_full = True
                self.tr_z.tr = array(self.tr_z.tr)
                self.tr_h1.tr = array(self.tr_h1.tr)
                self.tr_h2.tr = array(self.tr_h2.tr)
        elif isinstance(dataobj, Trace):
            if dataobj.chan == self.tr_z.chan:
                self.tr_z = dataobj
            elif dataobj.chan == self.tr_h1.chan:
                self.tr_h1 = dataobj
            elif dataobj.chan == self.tr_h2.chan:
                self.tr_h2 = dataobj
        return self

    def plot(self, **kwargs):
        """
        Plotting three component trace data.
        """
        import matplotlib.pyplot as plt
        fig = plt.figure(figsize=(12, 8))
        ax1 = fig.add_subplot(3, 1, 1)
        ax2 = fig.add_subplot(3, 1, 2)
        ax3 = fig.add_subplot(3, 1, 3)
        t = [self.tr_z.time + i * self.tr_z.dt for i in range(self.tr_z.nsamp)]
        ax1.plot(t, self.tr_z.tr)
        ax2.plot(t, self.tr_h1.tr)
        ax3.plot(t, self.tr_h2.tr)
        if 'picks' in kwargs:
            for pick in kwargs['picks']:
                if pick[1] == 0:
                    ax = ax1
                    ymin, ymax = min(self.tr_z.tr), max(self.tr_z.tr)
                elif pick[1] == 1:
                    ax = ax2
                    ymin, ymax = min(self.tr_h1.tr), max(self.tr_h1.tr)
                elif pick[1] == 2:
                    ax = ax3
                    ymin, ymax = min(self.tr_h2.tr), max(self.tr_h2.tr)
                ax.vlines(pick[0], ymin, ymax, color='r', linewidth=1.5)
        ymin, ymax = min(self.tr_z.tr), max(self.tr_z.tr)
        ax1.set_ylim(ymin - 0.05 * abs(ymax - ymin),
                     ymax + 0.05 * abs(ymax - ymin))
        ymin, ymax = min(self.tr_h1.tr), max(self.tr_h1.tr)
        ax2.set_ylim(ymin - 0.05 * abs(ymax - ymin),
                     ymax + 0.05 * abs(ymax - ymin))
        ymin, ymax = min(self.tr_h2.tr), max(self.tr_h2.tr)
        ax3.set_ylim(ymin - 0.05 * abs(ymax - ymin),
                     ymax + 0.05 * abs(ymax - ymin))
        for ax in (ax1, ax2, ax3):
            ax.set_xlim(t[0], t[-1])
            ax.set_xticks(arange(t[0], t[-1] + 5, 5))
            ax.set_xticklabels(arange(0, t[-1] - t[0] + 5, 5))
        if 'dir' in kwargs:
            logger.debug('Saving plot to {}/{}_{}.png'.format(kwargs['dir'],
                                                              self.sta,
                                                              int(self.time)))
            plt.savefig('{}/{}_{}.png'.format(kwargs['dir'],
                                              self.sta,
                                              int(self.time)),
                        format='png')
        else:
            plt.show()
        plt.close()

def sort_chans(chans):
    """
    A utility function to sort channel codes into Z, N, E or Z, 1, 2
    order.
    """
    sorted_chans = []
    for chan in chans:
        if chan[2] == "Z":
            sorted_chans += [chan]
    for chan in chans:
        if chan[2] == "N" or chan[2] == "1":
            sorted_chans += [chan]
    for chan in chans:
        if chan[2] == "E" or chan[2] == "2":
            sorted_chans += [chan]
    return sorted_chans

def configure_logging(logfile, level):
    """
    A utility function to configure logging functionality.
    """
    if level.upper() == "INFO":
        level = logging.INFO
    elif level.upper() == "DEBUG":
        level = logging.DEBUG
    for name in (__name__, ):
        logger = logging.getLogger(name)
        logger.setLevel(level)
        if level == logging.DEBUG:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                    '%(name)s::%(funcName)s()::%(process)d:: %(message)s',
                    datefmt='%Y%j %H:%M:%S')
        else:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                    ' %(message)s',
                    datefmt='%Y%j %H:%M:%S')
        if logfile:
            file_handler = logging.FileHandler(logfile)
            file_handler.setLevel(level)
            file_handler.setFormatter(formatter)
            logger.addHandler(file_handler)
        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(level)
        stream_handler.setFormatter(formatter)
        logger.addHandler(stream_handler)

def detection_reaper_thread(data_beo):
    """
    A threadable function to reap P-wave detections from the detection
    source back end.

    Reaped detections are placed onto a Queue.
    """
    global input_q, pfile
    while True:
        det = data_beo.get_next_detection()
        if det == None:
            for i in range(pfile['n_detection_threads']):
                input_q.put(None, True)
            return
        else:
            if det.state != pfile['p_state']:
                continue
            input_q.put(det, True)
            logger.debug("P-wave detection on {}:{} at {} added to "\
                    "processing queue.".format(det.sta,
                                              det.chan,
                                              epoch2str(det.time, "%Y%j %H:%M:%S.%s")))

def shear_wave_detector_thread(data_beo, terminate):
    """
    A threadable function to run shear wave detector as P-wave
    detections are reaped from P-wave detection Queue. Resultant S-wave
    detections are placed onto another Queue.
    """
    global input_q, output_q, pfile
    logger.debug("Spawning shear_wave_detector_thread")
    pid = getpid()
    terminate[pid] = False
    while True:
        if terminate[pid] == True:
            del terminate[pid]
            logger.info("Killing shear_wave_detector_thread.")
            return
        try:
            p_det = input_q.get()
        except Empty:
            continue
        if p_det == None:
            output_q.put(None, True)
            logger.info("Killing shear_wave_detector_thread.")
            return
        else:
            picks = [(p_det.time, 0)]
            logger.debug("Processing P-wave detection on {}:{} at "\
                    "{}.".format(p_det.sta,
                                 p_det.chan,
                                 epoch2str(p_det.time, "%Y%j %H:%M:%S.%s")))
            my_time = now()
            tr3c = data_beo.get_wfdata(p_det)
            my_time = now() - my_time
            logger.debug("Loading waveform data took {} seconds.".format(my_time))
            if tr3c == None:
                continue
            logger.debug("Waveform data retrieved for detection on {}:{} at "\
                    "{}.".format(p_det.sta,
                                 p_det.chan,
                                 epoch2str(p_det.time, "%Y%j %H:%M:%S.%s")))
            try:
                ptime, snr, chan_index = shear_pick(tr3c.tr_z.tr,
                                                    tr3c.tr_h1.tr,
                                                    tr3c.tr_h2.tr,
                                                    tr3c.tr_z.dt)
            except Exception as err:
                logger.debug("No S-wave detection found corresponding to "\
                        "P-wave detection on {}:{} at {}.\n\t{}".format(p_det.sta,
                                                                        p_det.chan,
                                                                        epoch2str(p_det.time, "%Y%j %H:%M:%S.%s"),
                                                                        err))
                continue
            if chan_index == 0:
                chan = tr3c.tr_h1.chan
            elif chan_index == 1:
                chan = tr3c.tr_h2.chan
            if ptime != None:
                ptime += p_det.time - pfile['start_lead']
                picks += [(ptime, chan_index + 1)]
                det = Detection(srcid='PLACEHOLDER',
                                sta=p_det.sta,
                                chan=chan,
                                time=ptime,
                                state=pfile['s_state'],
                                filter=pfile['filter'],
                                snr=snr)
                logger.debug("S-wave detection found on {}:{} at "\
                        "{}.".format(det.sta,
                                     det.chan,
                                     epoch2str(det.time, "%Y%j %H:%M:%S.%s")))
                output_q.put(det, True)
            else:
                logger.debug("No S-wave detection found corresponding to "\
                        "P-wave detection on {}:{} at {}.".format(p_det.sta,
                                                                  p_det.chan,
                                                                  epoch2str(p_det.time, "%Y%j %H:%M:%S.%s")))

def detection_output_thread(data_beo):
    """
    A threadable function to write S-wave detections from Queue to
    detection source back end.
    """
    global output_q, pfile
    none_count = 0
    while True:
        det = output_q.get(True)
        if det == None:
            none_count += 1
            if none_count == pfile['n_detection_threads']:
                return
        else:
            data_beo.write_detection(det)

def parse_args():
    """
    Parse command line arguments.
    """
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('input_src', type=str, help='input source (database '\
            'or ORB)')
    parser.add_argument('detection_src', type=str, nargs='?', help='optional '\
            'source for detections (database or ORB) if different from '\
            'waveform input source (input_src)')
    parser.add_argument('-p', '--pfile', type=str, help='parameter file')
    parser.add_argument('-l', '--logfile', type=str, help='log file')
    parser.add_argument('-v', '--verbose', action='store_true', help='verbose')
    return parser.parse_args()

def parse_pfile(pfile):
    """
    Parse parameter file.
    """
    if not pfile:
        pfile = pfread('dbshear')
    elif not os.path.isfile(pfile):
        pfile = pfread('dbshear')
    elif splitext(pfile)[1] == '.pf':
        pfile = pfin(pfile)
    else:
        pfile = pfin('%s.pf' % pfile)
    pfile = pfile.pf2dict()
    return eval_pfile(pfile)

def eval_pfile(pfile):
    """
    Appropriately type-cast paramters from parameter file.
    """
    def eval_element(element):
        try:
            element = eval(element)
        except (NameError, SyntaxError):
            pass
        return element
    for key in pfile:
        if key in locals():
            continue
        elif isinstance(pfile[key], dict):
            eval_pfile(pfile[key])
        elif isinstance(pfile[key], tuple):
            pfile[key] = list(pfile[key])
            for i in range(len(pfile[key])):
                if isinstance(pfile[key][i], dict):
                    pfile[key][i] = eval_pfile(pfile[key][i])
                else:
                    pfile[key][i] = eval_element(pfile[key][i])
        else:
            pfile[key] = eval_element(pfile[key])
    return pfile

def main_loop():
    """
    Main control loop.
    """
    global input_q, output_q, pfile
    input_q = Queue(maxsize=1)
    output_q = Queue(maxsize=1)
    args = parse_args()
    pfile = parse_pfile(args.pfile)
    manager = Manager()
    terminate = manager.dict()
    if not args.logfile:
        args.logfile = 'dbshear.log'
    if args.verbose:
        level = "DEBUG"
    else:
        level = "INFO"
    configure_logging(args.logfile, level)
    logger.info('Starting...')
    with DataBackEndObject(args.input_src,
                           args.detection_src if args.detection_src else\
                                   args.input_src) as data_beo:
        other_processes = []
        other_processes += [Process(target=detection_reaper_thread,
                                    args=(data_beo,))]
        other_processes += [Process(target=detection_output_thread, args=(data_beo,))]
        for process in other_processes:
            process.daemon = True
            process.start()
        det_procs = deque([])
        for i in range(pfile['n_detection_threads']):
            det_procs.append(Process(target=shear_wave_detector_thread,
                                        args=(data_beo, terminate)))
        for proc in det_procs:
            proc.start()
        while True:
            sleep(pfile['thread_respawn_time'])
            det_procs_alive = len([proc.is_alive() for proc in det_procs if proc.is_alive() == True])
            if det_procs_alive != pfile['n_detection_threads']:
                other_processes[1].join()
                break
            for i in range(pfile['n_detection_threads']):
                proc = det_procs.popleft()
                terminate[proc.pid] = True
                while proc.is_alive():
                    sleep(1)
                proc = Process(target=shear_wave_detector_thread,
                               args=(data_beo, terminate))
                proc.start()
                det_procs.append(proc)
    logger.info("Terminating...")

if __name__ == "__main__":
    main_loop()
