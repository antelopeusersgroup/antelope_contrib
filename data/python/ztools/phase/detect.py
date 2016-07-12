#########################################
# This is a module for earthquake       #
# detection                             #
#                                       #
# Version 1.5                           #
#                                       #
# Author: Zachary E. Ross (2014)        #
# Contact: zross@usc.edu                #
# Website: http://earth.usc.edu/~zross/ #
#                                       #
# Edited: Malcolm White                 #
# Contact: mcwhite@ucsd.edu             #
# Website: https://github.com/malcolmw  #
#########################################

# Standard library modules
from __future__ import division
import string
import logging

# 3rd party modules
import numpy as np
import matplotlib.pyplot as plt
import obspy.core as obc
import obspy.signal as obs
import antelope.datascope as ds
import antelope.stock as stock

# phase package modules
import ztools.phase.pick as pick
import ztools.phase.gen as gen
from os.path import splitext, isfile
from antelope.stock import pfin, pfread
logger = logging.getLogger(__name__)

#-------------------------------------------------------------
def parse_pfile(pfile):
    if not pfile:
        pfile = pfread('params')
    elif not isfile(pfile):
        pfile = pfread('params')
    elif splitext(pfile)[1] == '.pf':
        pfile = pfin(pfile)
    else:
        pfile = pfin('%s.pf' % pfile)
    pfile = pfile.pf2dict()
    return eval_pfile(pfile)

#-------------------------------------------------------------
def eval_pfile(pfile):
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

#-------------------------------------------------------------
def associate(stream, t_sum, sta, trig_on, trig_off, peak_min,
              start_end=None, trig_ext=0, dur_min=0):
    """
    Modified coincidence  algorithm with general method
    following obs.coincidenceTrigger. See obs documentation for explanation
    stream: stream (list) of ppts
    t_sum: coincidence sum necessary for detection (e.g. # of stations)
    sta: list of station names
    trig_on: ppts trigger on value
    trig_off: ppts trigger off value
    peak_min: minimum value for peak of trigger window to reach
    """
    s = []
    start = []
    end = []
    trace_ids = [tr.id for tr in stream]
    if isinstance(trace_ids, list) or isinstance(trace_ids, tuple):
        trace_ids = dict.fromkeys(trace_ids, 1)

    z = 0
    for tr, st in zip(stream, sta):
        if start_end != None:
            start.append(int(start_end[0]*tr.stats.sampling_rate))
            end.append(tr.stats.npts - int(start_end[1]*tr.stats.sampling_rate))
        else:
            start.append(0)
            end.append(tr.stats.npts)
        on = trig_on
        off = trig_off
        triggers = obs.trigger.triggerOnset(tr.data, on, off)

        for on, off in triggers:
            if on == off:
                continue
            if on < start[z]:
                continue
            peak = tr.data[on:off].max()
            peak_idx = np.argmax(tr.data[on:off]) / tr.stats.sampling_rate
            std = tr.data[on:off].std()
            on_time = tr.stats.starttime + float(on) / tr.stats.sampling_rate
            off_time = tr.stats.starttime + float(off) / tr.stats.sampling_rate
            dur = off_time.timestamp - on_time.timestamp

            if dur > dur_min:
                if peak > peak_min:
                    s.append((on_time.timestamp, off_time.timestamp,
                              st, peak, peak_idx, std))
        z += 1

    coincidence_s = []
    s.sort()
    last_off_time = 0.0
    while s != []:
        on, off, tr_id, peak, peak_idx, std = s.pop(0)
        event = {}
        event['time'] = {tr_id:obc.utcdatetime.UTCDateTime(on)}
        event['peak'] = {tr_id:peak_idx}
        event['stations'] = [tr_id]
        event['coincidence_sum'] = float(1)
        for i in xrange(len(s)):
            tmp_on, tmp_off, tmp_id, tmp_peak, tmp_peak_idx, tmp_std = s[i]
            if tmp_on <= off:
                if tmp_id not in event['stations']:
                    event['stations'].append(tmp_id)
                    event['time'].update({tmp_id:obc.utcdatetime.UTCDateTime(tmp_on)})
                    event['coincidence_sum'] += 1
                    event['peak'].update({tmp_id:tmp_peak_idx})
                off = max(off, tmp_off)
            else:
                del s[0:i]
                break
        if event['coincidence_sum'] < t_sum:
            continue
        if off == last_off_time:
            continue
        event['duration'] = off - on
        coincidence_s.append(event)
        last_off_time = off
    return coincidence_s

#-------------------------------------------------------------
def ecdf(T, N):
    """
    Calculates an empirical CDF for trace T. The ECDF is of length N.
    """
    xmin = np.min(T)
    xmax = np.max(T)
    x = np.linspace(xmin, xmax, N)
    T_sort = np.sort(T)
    f = np.searchsorted(T_sort, x)/T_sort.shape[0]
    return f, x

#-------------------------------------------------------------
def indicator(data, param, dt):
    """
    Generic detector function containing all detector options coded by 'type'
    'param' is a dictionary containing 'type' and all appropriate key words
    unique to a specific detector.
    """
    if param['type'] == 'recstalta':
        # sta, lta are window lengths in seconds
        out = obs.recSTALTA(data, int(param['sta']/dt), int(param['lta']/dt))

    return out

#-------------------------------------------------------------
def ppts(T, para, N, start=0, stop=None, ofile=None):
    """
    Calculates a product PPTS using a set of detectors.
    The parameters for each (presently) STA/LTA detector need to be
    provided with a list of dicts para which contains appropriate parameters
    see function 'indicator' above. Also needed is dt
    Inputs:
        T is an obc.trace object
        para: matrix of parameters, see above.
        N: Number of values to use for ECDF calculation
        start: starting index for window to calculate ECDF from
        stop: ending index for window to calculate ECDF from
        ofile: output filename for saving if desired
    """
    # Setting start/stop range for calculating PPTS
    if stop is None:
        stop = T.stats.npts - 1
    else:
        stop = T.stats.npts - 1 - int(stop*T.stats.sampling_rate)
    start = int(start*T.stats.sampling_rate)
    out = np.zeros(T.stats.npts)
    out[start:stop] = 1

    # Calculate joint ppts from parameters
    for i in xrange(len(para)):
        # Make sure upper corner isn't above Nyquist freq
        if para[i]['freq'][1] >= T.stats.sampling_rate/2.0:
            fmax = T.stats.sampling_rate/2.0
        else:
            fmax = float(para[i]['freq'][1])
        fmin = float(para[i]['freq'][0])
        # Bandpass filter data
        temp = T.copy()
        temp.filter('bandpass', freqmin=fmin, freqmax=fmax)
        # Calculate detector and transform to PPTS
        ind = indicator(temp.data, para[i], 1/T.stats.sampling_rate)[start:stop]
        f, x  = ecdf(ind, N)
        out[start:stop] *= f[np.searchsorted(x, ind)]

    if ofile is not None:
        np.save(ofile, out)
        return out
    else:
        return out

#-------------------------------------------------------------
def get_prefors(p_file, t_start, t_end, cmp=['N', 'E', 'Z']):
    """
    Reads in the path of a database and retrieves a dict with prefors as keys.
    Each key is mapped to an origin time of the particular event
    """
    posix = obc.utcdatetime.UTCDateTime(0)
    path = p_file['path']
    prefors = {}
    with ds.closing(ds.dbopen(path, 'r')) as db:
        # Ensure traces exist and read in data
        tbl_event = db.schema_tables['event']
        tbl_origin = db.schema_tables['origin']
        tbl_prefor = tbl_origin.join(tbl_event)
        tbl_subset = tbl_prefor.subset('orid==prefor && time >= %s && time <= %s' %
            (t_start - posix, t_end - posix))
        fdir = {}
        for rec in tbl_subset.iter_record():
            prefors[rec.getv('orid')[0]] = [rec.getv('time')[0],
                rec.getv('lat')[0], rec.getv('lon')[0], rec.getv('ml')[0]]
    return prefors

#-------------------------------------------------------------
def get_fdirs(p_file, t_start, t_end, cmp=['N', 'E', 'Z']):
    """
    Reads in the path of a database and retrieves a dict with stations as keys.
    Each key is mapped to a list of N, E, and Z files for that particular station
    """
    posix = obc.utcdatetime.UTCDateTime(0)
    chan_subset = p_file['chan_subset']
    path = p_file['path']
    if 'edge' in p_file:
        edge = p_file['edge']
    else:
        edge = 0
    with ds.closing(ds.dbopen(path, 'r')) as db:
        # Ensure traces exist and read in data
        tbl_wfdisc = db.schema_tables['wfdisc']
        tbl_subset = tbl_wfdisc.subset('time <= _%f_ && endtime >= _%f_' %
            (t_start - posix - edge, t_end - posix + edge))
        tbl_sorted = tbl_subset.sort('sta', unique=True)
        fdir = {}
        for record in tbl_sorted.iter_record():
            sta = record.getv('sta')[0]
            fdir[sta] = {}
            for chan in chan_subset:
                fdir[sta][chan] = {}
        for sta in fdir.keys():
            tbl_sta = tbl_subset.subset('sta =~ /%s/' % sta)
            for chan in chan_subset:
                tbl_chan = tbl_sta.subset('chan =~ /%s./' % chan)
                for comp in cmp:
                    tbl_comp = tbl_chan.subset('chan =~ /%s%s/' % (chan, comp))
                    tbl_comp = tbl_comp.sort('time')
                    fdir[sta][chan][comp] = []
                    for record in tbl_comp.iter_record():
                        fdir[sta][chan][comp].append(record.extfile()[1])
                    if not fdir[sta][chan][comp]:
                        fdir[sta].pop(chan, None)
                        break
                if fdir[sta] == {}:
                    fdir.pop(sta, None)
    return fdir

#-------------------------------------------------------------
def get_trigs(fdir, p_file, t_start, t_end):
    """
    Reads in a vertical trace via ObsPy and calculates a PPTS for it.
    Returns a list of touples for trigger window.
    """
    edge = p_file['edge']
    det_par = p_file['det_par']
    th_min = p_file['trig_min']
    # Check that file exists
    for a_file in fdir:
        if not isfile(a_file):
            print "%s is not a file" % a_file
            return []

    # Read in trace
    try:
        st = read_wf_data(fdir, t_start-edge, t_end+edge)
    except:
        return []

    if st is None:
        return []

    # Detrend and taper
    st.detrend(type='linear')
    st.taper(max_percentage=0.05, type='cosine')

    # Construct joint PPTS for trace
    try:
        st.data = ppts(st, det_par, 100, start=edge, stop=edge)
    except ValueError:
        return []

    # Calculate trigger window indices
    trigs = list(obs.trigger.triggerOnset(st.data, p_file['trig_on'], p_file['trig_off']))

    start = st.stats.starttime
    rtn_trigs = []

    i = 0
    for trig in trigs:
        th_max = np.max(st.data[trig[0]:trig[1]+1])
        if th_max >= th_min:
            rtn_trigs.append([])
            rtn_trigs[i].append(st.stats.starttime + np.float(trig[0]*st.stats.delta))
            rtn_trigs[i].append(st.stats.starttime + np.float(trig[1]*st.stats.delta))
            i += 1

   
    return rtn_trigs

#-------------------------------------------------------------
def get_picks(db, p_file, sta, chan, time, start_lag=11.0,
              end_lag=14, filt=[3,10], cov_win=3.0, pick_p=True):
    """
    Read in data for 3 component trace using datascope API, and
    attempt to make P and S picks
    """
    s_pick = None
    if pick_p:
        p_on = p_file['p_on']
        p_off = p_file['p_off']
    s_on = p_file['s_on']
    s_off = p_file['s_off']
    try:
        N, dt = get_data(db, sta, chan, time, start_lag, end_lag, filt)
    except (ds.TrloadNoData, ds.TrloadError, ds.TrfilterError):
        return None, None
    try:
        E, dt = get_data(db, sta, chan, time, start_lag, end_lag, filt)
    except (ds.TrloadNoData, ds.TrloadError, ds.TrfilterError):
        return None, None
    try:
        Z, dt = get_data(db, sta, chan, time, start_lag, end_lag, filt)
    except (ds.TrloadNoData, ds.TrloadError, ds.TrfilterError):
        return None, None

    if pick_p:
        p_picker = pick.PPicker()
        p_pick, snr_p = p_picker.pick(Z, dt, peak_min=p_on, on=3.0, off=p_off)
    else:
        p_pick = start_lag

    s_picker = pick.SPicker()
    T = s_picker.polarize_traces(N, E, Z, dt, cov_win)
    if p_pick is not None:
        s1_pick, snr1 = s_picker.pick(N, dt, p_pick=p_pick, on=s_on, off=s_off)
        s2_pick, snr2 = s_picker.pick(E, dt, p_pick=p_pick, on=s_on, off=s_off)
    else:
        s1_pick, snr1 = s_picker.pick(N, dt, on=s_on, off=s_off)
        s2_pick, snr2 = s_picker.pick(E, dt, on=s_on, off=s_off)

    if (s1_pick is None) and (s2_pick is not None):
        s_pick = s2_pick
    elif (s2_pick is None) and (s1_pick is not None):
        s_pick = s1_pick
    elif (s1_pick is not None) and (s2_pick is not None):
        if snr1 > snr2:
            s_pick = s1_pick
        else:
            s_pick = s2_pick
    else:
        s_pick = None
    if s_pick is not None:
        s_pick = time + s_pick - start_lag
    if p_pick is not None:
        p_pick = time + p_pick - start_lag
    return p_pick, s_pick

#-------------------------------------------------------------
def get_data(db, sta, chan, time, start_lag=11.0, end_lag=14.0,
             filt=[3, 10]):
    starttime = time - start_lag
    endtime = time + end_lag
    tr = db.trloadchan(starttime, endtime, sta, chan)
    if tr.record_count == 0:
        logger.warning('Could not load data for %s:%s %s - %s' %\
                    (sta,
                    chan,
                    stock.epoch2str(time - end_lag, '%Y%j-%H:%M:%S'),
                    stock.epoch2str(time + end_lag, '%Y%j-%H:%M:%S')))
        raise ds.TrloadError
    with ds.trdestroying(tr):
        tr.trfilter('BW %s 4 %s 4' % (filt[0], filt[1]))
        tr.record = 0
        try:
            dt = 1.0/tr.getv('samprate')[0]
        except ds.DbgetvError as err:
            logger.error("Could not get value 'samprate' for %s:%s %s - %s." %\
                    (sta,
                    chan,
                    stock.epoch2str(time - end_lag, '%Y%j-%H:%M:%S'),
                    stock.epoch2str(time + end_lag, '%Y%j-%H:%M:%S')))
        data = []
        for segment in tr.iter_record():
            data += list(segment.trdata())
        data = np.array(data)
    return data, dt


#-------------------------------------------------------------
def get_picks_old(fdirs, t_start, t_end, p_file, start_lag=22, end_lag=10.0,
              filter=[3,10], cov_win=3.0):
    """
    Reads in a 3 component seismogram from [N, E, Z] files in fdirs.
    Uses t_start-start_lag as start time, t_end+end_lag as end time.
    Optionally change bandpass filter.
    """
    T = obc.Stream()
    s_pick = None
    p_on = p_file['p_on']
    p_off = p_file['p_off']
    s_on = p_file['s_on']
    s_off = p_file['s_off']
    try:
        T += read_wf_data(fdirs['N'], t_start-start_lag, t_end+end_lag)
    except:
        return None, None
    try:
        T += read_wf_data(fdirs['E'], t_start-start_lag, t_end+end_lag)
    except:
        return None, None
    try:
        T += read_wf_data(fdirs['Z'], t_start-start_lag, t_end+end_lag)
    except:
        return None, None
    if not (T[0].stats.npts == T[1].stats.npts == T[2].stats.npts):
        return None, None
    T.detrend(type='linear')
    T.filter(type='bandpass', freqmin=filter[0], freqmax=filter[1])

    p_picker = pick.PPicker()
    p_pick, snr_p = p_picker.pick(T[2], peak_min=p_on, on=3.0, off=p_off)
    s_picker = pick.SPicker()
    T = s_picker.polarize_traces(T, cov_win)
    if p_pick is not None:
        s1_pick, snr1 = s_picker.pick(T[0], p_pick=p_pick, on=s_on, off=s_off)
        s2_pick, snr2 = s_picker.pick(T[1], p_pick=p_pick, on=s_on, off=s_off)
    else:
        s1_pick, snr1 = s_picker.pick(T[0], on=s_on, off=s_off)
        s2_pick, snr2 = s_picker.pick(T[1], on=s_on, off=s_off)

    if (s1_pick is None) and (s2_pick is not None):
        s_pick = s2_pick
    elif (s2_pick is None) and (s1_pick is not None):
        s_pick = s1_pick
    elif (s1_pick is not None) and (s2_pick is not None):
        if snr1 > snr2:
            s_pick = s1_pick
        else:
            s_pick = s2_pick
    else:
        s_pick = None
    if s_pick is not None:
        s_pick = T[0].stats.starttime + s_pick
    if p_pick is not None:
        p_pick = T[0].stats.starttime + p_pick
    return p_pick, s_pick

#-------------------------------------------------------------
def pick_fzhw(fdir, t_start, p_file, meta, fault, start_lag=10, end_lag=30.0,
              corner=1):
    """
    Reads in a 3 component seismogram from [N, E, Z] files in fdirs.
    Uses t_start-start_lag as start time, t_end+end_lag as end time.
    Optionally change bandpass filter.
    """
    T = read_wf_data(fdirs['Z'], t_start-start_lag, t_end+end_lag)
    picker = pick.FZHWPicker(meta=meta, fault=fault)
    p_pick, hw_pick = picker.pick(T[0])
    T.detrend(type='linear')
    T.filter('highpass', freq=corner)
    if hw_pick is None:
        hw_pick = -1
    if p_pick is None:
        p_pick = -1
    return p_pick, hw_pick

#-------------------------------------------------------------
def write_picks(dbout, picks, sta, chan, write_p=True):
    """
    Takes in a list of touples (p_pick, s_pick_) and writes valid picks
    to antelope detection/arrival tables for a given station 'sta'
    """
    tbl_detect = dbout.schema_tables['detection']
    if picks[0] is not None:
        if write_p:
            tbl_detect.record = tbl_detect.addnull()
            tbl_detect.putv(('sta', sta), ('chan', '%sZ' % chan[:-1]),
                            ('time', picks[0]), ('state', 'l'),
                            ('filter', 'BW 1 2 10 2'))
    if picks[1] is not None:
        if check_pick_spacing(picks[0], picks[1], 0):
            tbl_detect.record = tbl_detect.addnull()
            tbl_detect.putv(('sta', sta), ('chan', '%sN' % chan[:-1]),
                            ('time', picks[1]), ('state', 'l'),
                            ('filter', 'BW 1 2 10 2'))
    return 1

#-------------------------------------------------------------
def check_pick_spacing(p_pick, s_pick, posix):
    """
    Checks that the spacing between an s_pick and a p_pick is more
    than a certain amount.
    """
    if p_pick is not None:
        if s_pick is not None:
            if (s_pick - posix) - (p_pick - posix) >= 1.0:
                return 1
    return 0


#-------------------------------------------------------------
def read_wf_data(fdir, t_start, t_end):
    st = obc.Stream()
    for file in fdir:
        st += obc.read(file, starttime=t_start, endtime=t_end)
    if len(st) == 0:
        return None
    tr = st[0]
    for T in st[1:]:
        tr.__add__(T, method=1)
    return tr

#-------------------------------------------------------------
class FZTWDetector():

    def __init__(self, s_pick=None, max_len=1.0, offset=0):
        self.max_len = max_len
        self.pick = s_pick
        self.offset = offset
        self.feat_thr = None

    def classify(self, feat):
        if self.feat_thr is None:
            print "Error: classifier not trained"
            return
        if feat[0] >= self.feat_thr[0] and\
                feat[1] >= self.feat_thr[1] and\
                feat[2] <= self.feat_thr[2] and\
                feat[3] >= self.feat_thr[3] and\
                feat[4] >= self.feat_thr[4]:
            return 1
        else:
            return 0

    def extract_features(self, tr):
        """
        st is an ObsPy stream of either fault parallel, or vertical seismograms
        """
        if self.pick is None:
            print "Error: no pick defined in detector"
            return None, None, None

        # Calculate S to peak time
        dt = tr.stats.delta
        start = int(self.pick/dt)
        if start < 0:
            return None, None, None
        end = start + int(self.max_len/dt)
        if end >= len(tr):
            return None, None, None
        window = tr[start:end]
        window = window - np.mean(window)

        short_freq = self.nakamura(window, dt)
        short_energy = np.sum(window**2)
        peak_loc = np.argmax(window**2)
        win_avg = np.median(window**2)
        peak = np.max(window**2)
        peak_ratio = peak/win_avg

        start = int((self.pick-3.0)/dt)
        end = start + int(6.0/dt)
        if end >= len(tr):
            return None, None, None
        window = tr[start:end]
        window = window - np.mean(window)
        long_energy = np.sum(window**2)

        return short_freq, short_energy, long_energy, peak_loc, peak_ratio

    def med_abs_dev(self, x):
        return np.median(np.abs(np.array(x)-np.median(x)))

    def nakamura(self, tr, dt):
        X = np.zeros(tr.size-1)
        D = np.zeros(tr.size-1)
        xlast = 0
        dlast = 0
        alpha = 0.999
        for i in xrange(tr.size-1):
            if i > 0:
                dxdt = (tr[i] - tr[i-1])/dt
            else:
                dxdt = tr[i]
            X[i] = xlast*alpha + tr[i]**2
            D[i] = dlast*alpha + dxdt**2
            xlast = X[i]
            dlast = D[i]
        Tp = 2*np.pi*np.sqrt(X[i]/D[i])
        return Tp

    def normalize(self, feat):
        norm_feat = np.zeros(feat.shape)
        for j in xrange(feat.shape[1]):
            for i in xrange(feat.shape[0]):
                    nval = np.delete(feat[:,j], i)
                    median = np.median(nval)
                    MAD = self.med_abs_dev(nval)
                    norm_feat[i,j] = (feat[i,j] - median)/MAD
        return norm_feat

    def train(self, feat_thr):
        """
        Set classifier parameters
        """
        self.feat_thr = np.zeros(len(feat_thr))
        for i in xrange(len(feat_thr)):
            self.feat_thr[i] = feat_thr[i]
        return
