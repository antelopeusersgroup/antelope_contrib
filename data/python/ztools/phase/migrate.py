# This is a module that accompanies dbmigrate

# Written by Zachary E. Ross (2015)
# Affiliation: University of Southern California

# Standard library modules
from __future__ import division
import argparse as ap
import os
import sys

# 3rd party modules
import numpy as np
import antelope.datascope as ds
import antelope.stock as stock
from obspy.core.util.geodetics.base import gps2DistAzimuth
import matplotlib.pyplot as plt

# Import user defined modules
import ztools.phase.algorithm as pha

#------------------------------------------------
def detector(tr, dt, p_file):
    ntr = np.abs(pha.pai_k(tr, int(5.0/dt)))
    ntr = np.gradient(ntr, dt)
    ntr[np.nonzero(ntr<0)[0]] = 0
    ntr.resize(ntr.size+1)

    thr_cutoff = p_file['thr_cutoff']
    if ntr.max() >= thr_cutoff:
        ntr = ntr * thr_cutoff / ntr.max()
    return ntr

#------------------------------------------------
def hypo_dist(lat1, lon1, dep1, lat2, lon2, dep2):
    gc, _, _ = gps2DistAzimuth(lat1, lon1, lat2, lon2)
    gc = gc/1000.
    D = np.sqrt(gc**2 + (dep1-dep2)**2)
    return D

#------------------------------------------------
def lat2dist(lats, lons, lat_r, lon_r):
    if not isinstance(lats, np.ndarray):
        lats = np.array([lats])
    if not isinstance(lons, np.ndarray):
        lons = np.array([lons])
    X = hypo_dist(np.full(len(lats), lat_r), lons, 0, lat_r, lon_r, 0)
    Y = hypo_dist(lats, np.full(len(lons), lon_r), 0, lat_r, lon_r, 0)
    X = X*np.sign(lons-lon_r)
    Y = Y*np.sign(lats-lat_r)
    return X, Y

#------------------------------------------------
def get_data(db, sta, chan, starttime, endtime, filt=[3, 10]):
    # Read in data and check for success
    tr = db.trloadchan(starttime, endtime, sta, chan)

    with ds.trdestroying(tr):
        # Apply calibration
#        tr.trapply_calib()

        # Optional filtering of data
        if filt is not None:
            tr.trfilter('BW %s 4 %s 4' % (filt[0], filt[1]))

        tr.record = 0

        # Get sampling rate
        dt = 1.0/tr.getv('samprate')[0]

        # Actually extract data and string together multiple traces
        data = []
        for segment in tr.iter_record():
            data += list(segment.trdata())
        data = np.array(data)

    return data, dt

#------------------------------------------------
def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # haversine formula
    dlon = np.deg2rad(lon2 - lon1)
    dlat = np.deg2rad(lat2 - lat1)

    a = np.sin(dlat/2.0)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2.0)**2
    c = 2 * np.arcsin(np.sqrt(a))
    r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    return c * r

#------------------------------------------------
def next_pow_2(x):
    return 1<<(x-1).bit_length()

#------------------------------------------------
def cc(x, y, tmax=None, fs=None):
    xlen = np.size(x)
    ylen = np.size(y)
    olen = xlen + ylen - 1
    wlen = next_pow_2(olen)
    corr = np.fft.irfft(np.fft.rfft(x, n=wlen) * np.fft.rfft(y[::-1], n=wlen))
    xcov = np.sqrt(np.dot(x,x))
    ycov = np.sqrt(np.dot(y,y))
    if xcov == 0:
        return None, None
    if ycov == 0:
        return None, None
    corr = corr[:olen]/(xcov*ycov)
    if tmax is not None and fs is not None:
        mid = int((olen-1)/2)
        idx_max = int(tmax*fs)
        a = mid - idx_max
        b = mid + idx_max
        corr = corr[a:b+1]
    dt = np.argmax(corr)
    return int(dt - (corr.size-1)/2), corr[dt]

#------------------------------------------------
def xcf(a, b):
    from scipy.signal import fftconvolve
    new_len = len(a) + len(b) - 1
    x = (np.array(a) - np.mean(a))#/np.std(a)
    y = (np.array(b) - np.mean(b))#/(np.std(b)*len(b))
    #corr = np.correlate(x, y, mode='valid')
    #corr /= np.sqrt(varx*vary)
    x = x[::-1]
    corr = fftconvolve(x, y, mode='valid')
    return corr

#------------------------------------------------
def shift(tr, dt):
    # time shift should be in samples
    ntr = np.zeros(tr.size)
    if dt < 0:
        # Shift to the left
        dt = abs(dt)
        ntr[:tr.size-dt] = tr[dt:]
        return ntr
    elif dt > 0:
        # Shift to the right
        ntr[dt:] = tr[:len(tr)-dt]
        return ntr
    else:
        return tr

#------------------------------------------------
def beam_pick(D, origin, dt, tt, p_file, win_len=1.0, plot=False):
    # beam-picking function
    # Takes an array of traces [nsta,nsamp]
    # origin is the origin time in seconds
    # dt is the sampling interval in seconds
    # tt is an array of travel times (in samples) for the best loc. estimate

    shifted = np.zeros(D.shape)
    io = int(origin/dt)
    thr_cc = p_file['thr_cc']
    thr_cutoff = p_file['thr_cutoff']
    thr_pick = p_file['thr_pick']

    # Migrate each trace back to origin time
    for i in xrange(D.shape[0]):
        shifted[i,:] = shift(D[i,:], -1*tt[i])

    # Now, window around origin time with length equal to 2*win_len
    ia = io - int(win_len/dt)
    ib = io + int(win_len/dt)
    if ia < 0:
        return None, None
    if ib > shifted.shape[1]:
        return None, None
    shifted = shifted[:,ia:ib+1]
    beam = np.zeros(shifted.shape[1])

    if plot:
        # Optional plotting of migrated traces vs non-migrated
        for i in xrange(shifted.shape[0]):
            plt.subplot(411)
            plt.plot(np.arange(shifted.shape[1])*dt, shifted[i,:])
            plt.subplot(412)
            plt.plot(np.arange(D.shape[1])*dt, D[i,:])
        plt.subplot(413)
        plt.plot(np.arange(shifted.shape[1])*dt, np.sum(shifted, axis=0))

    # For trace with largest peak detector value, cross correlate against network
    best = 0
    peak = 0
    for i in xrange(shifted.shape[0]):
        if np.amax(shifted[i,:]) > peak:
            best = i
            peak = np.amax(shifted[i,:])

    for i in xrange(D.shape[0]):
        delay, corr = cc(shifted[best,:], shifted[i,:], fs=1./dt)
        if delay is None:
            continue
        if corr >= thr_cc and np.amax(shifted[i,:]) > thr_pick:
            tmp = shift(shifted[i,:], delay)
            if p_file['norm'] == 'on':
                beam += tmp/np.amax(tmp)
            else:
                beam += tmp
    beam /= np.max(np.abs(beam))

    if plot:
        plt.subplot(414)
        plt.plot(np.arange(beam.size)*dt, beam)
        plt.show()

    # Make pick on beam at peak of gradient
    on = np.argmax(np.gradient(beam, dt))
    picks = np.zeros(D.shape[0])
    corrs = np.zeros(D.shape[0])

    # Cross correlate with each station
    for i in xrange(D.shape[0]):
        dss = np.dot(beam, beam)
        delay, corr = cc(beam, shifted[i,:], fs=1./dt)
        if delay is None:
            continue
        if corr >= thr_cc and np.amax(shifted[i,:]) >= thr_pick:
            picks[i] = (on - delay + tt[i] + ia)*dt
            corrs[i] = corr
        else:
            picks[i] = np.nan
            corrs[i] = np.nan

    return picks, corrs

#------------------------------------------------
def write_hypo_fmt(ofile, picks, sta_list, chan, netwk):
    # Write picks in HYPOINVERSE2000 format
    with open(ofile, 'a') as f:
        f.write("000000\n")
        for i in xrange(picks.shape[0]):
            if np.isnan(picks[i]):
                continue
            year = int(stock.epoch2str(picks[i], '%Y'))
            month = int(stock.epoch2str(picks[i], '%m'))
            day = int(stock.epoch2str(picks[i], '%d'))
            hour = int(stock.epoch2str(picks[i], '%H'))
            mins = int(stock.epoch2str(picks[i], '%M'))
            sec = int(stock.epoch2str(picks[i], '%S'))
            ms = int(((picks[i]) - int(picks[i]))*100)
            temp = "%-5s%2s  %3s IP  %4s%02d%02d%02d%02d %02d%02d" % \
                   (sta_list[i], netwk, chan, year, month, day, hour, mins, sec, ms)
            f.write("%s\n" % temp)
        f.write("\n")

#------------------------------------------------
def write_event(db, stats, chans, picks, lat, lon, depth, \
                otime, stla, stlo, elev):
    """
    Creates a temporary single-origin database for use with dbgenloc
    """
    from obspy.core.util.geodetics.base import locations2degrees
    # Build origin table
    # Grab last orid from lastid table
    dbtable = db.lookup(table='origin')
    orid = dbtable.nextid('orid')
    tbl_origin = db.schema_tables['origin']
    tbl_origin.record = tbl_origin.addnull()
    nass = np.nonzero(np.isfinite(picks))[0].size
    ndef = nass
    tbl_origin.putv(('lat', lat), ('lon', lon), ('depth', depth),\
                    ('time', otime), ('nass', nass), ('ndef', ndef),\
                    ('dtype', 'f'), ('auth', 'K-beam'), ('orid', orid),\
                    ('evid', orid))

    # Build arrival and assoc tables
    tbl_arrival = db.schema_tables['arrival']
    tbl_assoc = db.schema_tables['assoc']
    dbtable = db.lookup(table='arrival')

    for i in xrange(picks.size):
        if np.isnan(picks[i]):
            continue
        arid = dbtable.nextid('arid')
        tbl_arrival.record = tbl_arrival.addnull()
        tbl_arrival.putv(('sta', stats[i]), ('time', picks[i]), \
                            ('arid', arid), ('chan', chans[i]), \
                            ('iphase', 'P'), ('deltim', 0.1), \
                            ('auth', 'K-beam'))
        tbl_assoc.record = tbl_assoc.addnull()
        delta = locations2degrees(lat, lon, stla[i], stlo[i])
        _, az, baz = gps2DistAzimuth(lat, lon, stla[i], stlo[i])
        tbl_assoc.putv(('arid', arid), ('orid', orid), ('sta', stats[i]), \
                        ('phase', 'P'), ('delta', delta), ('seaz', az), \
                        ('esaz', baz), ('timedef', 'd'), \
                        ('vmodel', 'iasp91'))
    return

#------------------------------------------------
def parse_args():
    parser = ap.ArgumentParser(prog='dbmigrate', description = 'A continuous migration code for event detection')
    parser.add_argument('dbin', type = str, default = None, help = 'Input database name')
    parser.add_argument('dbout', type = str, default = None, help = 'Output database name')
    parser.add_argument('-ts', '--tstart', type = str, help = 'Start time string')
    parser.add_argument('-te', '--tend', type = str, help = 'End time string')
    parser.add_argument('-tw', '--twin', default = 0, type = float, help = 'Processing time window length (sec)')
    parser.add_argument('-o', '--olap', default = 0, type = float, help = 'Overlap amount (sec)')
    parser.add_argument('-pf', default = "%s.pf" % os.path.split(sys.argv[0])[-1], help = 'Parameter file name')
    parser.add_argument('-v', '--verbose', action = "store_true", help = 'Verbose')
    parser.add_argument('-P', '--plot', action = "store_true", help = 'Disable output plot', default = False)
    return parser.parse_args()

#------------------------------------------------
def plot1(dd, D, dt, dt2, M, X, Y, Z, Mb, Xb, Yb, Zb, peak, thr_cutoff):
    plt.subplot(6,1,1)
    plt.plot(np.arange(dd.size)*dt, dd)
    plt.xlim((0, M.size*dt2))
    plt.plot(peak*dt2, 0, 'rD')

    plt.subplot(6,1,2)
    for i in xrange(D.shape[0]):
        plt.plot(np.arange(D.shape[1])*dt, D[i,:])
    plt.plot(peak*dt2, 0, 'rD')
    plt.ylim(0, thr_cutoff)
    plt.xlim((0, M.size*dt2))

    plt.subplot(6,1,3)
    plt.plot(np.arange(len(M))*dt2, M)
    plt.plot(peak*dt2, Mb, 'rD')
    plt.xlim((0, M.size*dt2))
    plt.ylabel('Migration')

    plt.subplot(6,1,4)
    plt.plot(np.arange(X.size)*dt2, X)
    plt.plot(peak*dt2, Xb, 'rD')
    plt.ylim(np.min(X), np.max(X))
    plt.xlim((0, M.size*dt2))
    plt.ylabel('Lon')

    plt.subplot(6,1,5)
    plt.plot(np.arange(Y.size)*dt2, Y)
    plt.plot(peak*dt2, Yb, 'rD')
    plt.ylim(np.min(Y), np.max(Y))
    plt.xlim((0, M.size*dt2))
    plt.ylabel('Lat')

    plt.subplot(6,1,6)
    plt.plot(np.arange(Z.size)*dt2, Z)
    plt.plot(peak*dt2, Zb, 'rD')
    plt.ylim(np.min(Z), np.max(Z))
    plt.xlim((0, M.size*dt2))
    plt.xlabel('Time (s)')
    plt.ylabel('Depth (km)')

    plt.show()

#------------------------------------------------
def plot2(picks, corrs, D0, D, dt):
    # Optional plotting
    idx = picks.argsort()
    picks = picks[idx]
    corrs = corrs[idx]
    D0 = D0[idx,:]
    D = D[idx,:]
    for i in xrange(D0.shape[0]):
        plt.subplot(211)
        plt.plot(np.arange(D0.shape[1])*dt, D0[i,:]/np.max(np.abs(D0[i,:])) + i)
        if np.isfinite(picks[i]):
            plt.plot(picks[i], i, 'rD')
        plt.subplot(212)
        plt.plot(np.arange(D0.shape[1])*dt, D[i,:]/D[i,:].max() + i)
        if np.isfinite(picks[i]):
            plt.plot(picks[i], i, 'rD')
        print picks[i], corrs[i]
    plt.show()

#------------------------------------------------
def create_tmp_db(fname, sta_list, chan_list, picks, evla, evlo, evdp,\
                  otime, stla, stlo, Zr, args):
    # Create param file
    with open("%s.pf" % fname, 'w') as f:
        f.write("method tttaup\n")
        f.write("model iasp91\n")
        f.write("relocate_params &Arr{\n")
        f.write("depth_floor 30.0\n")
        f.write("}")
    statout = "%s.out" % fname

    # Create descriptor file
    with open("%s" % fname, 'w') as f:
        f.write("# Datascope Database Descriptor file\n")
        f.write("schema")

    # Construct temporary db and call dbgenloc, retrieve results
    with ds.closing(ds.dbopen(fname, 'r+')) as db:
        write_event(db, sta_list, chan_list, picks, evla, evlo,\
                    evdp, otime, stla, stlo, Zr)
        copyanything("%s.site" % args.dbin, "%s.site" % fname)


#------------------------------------------------
def genloc2tables(fname, args, old_orid, t_start):
    with ds.closing(ds.dbopen(fname, 'r+')) as db1:
        with ds.closing(ds.dbopen(args.dbout, 'r+')) as db2:
            dbtable = db1.lookup(table = 'origin')
            dbtable.record = dbtable.find('orid == %d' % old_orid)
            dbout = db2.lookup(table = 'origin')
            orid = dbout.nextid('orid')
            dbtable.putv(('orid', orid),('evid', orid))
            dbout.add(dbtable.get())
            evla, evlo, evdp = dbtable.getv('lat', 'lon', 'depth')
            origin = dbtable.getv('time')[0] - t_start

            dberr = db1.lookup(table = 'origerr')
            dberr.record = dberr.find('orid == %d' % old_orid)
            dbout = db2.lookup(table = 'origerr')
            dberr.putv(('orid', orid))
            dbout.add(dberr.get())

            dbassoc = db1.lookup(table='assoc')
            dbout = db2.lookup(table='assoc')
            start = dbassoc.find('orid == %d' % old_orid)
            end = dbassoc.get_range()[1]

            dbarrival1 = db1.lookup(table='arrival')
            dbarrival2 = db2.lookup(table='arrival')

            arids = []
            for rec in dbassoc.iter_record(start, end):
                arid = dbout.nextid('arid')
                arids.append(arid)
                rec.putv(('arid', arid),('orid', orid))
                dbout.add(rec.get())

            i = 0
            for rec in dbarrival1.iter_record():
                rec.putv(('arid', arids[i]))
                dbarrival2.add(rec.get())
                i += 1

    # Cleanup temp database
    for fl in glob("%s*" % fname):
        os.remove(fl)

#------------------------------------------------
