#########################################
# This is a module for earthquake       #
# phase picking                         #
#                                       #
# Version 1.5                           #
#                                       #
# Author: Zachary E. Ross (2014)        #
# Contact: zross@usc.edu                #
# Website: http://earth.usc.edu/~zross/ #
#########################################
# Standard library modules
from __future__ import division
from copy import deepcopy
import time

# 3rd party modules
import numpy as np
from numpy.random import random_integers
#from obspy.signal.trigger import triggerOnset
from scipy.signal import argrelmin, argrelmax

# Personal libraries
import ztools.phase.algorithm as alg
import ztools.phase.gen as gen

def raytrace(ldeps, lvels, evdp, rec_dist):
    ldeps = np.array([0, 5])
    lvels = 1.7*np.array([2.95, 3.23])
    evdp = 3.0
    lslow = 1.0/lvels

    slopes = np.diff(lvels)/np.diff(ldeps)

#-------------------------------------------------------------
def hypo_dist(lat1, lon1, dep1, lat2, lon2, dep2):
    import obspy.core as obc
    # Calculate hypocentral distance between two locations
    xy = obc.util.geodetics.gps2DistAzimuth(lat1, lon1, lat2, lon2)[0]/1000
    D = np.sqrt(xy**2 + (dep1-dep2)**2)
    return D

#-------------------------------------------------------------
def pad_trace(data, dt, pad_len, w_len, w_start=0):
    """
    Pads the start of a seismic trace with noise drawn randomly from the first
    N seconds of a seismogram so that sufficient time is available for phase
    pickers.
    w_len: sample window length in seconds
    pad_len: padding window length in seconds
    w_start: sample window start location in seconds [optional]
    """
    w_len = int(w_len/dt)
    w_start = int(w_start/dt)
    pad_len = int(pad_len/dt)
    if isinstance(data, np.ndarray):
        noise = data[w_start:w_start+w_len]
        pad = np.random.choice(noise, pad_len, replace=True)
        data_out = np.concatenate((pad, data))
        return data_out
    else:
        return None

#-------------------------------------------------------------
class Picker (object):
    """
    Phase picker base class
    """
    def __init__(self, meta=None):
        self.p = None
        self.meta = meta
        self.cft = None

    def pick(self):
        raise NotImplementedError("Subclass must implement abstract method")

    def sec_to_samp(self, time, fs):
        return int(time*fs)

    def win_idx(self, t, npts, dura, fs):
        """
        Calculates start and stop indices of a window around t
        't' is the time in seconds from start of 'trace' to window around
        'npts' is the length of the trace in samples
        'dura' is in seconds from t (midpoint) to start/end of window
        """
        time = self.sec_to_samp(t, fs)

        start = time - self.sec_to_samp(dura, fs)
        stop = time + self.sec_to_samp(dura, fs)
        if start < 0:
            start = 0
        if start >= npts:
            raise ValueError("Start value in win_idx larger than npts")
        if stop >= npts:
            stop = npts - 1
        return (start, stop)

#-------------------------------------------------------------
class PPicker (Picker):
    """
    P-wave arrival picker (Ross & Ben-Zion 2014, GJI)
    """
    def __init__(self, meta=None):
        super(PPicker, self).__init__(meta=meta)

    def first_motion(self, v_trace):
        """
        Determines the first motion polarity from vertical trace
        v_trace is an ObsPy vertical trace
        """
        if self.p is None:
            raise ValueError("P pick must first be made")
        idx = self.sec_to_samp(self.p, v_trace.stats.sampling_rate)
        if v_trace.data[idx] >= 0:
            return 'U'
        else:
            return 'D'

    def pick(self, tr, dt, dur_min=1.5, peak_min=0, sta=1, lta=10, on=5,
             off=1.0, get_cft=False):
        """
        'tr' is a numpy array containing the seismogram of interest
        'dur_min' is the minimum duration for trigger window
        'peak_min' is the minimum SNR value allowed for the peak STA/LTA
        'sta': short term average window length in seconds
        'lta': long term average window length in seconds
        'on': STA/LTA value for trigger window to turn on
        'off': STA/LTA value for trigger window to turn off
        """
        # Calculate characteristic function
        picks = []
        fs = 1.0/dt
        cft = deepcopy(tr)
        if tr.size/fs <= lta:
            return None, None

        cft = alg.stalta(tr, int(sta*fs), int(lta*fs))
        if get_cft:
            self.cft = cft
        triggers = triggerOnset(cft, on, off)
        triggers = stitch_trigs(triggers, 2.0, 1/fs)
        for on_t, off_t in triggers:
            if off_t <= on_t:
                continue
            if (off_t - on_t)/fs < dur_min:
                continue
            MAX = np.max(cft[on_t:off_t])
            idx = np.argmax(cft[on_t:off_t])
            if MAX >= peak_min:
                picks.append((on_t, MAX))

        # Take the largest peak value
        if not picks:
            return None, None
        else:
            p_pick = max(picks, key=lambda x: x[1])[0]
            SNR = max(picks, key=lambda x: x[1])[1]
        self.p = p_pick/fs
        return self.p, SNR


    def resid(self, trace, origin_time):
        dt = super(PPicker, self).resid(trace, origin_time, 'Pg')
        return dt

#-------------------------------------------------------------
class SPicker (Picker):
    """
    S-wave arrival picker (Ross & Ben-Zion 2014, GJI)
    """
    def __init__(self, meta=None):
        self.filter = None
        super(SPicker, self).__init__(meta=meta)

    def polarize_traces(self, N, E, Z, dt, wlen):
        """
        Calculate P & S polarization filters from rectilinearity and
        apparent incidence angle.
        Inputs:
        [N, E, Z]:= Numpy arrays for each trace
        wlen: A sliding window length in seconds for rolling cov mat
        Outputs:
        p_filt: p polarization filter
        s_filt: s polarization filter
        """
        CZ = np.ascontiguousarray(Z, dtype=np.float32)
        CE = np.ascontiguousarray(E, dtype=np.float32)
        CN = np.ascontiguousarray(N, dtype=np.float32)
        r, phi = alg.cov_filter(CZ, CN, CE, int(wlen/dt))

        s = r*(1-phi)
        N = N * s
        E = E * s
        return N, E

    def pick(self, tr, dt, p_pick=-1, dur_min=1.0, peak_min=0, sta=1, lta=10,
             on=5, off=1, get_cft=False, k_len=5, k_buf=0.3):
        """
        'tr' is an ObsPy Trace object containing the trace of interest
            This generally should be a horizontal or Q/T trace.
        'p_pick': Use provided p_pick to calculate optional S pick quality
        'dur_min' is the minimum duration for trigger window
        'peak_min' is the minimum SNR value allowed for the peak STA/LTA
        'sta': short term average window length in seconds
        'lta': long term average window length in seconds
        'on': STA/LTA value for trigger window to turn on
        'off': STA/LTA value for trigger window to turn off
        'k_len': Kurtosis sliding-window length
        'k_buf': Kurtosis sliding-window buffer
        """
        # Calculate STA/LTA, run trigger mech to find window w/ largest peak
        picks = []
        fs = 1.0/dt
        k_nsamp = int(k_len*fs)
        if tr.size/fs <= lta:
            return None, None
        nsta = int(sta*fs)
        nlta = int(lta*fs)
        cft = alg.snr(tr, nsta, nlta, 3.0, 2.0)
        #cft = alg.stalta(tr, nsta, nlta)
        if get_cft:
            self.cftS = cft
        #   self.cftS = moving_average(cft, int(nsta/2))

        triggers = triggerOnset(cft, on, off)
        #triggers = stitch_trigs(triggers, 3.0, 1/fs)
        max_on = 0
        max_off = 0
        best = 0
        for on_t, off_t in triggers:
            if off_t <= on_t:
                continue
            if (off_t - on_t)/fs < dur_min:
                continue
            MAX = np.max(cft[on_t:off_t])
            idx = np.argmax(cft[on_t:off_t])
            if MAX >= peak_min:
                if MAX > best:
                    best = MAX
                    max_on = on_t
                    max_off = off_t
                picks.append((on_t+idx, MAX, on_t, off_t))

        # Write cft to picker
        if get_cft:
            tmp_cft = alg.pai_k(tr, int(k_len*fs))
            tmp_cft[:int((k_len+k_buf)*fs)] = 0
            self.cftK = tmp_cft

        # Take the largest peak value
        if not picks:
            if p_pick != -1:
                return None, None
            else:
                return None, None
        else:
            s_pick = max(picks, key=lambda x: x[1])[0]
            #max_on = max(picks, key=lambda x: x[1])[2]
            #max_off = max(picks, key=lambda x: x[1])[3]
            SNR = max(picks, key=lambda x: x[1])[1]

        # Check for whether multiple large peaks exist in trigger window
        """
        tmp_cft = moving_average(cft[max_on-nsta:max_off+nsta], int(nsta/2))
        maxima = argrelmax(tmp_cft, order=1)[0] + max_on - nsta
        tall_max = []
        for idx in maxima:
            if cft[idx] >= 0.6*SNR:
                tall_max.append(idx)
        if tall_max:
            s_pick = tall_max[-1]
        """
        # If P pick does exist, use S-P time to determine allowance
        if p_pick == -1:
            t_sp = 0.5
        else:
            t_sp = (s_pick)/fs - p_pick
        if t_sp <= 0:
            return None, SNR
        if t_sp > 4.0:
            t_sp = 4.0

        start0, stop0 = self.win_idx(s_pick/fs, tr.size, 0.5*t_sp, fs)
        cft = alg.pai_k(tr, k_nsamp)
        cft = np.diff(cft)
        if (stop0 - start0) < 2:
            return None, SNR
        s_pick = np.argmax(cft[start0:stop0]) + start0

        # Final pick refinement
#        u = s_pick - int(0.2*fs)
#        s_pick = cft[u:s_pick+1].argmin() + u

        return s_pick/fs, SNR



#-------------------------------------------------------------
class FZHWPicker (Picker):
    """
    FZHW picker (Ross & Ben-Zion 2014, GJI)
    """
    def __init__(self, meta, fault):
        """
        fault = [lat0, lon0, lat1, lon1],  or station-fault distance (km)
        when giving coordinates, 0 is start location of fault, 1 is end
        meta is a dict containing evlo, evla, evdp, stlo, stla, stdp in km
        """
        self.filter = None
        super(FZHWPicker, self).__init__(meta=meta)
        self.fault = fault

    def fzhw_p_dt(self, p_fast, p_slow):
        """
        Calculates fzhw-p travel time difference for an event located near a
        vertical fault
        """
        evlo = self.meta['evlo']
        evla = self.meta['evla']
        evdp = self.meta['evdp']
        stlo = self.meta['stlo']
        stla = self.meta['stla']
        stdp = self.meta['stdp']
        # Ensure stdp is in km, not meters
        if abs(stdp) >= 5:
            stdp = stdp/1000

        if len(self.fault) == 2:
            lat0 = self.fault[0][0]
            lon0 = self.fault[0][1]
            lat1 = self.fault[1][0]
            lon1 = self.fault[1][1]
            x0 = hypo_dist(lat0, lon0, 0, lat0, lon1, 0)
            y0 = hypo_dist(lat0, lon0, 0, lat1, lon0, 0)
            strike = np.array([x0, -y0])
            norm = np.sqrt(strike[0]**2 + strike[1]**2)
            strike = strike/norm

            x1 = hypo_dist(lat0, lon0, 0, lat0, stlo, 0)
            y1 = hypo_dist(lat0, lon0, 0, stla, lon0, 0)
            if stlo - lon0 < 0:
                x1 = -x1
            if stla - lat0 < 0:
                y1 = -y1
            sta_vec = np.array([x1, y1])

            sta_proj = np.dot(strike, sta_vec)*strike
            sta_dist_vec = sta_vec - sta_proj
            x = np.sqrt(sta_dist_vec[0]**2 + sta_dist_vec[1]**2)
        elif len(self.fault) == 1:
            x = self.fault[0]
        R = hypo_dist(stla, stlo, stdp, evla, evlo, evdp)
        r = np.sqrt(R**2 - x**2)
        th = r/p_fast + np.sqrt(p_slow**-2 - p_fast**-2)*x
        td = R/p_slow

        return td - th

    def pick(self, trace, q_thr=0.03, dt_min=0.065, max_vc=0.10, vp=5.5,
             snr=None, kurt=None, skew=None, polar_flip=True):
        """
        Automatic picker for fault zone head waves. Only requires a single
        component (usually vertical) for usage. The picker makes an initial
        pick for the earliest onset of particle motion, and then makes two
        more in an attempt to find a direct P wave. If the initial pick and
        secondary detectors are too close in time, no fzhw is picked and only
        a direct P is identified.
        picks
        :type fault: float64 or [float64]*4
        :param fault: Station-fault distance or start-end coordinates of fault
        :type q_thr: float64
        :param q_thr: Maximum allowable time difference between P picks
        :type dt_min: float64
        :param dt_min: Minim allowed time separation
        :type max_vc: float64
        :param max_vc: maximum velocity contrast allowed
        :type vp: float64
        :param vp: P wave velocity (km/s) to assume for contrast calculation
        :type para: list of dictionaries
        :param para: Contains picker params for each picker
        :type detector: bool
        :param detector: Return CF traces on exit?
        :return: [picks, Final_P_pick, dt offset, CF traces (optional)]
        If a hw is not present, 'Final_P_pick' = None
        """
        SNR_par= {'on': 5,
                  'off': 4,
                  'sta': 0.1,
                  'lta': 10,
                  'peak_min': 0,
                  'dur_min': 0}

        if snr is not None:
            for key in snr:
                SNR_par[key] = snr[key]

        K_par = {'on': 15,
                 'off': 2,
                 'wlen': 5,
                 'peak_min': 0}

        if kurt is not None:
            for key in kurt:
                K_par[key] = kurt[key]

        S_par = {'on': 3,
                 'off': 0.5,
                 'wlen': 5,
                 'peak_min': 0}

        if skew is not None:
            for key in skew:
                S_par[key] = skew[key]

        hw_pick, hw_alt = self._first_pick(trace, SNR_par)
        cft1 = self.cft
        K_pick, K_alt = self._K_pick(trace, K_par)
        cft2 = self.cft
        S_pick, S_alt = self._S_pick(trace, S_par)
        cft3 = self.cft
        self.cft = np.column_stack((cft1, cft2, cft3))

        # Check that picks were made properly
        if hw_pick is None:
            return None, None
        if K_pick is None or S_pick is None:
            return hw_pick, None
        if np.mean([K_pick, S_pick]) < hw_pick:
            return hw_pick, None

        dt = []
        dt.append(K_pick - hw_pick)
        dt.append(S_pick - hw_pick)

        fs = trace.stats.sampling_rate
        hw_pick = int(hw_pick*fs)
        hw_alt = int(hw_alt*fs)
        K_pick = int(K_pick*fs)
        K_alt = int(K_alt*fs)
        S_pick = int(S_pick*fs)
        S_alt = int(S_alt*fs)

        # Check for polarity reversal near skewness gradient peak
        half_width = S_alt - S_pick
        start = S_pick - half_width
        stop = S_pick + half_width
        skew_win = cft3[start:stop]
        zeros = np.where(np.diff(np.sign(skew_win)))[0] + 1

        if zeros.size == 0:
            return hw_pick/fs, None
        zeros += start
        p_flip = zeros[np.argmin(np.abs(zeros-S_pick))]
        P_pol = np.sign(cft3[p_flip])

        # Check for polarity reversal near hw pick (error checking)
        start = hw_pick - int(0.02*fs)
        stop = hw_pick + int(0.02*fs)
        hw_win = cft3[start:stop]
        zeros = np.where(np.diff(np.sign(hw_win)))[0] + 1
        if zeros.size == 0:
            HW_pol = np.sign(cft3[hw_pick])
            hw_flip = hw_pick
        elif zeros.size == 1:
            zeros += start
            hw_flip = zeros[np.argmin(np.abs(zeros-hw_pick))]
            HW_pol = np.sign(cft3[hw_flip])
        else:
            zeros += start
            hw_flip = zeros[-1]
            HW_pol = np.sign(cft3[hw_flip])
        if polar_flip:
            if HW_pol == P_pol:
                return hw_pick/fs, None

        # Make sure the skewness polarity doesn't flip multiple times
        hw_win = cft3[hw_flip+1:p_flip]
        zeros = np.where(np.diff(np.sign(hw_win)))[0] + 1
        if zeros.size > 0:
            return hw_pick/fs, None

        # Check that P picks are in specific range from HW pick
        vp_slow = vp - max_vc*vp
        dt_max = self.fzhw_p_dt(vp, vp_slow)
        if dt_max <= 0:
            return hw_pick/fs, None

        # Check that time differences are in allowed range
        if dt[0] < dt_min or dt[1] < dt_min:
            return hw_pick/fs, None
        if dt[0] > dt_max or dt[1] > dt_max:
            return hw_pick/fs, None
        # Check that picks are nearly coincident
        if np.abs(K_pick - S_pick) / fs  >= q_thr:
            return hw_pick/fs, None

        # Refine skewness pick with extrema near polarity flip
        minima = argrelmin(cft3[p_flip-1*half_width:p_flip+1])[0]
        if len(minima) >= 1:
            min_pick = minima[len(minima)-1] + p_flip - 1*half_width
        maxima = argrelmax(cft3[p_flip-1*half_width:p_flip+1])[0]
        if len(maxima) >= 1:
            max_pick = maxima[len(maxima)-1] + p_flip - 1*half_width
        if len(maxima) >= 1 and len(minima) >= 1:
            if max_pick > min_pick:
                S_pick = max_pick
            else:
                S_pick = min_pick
        elif len(maxima) >= 1:
            S_pick = max_pick
        elif len(minima) >= 1:
            S_pick = min_pick

        # Refining kurtosis pick with minima near polarity flip
        minima = argrelmin(cft2[p_flip-1*half_width:p_flip+1])[0]
        if len(minima) >= 1:
            K_pick = minima[len(minima)-1] + p_flip - 1*half_width
        hw_pick = hw_alt
        P_pick = (K_pick+S_pick)/2

        # Algorithm is concluded; mark first motion as FZHW
        return P_pick/fs, hw_pick/fs

    def _first_pick(self, trace, par):
        """
        'trace' is an ObsPy trace class containing a vertical cmp seismogram
        'par' is a dict containing the following quantities:
        'dur_min' is the minimum duration for trigger window
        'peak_min' is the minimum SNR value allowed for the peak STA/LTA
        'sta': short term average window length in seconds
        'lta': long term average window length in seconds
        'on': STA/LTA value for trigger window to turn on
        'off': STA/LTA value for trigger window to turn off
        """
        dur_min = par['dur_min']
        peak_min = par['peak_min']
        sta = par['sta']
        lta = par['lta']
        on = par['on']
        off = par['off']

        # Calculate characteristic function
        picks = []
        fs = trace.stats.sampling_rate
        cft = trace.copy()
        cft.trigger('recstalta', sta=sta, lta=lta)
        triggers = triggerOnset(cft.data, on, off)
        for on_t, off_t in triggers:
            if off_t <= on_t:
                continue
            if (off_t - on_t)/fs < dur_min:
                continue
            MAX = np.max(cft.data[on_t:off_t])
            idx = np.argmax(cft.data[on_t:off_t])
            if MAX >= peak_min:
                picks.append((on_t, MAX))

        # Write cft to picker for plotting
        self.cft = cft.data

        # Take the largest peak value
        if not picks:
            return None, None
        else:
            p_pick = max(picks, key=lambda x: x[1])[0]
            SNR = max(picks, key=lambda x: x[1])[1]

        start = p_pick - int(0.03*fs)
        alt_pick = np.argmin(cft.data[start:p_pick]) + start

        return p_pick/fs, alt_pick/fs

    def _K_pick(self, trace, par):
        """
        Make a pick for sharpest arrival using kurtosis
        'trace' is an ObsPy trace class containing a vertical cmp seismogram
        'par' is a dict with the following keys:
        'peak_min' is the minimum K value allowed for the peak
        'wlen': sliding window length in seconds
        'on': K value for trigger window to turn on
        'off': K value for trigger window to turn off
        """
        peak_min = par['peak_min']
        wlen = par['wlen']
        on = par['on']
        off = par['off']

        # Calculate characteristic function
        picks = []
        fs = trace.stats.sampling_rate
        cft = trace.copy()
        cft.data = alg.pai_k(cft.data, int(wlen*fs))

        triggers = triggerOnset(cft.data, on, off)
        for on_t, off_t in triggers:
            if off_t <= on_t:
                continue
            MAX = np.max(cft.data[on_t:off_t])
            idx = np.argmax(cft.data[on_t:off_t])
            if MAX >= peak_min:
                picks.append((on_t+idx, MAX))

        # Write cft to picker for plotting
        self.cft = cft.data

        # Take the largest peak value
        if not picks:
            return None, None
        else:
            k_pick = max(picks, key=lambda x: x[1])[0]

        # Refining pick using nearby derivative
        alt_pick = k_pick
        start = k_pick - self.sec_to_samp(1, fs)
        grad = np.gradient(cft.data, 1/fs)
        if start <= 0:
            return None, None
        k_pick = np.argmax(grad[start:k_pick]) + start - 1
        return k_pick/fs, alt_pick/fs

    def _S_pick(self, trace, par):
        """
        Make a pick for sharpest arrival using skewness
        'trace' is an ObsPy trace class containing a vertical cmp seismogram
        'par' is a dict with the following keys:
        'peak_min' is the minimum S value allowed for the peak
        'wlen': sliding window length in seconds
        'on': S value for trigger window to turn on
        'off': S value for trigger window to turn off
        """
        peak_min = par['peak_min']
        wlen = par['wlen']
        on = par['on']
        off = par['off']

        # Calculate characteristic function
        picks = []
        fs = trace.stats.sampling_rate
        cft_clean = trace.copy()
        cft_clean.data = alg.pai_s(cft_clean.data, int(wlen*fs))
        cft = cft_clean.copy()
        cft.data = np.abs(cft.data)

        triggers = triggerOnset(cft.data, on, off)
        for on_t, off_t in triggers:
            if off_t <= on_t:
                continue
            MAX = np.max(cft.data[on_t:off_t])
            idx = np.argmax(cft.data[on_t:off_t])
            if MAX >= peak_min:
                picks.append((on_t+idx, MAX))

        # Write cft to picker for plotting
        self.cft = cft_clean.data

        # Take the largest peak value
        if not picks:
            return None, None
        else:
            s_pick = max(picks, key=lambda x: x[1])[0]

        # Refining pick using nearby derivative
        alt_pick = s_pick
        start = s_pick - self.sec_to_samp(1, fs)
        grad = np.gradient(cft.data, 1/fs)
        if start <= 0:
            return None, None
        s_pick = np.argmax(grad[start:s_pick]) + start - 1
        return s_pick/fs, alt_pick/fs

#-------------------------------------------------------------
def Tpd(tr, t_mx, dt, t_w, n_samp):
    # Predominant damped period (Hildyard et al. 2008)
    # Requires tr to be a trace in velocity
    rand_vals = numpy.random.random_integers(0, len(tr), size=n_samp)
    noise = sum(tr[rand_vals]*tr[rand_vals])/n_samp
    D_s = 4*np.pi**2*noise*t_w/dt/t_mx**2
    alpha = np.exp(np.log(0.1)/(t_w/dt))
    a = np.gradient(tr, dt)
    X = np.zeros(len(tr))
    D = np.zeros(len(tr))
    T = np.zeros(len(tr))
    for i in xrange(len(tr)):
        if i == 0:
            X[i] = tr[i]*tr[i]
            D[i] = a[i]*a[i]
        else:
            X[i] = alpha*X[i-1] + tr[i]*tr[i]
            D[i] = alpha*D[i-1] + a[i]*a[i]
        T[i] = 2*np.pi*np.sqrt(X[i]/(D[i]+D_s))
    return T

#-------------------------------------------------------------
def ncheck(vals):
    # if any values in the list are None-type, return False
    a = [0]*len(vals)
    for i in xrange(len(vals)):
        if vals[i] is not None:
            a[i] = 1
    if sum(a) == len(a):
        return True
    else:
        return False

#-------------------------------------------------------------
def find_nearest(array, value):
    idx = (np.abs(array-value)).argmin()
    return idx

#-------------------------------------------------------------
def stitch_trigs(trigs, tol, dt):
    t_len = int(tol/dt)
    sorted_trigs = sorted(trigs, key=lambda x: x[0])
    new_trigs = []
    if len(sorted_trigs) == 0:
        return []
    new_trigs.append(sorted_trigs[0])
    j = 0
    for i in xrange(1, len(sorted_trigs)):
        if (sorted_trigs[i][0] - new_trigs[j][1]) <= t_len:
            new_trigs[j][1] = sorted_trigs[i][1]
        else:
            new_trigs.append(sorted_trigs[i])
            j += 1

    return new_trigs

#-------------------------------------------------------------
def moving_average(a, n) :
    ret = np.cumsum(a, dtype=float)
    ret[n:] = ret[n:] - ret[:-n]
    return ret[n - 1:] / n

#-------------------------------------------------------------
class FZTWDetector():

    def __init__(self, s_pick=None, max_len=2.5, offset=0):
        self.max_len = max_len
        self.pick = s_pick
        self.offset = offset
        self.feat_thr = None

    def classify(self, feat):
        if self.feat_thr is None:
            print "Error: classifier not trained"
            return
        if feat[0] >= self.feat_thr[0] and feat[1] >= self.feat_thr[1] and feat[2] <= self.feat_thr[2]:
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

        start = int((self.pick-3.0)/dt)
        end = start + int(6.0/dt)
        if end >= len(tr):
            return None, None, None
        window = tr[start:end]
        window = window - np.mean(window)
        long_energy = np.sum(window**2)

        return short_freq, short_energy, long_energy

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

#-------------------------------------------------------------
def shear_pick(Z, N, E, dt):
    from __main__ import pfile
    p_picker = PPicker()
    s_picker = SPicker()
    p_on = pfile['p_on']
    p_min = pfile['p_min']
    s_on = pfile['s_on']
    s_off = pfile['s_off']
    cov_len = pfile['cov_len']
    min_dt = pfile['min_dt']
    k_len = pfile['k_len']
    k_buf = pfile['k_buf']

    # Try to make a P pick
    p_pick, snr_p = p_picker.pick(Z, dt, on=p_on, peak_min=p_min)
    # Polarize the horizontal traces
    N, E = s_picker.polarize_traces(N, E, Z, dt, cov_len)

    # Try to make an S pick on N, E
    if p_pick is not None:
        s1_pick, snr_s1 = s_picker.pick(N, dt, p_pick=p_pick, on=s_on, off=s_off, k_len=k_len, k_buf=k_buf)
        s2_pick, snr_s2 = s_picker.pick(E, dt, p_pick=p_pick, on=s_on, off=s_off, k_len=k_len, k_buf=k_buf)
        if s1_pick is not None:
            if s1_pick - p_pick < min_dt:
                s1_pick = None
        if s2_pick is not None:
            if s2_pick - p_pick < min_dt:
                s2_pick = None
    else:
        s1_pick, snr_s1 = s_picker.pick(N, dt, on=s_on, off=s_off, k_len=k_len, k_buf=k_buf)
        s2_pick, snr_s2 = s_picker.pick(E, dt, on=s_on, off=s_off, k_len=k_len, k_buf=k_buf)
    # Checking for the various possible pick results
    if s1_pick is not None and s2_pick is not None:
        if snr_s1 > snr_s2:
            s_pick = s1_pick
            snr = snr_s1
            chan = 0
        else:
            s_pick = s2_pick
            snr = snr_s2
            chan = 1
    elif s1_pick is not None:
        s_pick = s1_pick
        snr = snr_s1
        chan = 0
    elif s2_pick is not None:
        s_pick = s2_pick
        snr = snr_s2
        chan = 1
    else:
        s_pick = None
        snr = None
        chan = None

    return s_pick, snr, chan

#-------------------------------------------------------------
def triggerOnset(cft, on, off):
    res = alg.trigger(cft, on, off)
    ons = res[0]
    offs = res[1]
    idx = np.nonzero(ons)[0]
    ons = ons[idx]
    offs = offs[idx]
    return [list(a) for a in zip(ons, offs)]
