def _configure_logging(logfile, level="INFO"):
    level = getattr(logging, level)
    for name in (__name__,):
        logger = logging.getLogger(name)
        logger.setLevel(level)
        if level == logging.DEBUG:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                                              '%(name)s::%(funcName)s():: '\
                                              '%(message)s',
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

def var_red(source, f, fc, omega, n):
    lsrc = np.log10(source)
    lsyn = np.log10(source_model(f, fc, omega, n))
    ymean = np.mean(lsrc)
    sstot = np.sum((lsrc-ymean)**2)
    return 1 - np.sum((lsyn-lsrc)**2)/sstot

def resample_spec(spec, f, df, f_min, f_max):
    logf = np.log10(f)
    new_f = np.arange(np.log10(f_min), np.log10(f_max), df)
    new_spec = np.interp(new_f, logf, spec)
    new_f = 10**(new_f)
    return new_spec, new_f

class VelModel:
    def __init__(self, modfile):
        temp = np.loadtxt(modfile)
        self.depths = temp[:,0]
        self.vp0 = temp[0,2]
        self.vs0 = temp[0,1]
        self.Vp = temp[:,2]
        self.Vs = temp[:,1]
        self.Qp = temp[:,3]
        self.Qs = temp[:,4]
        self.Vp_interpolator = interp1d(self.depths, temp[:,2])
        self.Vs_interpolator = interp1d(self.depths, temp[:,1])

    def vel(self, depth, phase):
        if phase == 'P':
            if depth > 0:
                return self.Vp_interpolator(depth)
            else:
                return self.vp0
        elif phase == 'S':
            if depth > 0:
                return self.Vs_interpolator(depth)
            else:
                return self.vs0

def next_pow_2(n):
    return 1<<(n-1).bit_length()

def get_rad_pat(azim, inc, foc_mech, phase):
    from numpy import cos, sin
    s = np.radians(foc_mech[0])
    d = np.radians(foc_mech[1])
    r = np.radians(foc_mech[2])
    a = np.radians(azim)
    i = np.radians(inc)
    if phase == 'P':
        F = cos(r) * sin(d) * sin(i)**2 * sin(2*(a-s))\
                - cos(r) * cos(d) * sin(2*i) * cos(a-s)\
                + sin(r) * sin(2*d) * (cos(i)**2 - sin(i)**2 * sin(a-s)**2)\
                + sin(r) * cos(2*d) * sin(2*i) * sin(a-s)
        F = np.abs(F)
    elif phase == 'S':
        FSV = sin(r) * cos(2*d) * cos(2*i) * sin(a-s)\
                - cos(r) * cos(d) * cos(2*i) * cos(a-s)\
                + 0.5 * cos(r) * sin(d) * sin(2*i) * sin(2*(a-s))\
                - 0.5 * sin(r) * sin(2*d) * sin(2*i) * (1 + sin(a-s)**2)
        FSH = cos(r) * cos(d) * cos(i) * sin(a-s)\
                + cos(r) * sin(d) * sin(i) * cos(2*(a-s))\
                + sin(r) * cos(2*d) * cos(i) * cos(a-s)\
                - 0.5 * sin(r) * sin(2*d) * sin(i) * sin(2*(a-s))
        F = np.sqrt(FSV**2 + FSH**2)
    return F

def parse_args():
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('db',
                        type=str,
                        help='input database')
    parser.add_argument('velocity_model',
                        type=str,
                        nargs='?',
                        help='velocity model')
    parser.add_argument('-c',
                        '--chan_subset',
                        type=str,
                        default='chan =~ /.[HN].*/', \
                        help='chan subset expression')
    parser.add_argument('-d',
                        '--defining',
                        default=False,
                        action='store_true',
                        help='use defining phase picks only [False]')
    parser.add_argument('-e',
                        '--event_subset',
                        type=str,
                        help='channel subset expression')
    parser.add_argument('-p',
                        '--pfile',
                        type=str,
                        help="parameter file")
    parser.add_argument('-r',
                        '--rej',
                        type=str,
                        help='reject stations')
    parser.add_argument('-v',
                        '--verbose',
                        default=False,
                        action='store_true',
                        help='verbose')
    parser.add_argument('-x',
                        '--dist',
                        default=0,
                        type=float,
                        help='Minimum hypocentral distance allowed [0]')

    return parser.parse_args()

def parse_pfile(pfile):
    """
    Parse parameter file.
    """
    if not pfile:
        pfile = pfread('dbmw')
    elif not os.path.isfile(pfile):
        pfile = pfread('dbmw')
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

def get_site_metadata():
    sites_metadata = {}
    logger.info('Getting site metadata.')
    with closing(dbopen(args.db, 'r')) as db:
        tbl_site = db.lookup(table='site')
        for site in tbl_site.iter_record():
            sta, lat, lon, elev= site.getv('sta', 'lat', 'lon', 'elev')
            sites_metadata[sta] = {}
            sites_metadata[sta]['lat'] = lat
            sites_metadata[sta]['lon'] = lon
            sites_metadata[sta]['elev'] = elev
    return sites_metadata

def origin_reap_thread(db):
    global args, pfile, input_q
    logger.info('Starting origin_reap_thread.')
    dbtable = db.lookup(table='origin')
    prefor = dbtable.join('event')
    with freeing(prefor):
        prefor = prefor.subset('orid==prefor')
    if args.event_subset:
        with freeing(prefor):
            prefor = prefor.subset(args.event_subset)
    with freeing(prefor):
        prefor = prefor.join('assoc')
    with freeing(prefor):
        prefor = prefor.join('arrival')
    if args.defining:
        with freeing(prefor):
            prefor = prefor.subset('timedef =~ /d/')
    with freeing(prefor):
        prefor = prefor.subset(args.chan_subset)
    with freeing(prefor):
        prefor = prefor.sort( ['orid', 'sta'])
    gr1_ = prefor.group(['orid', 'sta'])
    gr1 = gr1_.sort('orid')
    gr2 = gr1.group('orid')
    # For each origin
    for row1 in gr2.iter_record():
        bund1 = row1.get_range()
        gr1.record = bund1[0]
        bund2 = gr1.get_range()
        prefor.record = bund2[0]
        res = prefor.getv('lat', 
                          'lon', 
                          'origin.depth', 
                          'origin.time', 
                          'orid', 
                          'origin.auth', 
                          'evid')
        lat, lon, depth, time, orid, auth, evid = res
        p_arr = []
        s_arr = []
        # For each station
        for row2 in gr1.iter_record(bund1[0], bund1[1]):
            bund2 = row2.get_range()
            # For each phase (pick)
            for row3 in prefor.iter_record(bund2[0], bund2[1]):
                res = row3.getv('sta', 'arrival.time', 'iphase', 'chan')
                sta, arr_time, phase, chan = res
                if phase == 'P':
                    p_arr += [Arrival(sta, arr_time, phase, chan=chan)]
                if phase == 'S':
                    s_arr += [Arrival(sta, arr_time, phase, chan=chan)]
        logger.debug("Putting origin %d on queue" % orid)
        input_q.put((Origin(lat, lon, depth, time, auth, \
                     orid=orid, evid=evid, unc_z=pfile['unc_z']), \
                     p_arr, s_arr))
    prefor.free()
    gr1_.free()
    gr1.free()
    gr2.free()
    for i in range(pfile['n_threads']):
        input_q.put(None)

def prepare_trace(sta, chan, origin, start_time, end_time, db, \
        arrival=None):
    import antelope.coords
    global site_md
    trace = get_trace(sta, chan, start_time, end_time, db)
    if trace == "clip":
        return "clip"
    elif trace != None:
        delta, az = antelope.coords.dist(origin.lat, origin.lon,
                                         site_md[trace.sta]['lat'],
                                         site_md[trace.sta]['lon'])
        a = delta*111.111#*np.cos(np.radians(site_md[trace.sta]['lat']))
        trace.user1 = np.sqrt(a**2 + origin.depth**2)
        trace.user2 = a
        #if arrival is not None:
        #    trace.user2 = arrival.time - origin.time
    else:
        return None
    return trace

def get_chans(sta_chan, time, db):
    from antelope.stock import yearday
    jdate = yearday(time)
    dbtable = db.lookup(table='wfdisc')
    dbtable = dbtable.subset('jdate =~ /%s/' % jdate)
    chans = {}
    for rec in dbtable.iter_record():
        sta, chan = rec.getv('sta', 'chan')
        if sta in sta_chan:
            if sta_chan[sta][:2] == chan[:2]:
                chan = list(chan)
                #if chan[0] == 'B':
                #    chan[0] = 'H'
                chan = "".join(chan)
                if sta in chans:
                    chans[sta].append(chan)
                else:
                    chans[sta] = [chan]
    dbtable.free()
    return chans

def process_origin_thread(db):
    from antelope.coords import dist
    global input_q, output_q, p_file, site_md, vmod
    logger.info('Starting origin_process_thread.')
    for i in xrange(pfile['max_jobs_respawn']):
        # Pull origin from the queue and get all relevant data
        origin = input_q.get()
        if origin == None:
            output_q.put(None)
            return 0
        origin, p_arrivals, s_arrivals = origin
        logger.debug('Processing origin %d: %d P, %d S' % \
                (origin.orid, len(p_arrivals), len(s_arrivals)))
        phases = {'P': p_arrivals, 'S': s_arrivals}
        stations = [x.sta for x in p_arrivals]
        chans = [x.chan for x in p_arrivals]
        sta_chan = {}
        for (sta, chan) in zip(stations, chans):
            sta_chan[sta] = chan
        #sta_chan = {x.sta:x.chan for x in p_arrivals}
        traces = {'P': [], 'S': []}
        noises = {'P': [], 'S': []}
        out = {'P': [], 'S': []}
        chan_list = get_chans(sta_chan, origin.time, db)

        # Check to see if a focal mechanism exists in fplane table
        tbl_fplane = db.lookup(table='fplane')
        with freeing(tbl_fplane.subset('orid==%d' % origin.orid)) as v:
            if v.record_count == 0:
                fm = None
            else:
                logger.info("Focal mechanism available for orid: %d" % \
                      origin.orid)
                v.record = 0
                strike, dip, rake = v.getv('str1', 'dip1', 'rake1')
                fm = (strike, dip, rake)
        n_clipped = 0
        # For each phase, get 3 component data
        for phase in ['P', 'S']:
            for arr in phases[phase]:
                if arr.sta not in chan_list:
                    continue
                chans = sorted(set(chan_list[arr.sta]))
                if len(chans) != 3:
                    continue
                # Make sure station is beyond min distance
                stla = site_md[arr.sta]['lat']
                stlo = site_md[arr.sta]['lon']
                #r = dist(stla, stlo, origin.lat, origin.lon)[0]*111.11
                #r = np.sqrt(r**2 + origin.depth**2)
                #if r < args.dist:
                #    continue

                # Try and grab data for all 3 components
                for chan in chans:  
                    # Build phase trace
                    start = arr.time - 0.5*pfile['trace_twin']
                    end = arr.time + 0.5*pfile['trace_twin']
                    trace = prepare_trace(arr.sta, chan, origin, \
                                          start, end, db, arrival=arr)
                    if trace == "clip":
                        n_clipped += 1
                        acc_chan = list(chan)
                        acc_chan[0] = 'H'
                        acc_chan[1] = 'N'
                        acc_chan = "".join(acc_chan)
                        trace = prepare_trace(arr.sta, acc_chan, origin, \
                                              start, end, db, arrival=arr)
                    # Build noises trace
                    p_arr = [x for x in p_arrivals if x.sta == arr.sta][0]
                    start = p_arr.time - pfile['trace_twin']
                    end = p_arr.time
                    noise = prepare_trace(p_arr.sta, chan, origin, \
                                          start, end, db, arrival=p_arr)
                    if noise == "clip":
                        acc_chan = list(chan)
                        acc_chan[0] = 'H'
                        acc_chan[1] = 'N'
                        acc_chan = "".join(acc_chan)
                        noise = prepare_trace(p_arr.sta, acc_chan, origin, \
                                              start, end, db, arrival=p_arr)
                    # Extra checks on conditions of traces
                    if trace is None or noise is None:
                        continue
                    if trace == "clip" or noise == "clip":
                        continue
                    traces[phase] += [trace]
                    noises[phase] += [noise]

        logger.info("%d: %d P spectra, %d S spectra, %d P %d S, %s" % \
                (origin.orid, len(traces['P']), len(traces['S']), \
                 len(p_arrivals), len(s_arrivals), origin.auth))

        if len(traces['P']) + len(traces['S']) < pfile['n_min_phase']:
            continue

        # Finally calculate seismic moment for each phase
        for p in ('P', 'S'):
            out[p] = None
            if len(traces[p]) < pfile['n_min_phase']/2.0:
                continue
            out[p] = get_moment(origin, traces[p], noises[p], p, foc_mech=fm)

        # Analysis of results
        mags = []
        n_phase = 0
        if out['P'] is not None:
            logger.info(out['P'][0])
            mags.append(out['P'][0])
        if out['S'] is not None:
            logger.info(out['S'][0])
            mags.append(out['S'][0])
        if len(mags) == 0:
            continue
        Mw = np.mean(mags)
        output_q.put([origin.orid, origin.evid, Mw, 0, len(mags)])
        logger.info("%d Mw=%3.2f" % (origin.orid, Mw))

def distance(lat1, lon1, lat2, lon2, in_km=False):
    if in_km:
        lat = (lat1 + lat2) / 2.0
        return cos(lat * 180.0 / pi) * 111.32 * \
            Dbptr().ex_eval('distance(%f, %f, %f, %f)'
                % (lat1, lon1, lat2, lon2))
    else: return Dbptr().ex_eval('distance(%f ,%f ,%f, %f)'
            % (lat1, lon1, lat2, lon2))

def check_samp_rate(samprate, dec_freq):
    if (samprate % dec_freq) == 0:
        return True
    else:
        return False

def bootstrap(samples, n_boots):
    mags = []
    for i in xrange(n_boots):
        samp = np.random.choice(samples, size=len(samples), replace=True)
        mags.append(np.median(samp))
    return (np.percentile(mags, 97.5) - np.percentile(mags, 2.5))/2.0

def get_trace(sta, chan, time, endtime, db):
    global pfile, args
    tbl_wfdisc = db.lookup(table='wfdisc')
    with trdestroying(tbl_wfdisc.trloadchan(time, endtime, sta, chan)) as tmp_tr:
        if tmp_tr.record_count > 1:
            logger.error("Error reading data: rec count greater than =")
            return None
        try:
            tr = Trace(tmp_tr, attach_response=True)
        except TrdataError:
            logger.error("Error reading data for %s %s" % (sta, chan))
            return None
        except ResponseError:
            logger.error("ERROR: couldn't read instrument response files")
            return None
        except ClippedData:
            logger.error("%s %s is clipped" % (sta, chan))
            return "clip"
        if tr.samprate < pfile['dec_freq']:
            return None
            tr.decimate(int(tr.samprate / pfile['dec_freq']))
    return tr

def source_model2(f, *p):
    fc, omega, n = p
    return 2 * np.pi * f * omega / (1 + (f/fc)**n)

def source_model(f, *p):
    fc, omega, n = p
    return omega / (1 + (f/fc)**n)

def moment(asympt, phase, depth, unc_z=None):
    # Velocities in km/s
    # Density in g/cm^3
    # Asymptote in nanometers
    # Moment is returned in N-m
    global pfile, vmod
    velo = vmod.vel(depth, phase) * 1000
    density = pfile['density'] * 1000
    asympt *= 10**-9

    M0 = 4 * np.pi * density * velo**3 * asympt / 2.0
    Mw = (2/3.)*np.log10(M0) - 6.07
    if unc_z is not None:
        velo_low = vmod.vel(depth-unc_z, phase) * 1000
        velo_high = vmod.vel(depth+unc_z, phase) * 1000
        M0_low = 4 * np.pi * density * velo_low**3 * asympt
        M0_high = 4 * np.pi * density * velo_high**3 * asympt
        Mw_low = (2/3.)*np.log10(M0_low) - 6.07
        Mw_high = (2/3.)*np.log10(M0_high) - 6.07
        unc_vel = (Mw_high - Mw_low)/2.0
        return Mw, unc_vel
    else:
        return Mw

def fit_src(spec, f, fc, omega, n):
    range_fc = 10**(np.linspace(np.log10(fc[0]), np.log10(fc[1]), 100))
    range_o = 10**(np.linspace(np.log10(omega[0]), np.log10(omega[1]), 100))
    range_n = np.linspace(n[0], n[1], 100)
    p0 = mo.fit_1fc(spec, f, range_fc, range_o, range_n)
    return p0[0], p0[1], p0[2]

def resid(p, freqs, data):
    synth = source_model(freqs, p[0], p[1], p[2])
    return np.sum((np.log10(synth) - np.log10(data))**2) 

class Spectrum:
    def __init__(self, 
                 spec, 
                 f,
                 sta=None, 
                 chan=None, 
                 phase=None,
                 azi=None,
                 takeoff=None):
        self.sp = spec
        self.f = f
        self.sta = sta
        self.chan = chan
        self.phase = phase
        self.f0 = 0
        self.azi = azi
        self.takeoff = takeoff

    def div(self, denom_spec):
        self.sp /= denom_spec.sp

    #def correct_atten(self, trav_time, Q):
    def correct_atten(self, t_star):
        #atten_spec = np.exp(-1*self.f*trav_time*np.pi/Q)
        atten_spec = np.exp(-1 * self.f * t_star * np.pi)
        wlevel = pfile['water_level'] * atten_spec.max()
        atten_spec = np.array([r if r != 0 and r > wlevel\
                               else wlevel for r in atten_spec])
        self.sp = self.sp #/ atten_spec
        return

    def rot_amp_spec(self, expon):
        # Differentiate or integrate an amplitude spectrum
        # Exponent > 0 in integer steps for # derivatives
        # Exponent < 0 in integer steps for # integrations
        from numpy import log, exp
        rot_spec = np.zeros(self.sp.size)
        lzero = 0
        if (expon == 0):
            logger.error("Error: exponent of 0 used for rot_amp_spec")
            raise ValueError
        nstart = 0
        if self.f[0] == 0:
            nstart = 1
            lzero = 1
        factor = expon * log(2*np.pi*self.f[nstart:])
        rot_spec[nstart:self.f.shape[0]] = exp(factor + log(self.sp[nstart:]))
        if lzero == 0:
            self.sp = rot_spec
            return
        if expon > 0:
            rot_spec[0] = rot_spec[1]
        if expon < 0:
            rot_spec[0] = 0
        self.sp = rot_spec
        return

def stack_spectra(spec_list, median=False):
    from copy import deepcopy
    # Expects a list of Spectrum objects
    specs = [x.sp for x in spec_list]
    spec = deepcopy(spec_list[0])
    if not median:
        spec.sp = np.sum(specs, axis=0) / len(specs)
    else:
        spec.sp = np.median(specs, axis=0)
    return spec

def vec_mag(spec_list):
    from copy import deepcopy
    specs = [x.sp for x in spec_list]
    spec = deepcopy(spec_list[0])
    spec.sp = np.sqrt(specs[0]**2 + specs[1]**2 + specs[2]**2)
    return spec

def get_moment(origin, traces, noises, phase, foc_mech=None):
    from copy import deepcopy
    global pfile, args, site_md, vmod
    if phase == "P":
        Q = pfile['Qp']
        rad_pat = 0.52
        col = 2
    elif phase == "S":
        Q = pfile['Qs']
        rad_pat = 0.63
        col = 1
    else:
        logger.warning("Phase is not P or S.")

    # Calculate spectra for noise and phase windows
    specs = []
    snrs = []
    dists = {}
    times = {}
    for tr in traces:
        if tr.sta not in dists:
            dists[tr.sta] = tr.user1
        if tr.sta not in times:
            times[tr.sta] = tr.user2
    for sta in sorted(set([x.sta for x in traces])):
        phase_tr = [x for x in traces if x.sta == sta]
        noise_tr = [x for x in noises if x.sta == sta]
        # Check that 3 components exist for both noise and phase
        if len(phase_tr) != 3 or len(noise_tr) != 3:
            logger.warning("Missing a component for station %s" % sta)
            continue
        tmp_phs = []
        tmp_noi = []
        # Calculate velocity spectra 
        for phs, noise in zip(phase_tr, noise_tr):
            tmp_phs.append(phs.calc_spectrum())
            tmp_noi.append(noise.calc_spectrum())
        # Create 3-c spectra
        phs = vec_mag(tmp_phs)
        noise = vec_mag(tmp_noi)

        # Calculate and check SNR
        snr = (phs.sp/noise.sp)**2
        q3 = np.percentile(snr, 75)
        if q3 < 5.0:
            continue
        snrs.append(snr)

        # Rotate spectra into velocity
        if phs.chan[1] == 'N':
            phs.sp /= (2 * np.pi * phs.f)

        specs.append(phs)
        specs[-1].sta = sta
    if not specs:
        return None

    if len(specs) < 5:
        return None

    logger.info("Finished calculating spectra for all stations")
    tracer = rt.RayTracer(vmod.depths, vmod.Vp, origin.depth, Q_vals=vmod.Qp)\
             if phase == 'P'\
             else rt.RayTracer(vmod.depths, vmod.Vs, origin.depth, Q_vals=vmod.Qs)

    # Correct spectra for source & propagation effects
    for i in xrange(len(specs)):
        specs[i].sp *= 1000 * dists[specs[i].sta]
        t_star = tracer.t_star(times[specs[i].sta])
        specs[i].correct_atten(t_star)
        specs[i].sp /= rad_pat
    stack = stack_spectra(specs)
    snr_stack = 10**(np.mean([np.log10(x) for x in snrs], axis=0))
    snr_freq = stack.f

    # Remove portion of spectrum with low SNR from SNR stack
    idx = np.where(snr_stack >= 5.0)[0]
    stack.sp = stack.sp[idx]
    stack.f = stack.f[idx]
    range_fc = (stack.f[0], stack.f[-1])

    stack.sp /= (2 * np.pi * stack.f)

    range_o = (0.75*np.max(stack.sp), 1.25*np.max(stack.sp))
    range_n = (1.5, 3.0)
    logger.info("Fitting model to stack")

    best_model = fit_src(stack.sp, stack.f, range_fc, range_o, range_n)
    logger.info("Finished fitting model..best results are:")
    fc, omega, n = best_model
    Mw, unc_vel = moment(omega, phase, origin.depth, unc_z=origin.unc_z)

    # Estimate the station/focal sphere uncertainties
    unc_mw = unc_vel

    logger.info("phase=%s, Mw=%3.2f, fc=%4.2f, [%3.2f, %3.2f], VR=%3.2f" % \
          (phase, Mw, fc, Mw-unc_mw, Mw+unc_mw, 1.0))

    #if args.plot:
    #    freqs = np.arange(2.0, stack.f[-1], 0.1)
    #    synth = source_model(freqs, fc, omega, n)
    #    import pylab as plt
    #    if phase == 'P':
    #        color = 'r'
    #        plt.clf()
    #    else:
    #        color = 'k'
    #    plt.subplot(211)
    #    plt.loglog(stack.f, stack.sp, color, linewidth=2.0)
    #    plt.loglog(freqs, synth, color+'--', linewidth=2.0)
    #    plt.subplot(212)
    #    plt.loglog(snr_freq, snr_stack, color, linewidth=2.0)

    #    if phase == 'S':
    #        plt.show()
    #        plt.clf()
    return Mw, unc_mw, len(specs)

class MetaResponse:
    def __init__(self, path, chans, time, endtime):
        self.path = path
        self.chans = chans
        self.time = time
        self.endtime = endtime

    def is_valid(self, chan, time):
        chan = '{}{}'.format(chan[:2], chan[3:])
        if chan in self.chans and\
                time >= self.time and\
                time <= self.endtime:
            return True

class ResponseError(Exception):
    pass

class ClippedData(Exception):
    pass

class ResponseInventory:
    def __init__(self):
        global args
        self.responses = {}
        with closing(dbopen(args.db, 'r')) as db:
            tbl_site = db.lookup(table='site')
            view = tbl_site.join('sensor')
            view_ = view.join('instrument')
            view.free()
            view = view_
            view_ = view.subset(args.chan_subset)
            view.free()
            view = view_
            for record in view.iter_record():
                sta, chan, time, endtime, calratio = record.getv('sta',
                                                                 'chan',
                                                                 'time',
                                                                 'endtime',
                                                                 'calratio')
                path = record.filename()[1]
                if sta not in self.responses:
                    self.responses[sta] =  []
                response = MetaResponse(path,
                                       '{}{}'.format(chan[:2], chan[3:]),
                                       time,
                                       endtime)
                found_entry = False
                for (this_response, calratio) in self.responses[sta]:
                    if this_response.chans == response.chans and\
                            this_response.time == response.time and\
                            this_response.endtime == response.endtime:
                        found_entry = True
                        #if this_response.path != response.path:
                            #raise Exception("ResponseInventory Error")
                if not found_entry:
                    self.responses[sta] += [(response, calratio)]
            view.free()

    def get_response(self, sta, chan, time):
        for (response, calratio) in self.responses[sta]:
            if response.is_valid(chan, time):
                return response.path, calratio
        return None

class Response:
    def __init__(self, tr):
        global resp_inv
        sta, chan, time= tr.getv('sta', 'chan', 'time')
        try:
            result = resp_inv.get_response(sta, chan, time)
        except:
            result = None
        if result is not None:
            path, self.calratio = result
        else:
            raise ResponseError("Error reading response")
        self.py_response = respmod.PyResponse(path)

    def eval_response(self, w):
        return self.py_response.eval_response(w)

class Trace:
    def __init__(self, tr, user1=None, user2=None, attach_response=False):
        tr.record = 0
        self.tr = np.array(tr.trdata())
        if self.is_clipped():
            raise ClippedData
        tr.trapply_calib()
        self.tr = np.array(tr.trdata())
        from scipy.signal import detrend
        self.tr = detrend(self.tr, type='linear')
        self.response = None
        self.net = tr.getv('net')[0]
        self.sta = tr.getv('sta')[0]
        self.chan = tr.getv('chan')[0]
        self.time = tr.getv('time')[0]
        self.endtime = tr.getv('endtime')[0]
        self.calib = tr.getv('calib')[0]
        self.calper = tr.getv('calper')[0]
        self.nsamp = tr.getv('nsamp')[0]
        self.samprate = tr.getv('samprate')[0]
        self.dt = 1.0 / self.samprate
        self.user1 = user1
        self.user2 = user2
        if attach_response:
            self.attach_response(tr)

    def attach_response(self, tr):
        self.response = Response(tr)

    def is_clipped(self):
        clip_level = 2**23
        clip_thr = 0.75
        idx = np.nonzero(np.abs(self.tr) >= clip_thr*clip_level)[0]
        if idx.size == 0:
            return False
        else:
            return True

    def calc_spectrum(self, pre_filter=None):
        global pfile
        if self.response == None:
            raise Exception('No Response!')
        # Window larger trace to the correct smaller trace
        arr_time = self.time + 0.5 * pfile['trace_twin']
        from scipy.signal import detrend
        time = arr_time - 0.15
        endtime = time + pfile['spec_twin']
        trdata = self.tr[int((time - self.time) / self.dt) :\
                        int((endtime - self.time) / self.dt)]
        trdata = np.array(trdata)

        # Calculate multitaper power spectral density
        n_fft = next_pow_2(len(trdata))
        freqs, sgnl_spec, _ = multi_taper_psd(trdata, Fs=self.samprate, \
                                              jackknife=False, NFFT=n_fft)
        sgnl_spec = np.sqrt(sgnl_spec)
        f_min = 2.0
        f_max = pfile['freq_max']
        sgnl_spec = sgnl_spec[freqs > 0]
        freqs = freqs[freqs > 0]
        sgnl_spec, freqs = resample_spec(sgnl_spec, freqs, 0.02, f_min, f_max)

        # Removing instrument response with water level deconv
        inst_spec = np.array([self.response.eval_response(2 * pi * f) for \
                              f in freqs])
        inst_spec = np.abs(inst_spec)

        max_inst_spec = inst_spec.max()
        wlevel = pfile['water_level'] * max_inst_spec
        inst_spec = np.array([r if r != 0 and abs(r) > wlevel\
                else wlevel for r in inst_spec])

        spec = Spectrum(sgnl_spec, #/ inst_spec,
                        freqs,
                        sta=self.sta,
                        chan=self.chan)

        return spec

    def trim(self, n):
        self.tr = self.tr[:n]
        self.nsamp = len(self.tr)
        self.endtime = self.time + self.det * (self.nsamp - 1)

    def decimate(self, dec_factor):
        prev = self.samprate
        self.tr= decimate(self.tr, dec_factor)
        self.set_samprate(self.samprate / dec_factor)

    def set_samprate(self, samprate):
        if samprate <= 0:
            raise Exception('Invalid samprate!')
        self.samprate = samprate
        self.dt = 1.0 / samprate
        self.nsamp = len(self.tr)
        self.endtime = self.time + self.nsamp * self.dt

class Arrival():
    '''
    A container class for phase data.
    '''
    def __init__(self,
                 sta,
                 time,
                 phase,
                 chan=None,
                 deltim=None,
                 qual=None,
                 arid=None):
        self.sta = sta
        self.time = time
        self.phase = phase
        self.chan = chan
        self.deltim = deltim
        self.qual = qual
        self.arid = arid
        self.tt_calc = None #calculated travel time
        self.predarr = None #predicted arrival time 

    def __str__(self):
        ret = 'Arrival Object\n--------------\n'
        ret += 'sta:\t\t%s\n' % self.sta
        ret += 'time:\t\t%s\n' % self.time
        ret += 'phase:\t\t%s\n' % self.phase
        ret += 'arid:\t\t%s\n' % self.arid
        ret += 'deltim:\t\t%s\n' % self.deltim
        ret += 'qual:\t\t%s\n'  % self.qual
        ret += 'tt_calc:\t\t%s\n' % self.tt_calc
        ret += 'predarr:\t\t%s\n' % self.predarr
        return ret

class Origin():
    '''
    A container class for earthquake event data. Mirrors the Origin
    table of the CSS3.0 databse schema.
    '''
    def __init__(self,
                 lat,
                 lon,
                 depth,
                 time,
                 auth,
                 arrivals=[],
                 orid=None,
                 evid=None,
                 jdate=None,
                 nass=None,
                 ndef=None,
                 ndp=None,
                 grn=None,
                 srn=None,
                 etype=None,
                 review=None,
                 depdp=None,
                 dtype=None,
                 mb=None,
                 mbid=None,
                 ms=None,
                 msid=None,
                 ml=None,
                 mlid=None,
                 algorithm=None,
                 commid=None,
                 lddate=None,
                 unc_z=None):
        self.lat = lat
        self.lon = lon
        self.depth = depth
        self.time = time
        self.orid = orid
        self.evid = evid
        self.auth = auth
        self.arrivals = arrivals
        self.jdate = jdate
        self.nass = nass
        self.ndef = ndef
        self.ndp = ndp
        self.grn = grn
        self.srn = srn
        self.etype = etype
        self.review = review
        self.depdp = depdp
        self.dtype = dtype
        self.mb = mb
        self.mbid = mbid
        self.ms = ms
        self.msid = msid
        self.ml = ml
        self.mlid = mlid
        self.algorithm = algorithm
        self.commid = commid
        self.lddate = lddate
        self.unc_z = unc_z

def write_moment(db, res):
    tbl_netmag = db.lookup(table='netmag')
    orid, evid, mw, unc, n_phs = res
    magid = tbl_netmag.nextid('magid')
    tbl_netmag.addv(('magid', magid), ('orid', orid), ('evid',evid),
                    ('magtype', 'mw'), ('nsta', n_phs), ('magnitude', mw),
                    ('uncertainty', unc), ('auth', 'dbmoment'))
    return

def detection_output_thread(db):
    global output_q, pfile
    none_count = 0
    while True:
        res = output_q.get(True)
        if res == None:
            none_count += 1
            if none_count == pfile['n_threads']:
                return
        else:
            write_moment(db, res)

def main(_args, _pfile):
    global args, pfile, input_q, output_q, site_md, resp_inv, vmod
    mpl.rcParams['pdf.fonttype'] = 42
    args, pfile = _args, _pfile
    logger.info('Starting...')
    input_q = Queue(maxsize=pfile['max_input_q_size'])
    output_q = Queue(maxsize=pfile['max_output_q_size'])
    site_md = get_site_metadata()
    resp_inv = ResponseInventory()
    processes = []

    vmod = VelModel(args.velocity_model)\
           if args.velocity_model\
           else\
           VelModel("%s/data/velocity/socal" % os.environ['ANTELOPE'])

    with closing(dbopen(args.db, 'r+')) as db:
        reaper = Process(target=origin_reap_thread, args=(db,))
        reaper.start()
        for i in range(pfile['n_threads']):
            p = Process(target=process_origin_thread, args=(db,))
            logger.info("Initializing thread")
            p.start()
            processes += [p]
        output_proc = Process(target=detection_output_thread, args=(db,))
        output_proc.start()
        while True:
            if not reaper.is_alive():
                break
            for i in xrange(len(processes)):
                if not processes[i].is_alive():
                    processes[i] = Process(target=process_origin_thread, \
                                           args=(db,))
                    processes[i].start()
            time.sleep(30)

if __name__ == '__main__':
    # Parse command line options.
    _args = parse_args()
    # Import Antelope functionality to read parameter file.
    from antelope.stock import pfin,\
                               pfread
    # Read parameter file.
    _pfile = parse_pfile(_args.pfile)
    # If a sitedir is specified in parameter file, add it to the search
    # path.
    if _pfile['sitedir']:
        import site
        site.addsitedir(_pfile['sitedir'])
    # Import built-in modules.
    import time
    import logging
    from math import cos, pi
    from multiprocessing import Process, Queue, Pool
    # Import third-party modules
    import numpy as np
    import matplotlib as mpl
    from antelope.datascope import closing,\
                                dbopen,\
                                Dbptr,\
                                trdestroying,\
                                trnew, \
                                TrdataError, \
                                freeing
    from nitime.algorithms.spectral import multi_taper_psd
    from scipy.fftpack import fft,\
                            fftfreq,\
                            ifft,\
                            fftshift,\
                            rfft,\
                            rfftfreq
    from scipy.signal import decimate
    from scipy.interpolate import interp1d
    # Import locally defined modules.
    import ztools.moment as mo
    import ztools.ray.raytracer as rt
    import ztools.response as respmod
    # Configure and retreive logger.
    _args.verbose = "INFO" if not _args.verbose else "DEBUG"
    _configure_logging("dbmw.log", _args.verbose)
    logger = logging.getLogger(__name__)
    # Start main execution.
    main(_args, _pfile)
