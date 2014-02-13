"""
A script to map SMC format files (http://nsmp.wr.usgs.gov/smcfmt.html)
to a css3.0 database.
"""
import antelope.datascope as datascope
from antelope.datascope import dbopen, closing, freeing, Trsave_wfError
import contextlib

class Trsave_wfException(datascope.DatascopeException): pass
datascope.Trsave_wfException = Trsave_wfException
def _read_text_header(infile):
    """
    Read text header, return dictionary of parsed data
    """
    data, lines = {}, []
    #read text header lines into memory
    for line in range(1,12): lines += [infile.readline()]
    #parse first line
    data['data_type'] = lines[0]
    #second line is just a place holder, skip it
    #parse third line
    data['sta_code'], data['trace_id'] = lines[2][:4], lines[2][4]
    #parse fourth line
    data['time_zone'] = lines[3][:3]
    data['origin_y'], data['origin_m'], data['origin_d'], data['origin_hm'], \
        data['event_name'] = lines[3][5:9], lines[3][11:13], \
        lines[3][15:17], lines[3][21:25], lines[3][26:80]
    #parse fifth line
    data['mag_moment'] = lines[4][11:20]
    data['mag_s'] = lines[4][24:33]
    data['mag_l'] = lines[4][37:46]
    #parse sixth line
    data['sta_name'] = lines[5][10:40]
    data['component'] = lines[5][52:57]
    #parse seventh line
    data['epi_dist'] = lines[6][17:26]
    data['pk_acc'] = lines[6][41:50]
    #parse eighth line
    data['inst_type'] = lines[7][10:15]
    data['data_src'] = lines[7][35:80]
    #lines nine, ten and eleven are place holders, skip them
    #strip parsed data of leading and trailing whitespace, map \
    #null values to None, and convert to numeric values where \
    #appropriate
    for key in data:
        data[key] = data[key].rstrip().lstrip()
        if data[key] == '' or data[key] == '*': data[key] = None
        else:
            try: data[key] = eval(data[key])
            except (SyntaxError, NameError): pass
    _log_null_values(data)
    return data

def _read_integer_header(infile):
    """
    Read integer header, return dictionary of parsed data
    """
    data, cells = {}, []
    #read integer header cells into memory
    for line in range(1,7): cells += infile.readline().split()
    #parse first line, cells 1-8
    data['null_value'], data['fsamp_y'], data['fsamp_j'], data['fsamp_h'], \
        data['fsamp_m'], data['fsamp_s'], data['fsamp_milli'], \
        data['rec_serial'] = cells[0:8]
    #parse second line, cells 9-16
    data['chan_num'], data['num_traces_rec'], data['num_traces_sta'], \
        data['sensor_serial'], data['vert_orien'], data['horiz_orien'], \
        data['sensor_type_code'], data['num_comments'] = cells[8:16]
    #parse third line, cells 17- 24
    #cells 23-25 (num_floor - sensor_floor) are for buildings
    #ie. when cell 19 (struct_type) is 1
    data['nsamp'], data['problem_flag'], data['struct_type'], \
        data['struct_num'], data['transducer_num'], data['num_transducer'], \
        data['num_floor'], data['basem_stories'] = cells[16:24]
    #parse fourth line, cells 25-32
    #cells 26, 27 (num_spans, transducer_loc_bridge) are for bridges
    #ie. when cell 19 (struct_type) is 2
    #cells 28, 29 (transducer_loc_dam, construct_type) are for dams
    #ie. when cell 19 (struct_type) is 3
    data['sensor_floor'], data['num_spans'], data['transducer_loc_bridge'], \
        data['transducer_loc_dam'], data['construct_type'], data['sta_num'], \
        data['first_rec_samp'], data['last_rec_samp'] = cells[24:32]
    #parse fifth line, cells 33-40
    data['2nd_file_flag'], data['undef1'], data['undef2'], data['undef3'], \
        data['undef4'], data['undef5'], data['undef6'], data['user_def1'] = \
        cells[32:40]
    #parse sixth line, cells 41-48
    data['user_def2'], data['undef7'], data['undef8'], data['undef9'], \
        data['undef10'], data['undef11'], data['undef12'], data['clock_src'] = \
        cells[40:48]
    #map null values to None, and convert to integer values
    for key in data:
        if key == 'null_value': continue
        if data[key] == data['null_value']: data[key] = None
        else: data[key] = int(data[key])
    return data

def _read_real_header(infile):
    """
    Read real header, return dictionary of parsed data
    """
    data, cells = {}, []
    #read real header cells into memory
    for line in range(1, 11): cells += infile.readline().split()
    #parse first line, cells 1-5
    data['null_value'], data['samprate'], data['origin_lat'], \
            data['origin_lon'], data['origin_depth'] = cells[0:5]
    #parse second line, cells 6-10
    data['mag_moment'], data['mag_s'], data['mag_l'], data['mag_other'], \
        data['moment'] = cells[5:10]
    #parse third line, cells 11-15
    data['sta_lat'], data['sta_lon'], data['sta_elev'], data['sta_offset_E'], \
        data['sta_offset_N'] = cells[10:15]
    #parse fourth line, cells 16-20
    data['sta_offset_up'], data['epi_dist'], data['azm'], \
            data['digitization_const'], data['aaf_corner_freq'] = cells[15:20]
    #parse fifth line, cells 21-25
    data['aaf_poles'], data['sensor_natural_freq'], data['sensor_damp_coeff'], \
        data['recorder_sensitivity'], data['amp_gain'] = cells[20:25]
    #parse sixth line, cells 26-30
    data['preamp_gain'], data['undef1'], data['undef2'], data['max_val_time'], \
            data['max_value'] = cells[25:30]
    #parse seventh line, cells 31-35
    data['min_val_time'], data['min_value'], data['filt_transition1'], \
        data['filt_transition2'], data['filt_transition3'] = cells[30:35]
    #parse eighth line, cells 36-40
    data['filt_transition4'], data['undef3'], data['init_vel_val_post_locut'], \
            data['init_disp_val_post_locut'], data['Vs30'] = cells[35:40]
    #parse ninth line, cells 41-45
    data['undef4'], data['undef5'], data['undef6'], data['undef7'], \
            data['undef8'] = cells[40:45]
    #parse tenth line, cells 46-50
    data['undef9'], data['undef10'], data['undef11'], data['undef12'], \
            data['undef13'] = cells[45:50]
    #map null values to None, and convert to floating point values
    for key in data:
        if key == 'null_value': continue
        if data[key] == data['null_value']: data[key] = None
        else: data[key] = float(data[key])
    return data

def _read_time_series_data(infile, nsamp):
    data = []
    for line in infile:
        data += [float(line[i:i+10]) for i in range(0, 80, 10) \
                if line[i:i+10].rstrip() != '']
    return data

def _write_data(data, dbout):
    _write_origin_event_data(data, dbout)
    _write_wf_data(data, dbout)
    _write_site_data(data, dbout)

def _write_origin_event_data(data, dbout):
    from antelope.stock import epoch2str
    text_hdr, integer_hdr, real_hdr = data['text_header'], \
        data['integer_header'], data['real_header']
    with closing(dbopen(dbout, 'r+')) as db:
        with contextlib.nested(freeing(db.schema_tables['origin']), \
            freeing(db.schema_tables['event'])) as (tbl_origin, tbl_event):
            origin_is_unique = _is_origin_unique(data, tbl_origin)
            if origin_is_unique:
                #add rows to event and origin tables
                orid, evid, time = tbl_origin.nextid('orid'), \
                    tbl_event.nextid('evid'), \
                    _text_hdr_time_2_epoch(text_hdr['origin_y'], \
                    text_hdr['origin_m'], text_hdr['origin_d'], \
                    text_hdr['origin_hm'])
                origin_data = {'lat': real_hdr['origin_lat'], \
                    'lon':  real_hdr['origin_lon'], \
                    'depth': real_hdr['origin_depth'], \
                    'time': time, \
                    'orid': orid, \
                    'evid': evid, \
                    'jdate': int(epoch2str(time, '%Y%j')), \
                    'nass': 0, \
                    'ndef': 0, \
                    'ml': real_hdr['mag_l'], \
                    'mlid': tbl_origin.nextid('mlid'), \
                    'ms': real_hdr['mag_s'], \
                    'msid': tbl_origin.nextid('msid'), \
                    'auth': sys.argv[0]}
                tbl_origin.record = tbl_origin.addnull()
                for field in origin_data:
                    if origin_data[field] == None: continue
                    else: tbl_origin.putv((field, origin_data[field]))
                event_data = {'evid': evid, \
                    'evname': text_hdr['event_name'], \
                    'prefor': orid, \
                    'auth': sys.argv[0]}
                tbl_event.record = tbl_event.addnull()
                for field in event_data:
                    if event_data[field] == None: continue
                    else: tbl_event.putv((field, event_data[field]))

def _write_wf_data(data, dbout):
    from antelope.datascope import destroying, trdestroying, freeing, closing, \
        dbtmp, Trsave_wfError, trnew
    from antelope.stock import str2epoch, epoch2str
    text_hdr, integer_hdr, real_hdr, time_series = data['text_header'], \
        data['integer_header'], data['real_header'], data['time_series']
    time_series = [t*1e7 for t in time_series]
    year, jday, hour, minute, sec, milli = integer_hdr['fsamp_y'], \
        integer_hdr['fsamp_j'], integer_hdr['fsamp_h'], \
        integer_hdr['fsamp_m'], integer_hdr['fsamp_s'], \
        integer_hdr['fsamp_milli']
    if milli == None: milli = 0
    if sec == None: sec = 0
    if minute == None: minute = 0
    if hour == None: hour = 0
    if jday == None: jday = 0
    if year == None: year = 0
    samprate, nsamp = real_hdr['samprate'], integer_hdr['nsamp']
    dt = 1.0/samprate
    pad = 0 if jday < 100 else ''
    time = str2epoch('%s%s%s %d:%d:%d.%d' % \
        (year, pad, jday, hour, minute, sec, milli))
    endtime = time + nsamp*dt
    with contextlib.nested(trdestroying(trnew('temp_tr_db')), \
        closing(dbopen(dbout, 'r+'))) as (tmpdb, db):
        tmptr = tmpdb.schema_tables['trace']
        tbl_wfdisc = db.schema_tables['wfdisc']
        tmptr.record = tmptr.addnull()
        tmptr.trputdata(time_series)
        datatype = 'u4'
        metadata = [('sta', text_hdr['sta_code']), \
            ('chan', _build_chan_code(data)), ('net', 'XX'), \
            ('time', time), ('endtime', endtime), ('nsamp', nsamp), \
            ('samprate', samprate), ('segtype', _get_segtype(data)), \
            ('datatype', datatype), ('calib', _get_calib_value(data))]
        for d in metadata: tmptr.putv(d)
        try:
            tmptr.trsave_wf(tbl_wfdisc, datatype=datatype)
        except (NameError, Trsave_wfError) as e:
            print e

def _write_site_data(data, dbout):
    text_hdr, integer_hdr, real_hdr = data['text_header'], \
        data['integer_header'], data['real_header']
    if _is_site_unique(data, dbout):
        lat, lon, elev = real_hdr['sta_lat'], real_hdr['sta_lon'], real_hdr['sta_elev']
        if lat == None: lat = -999.0000
        if lon == None: lon = -999.0000
        if elev == None: elev = -999.0000
        data = [('sta', text_hdr['sta_code']), ('lat', lat), \
            ('lon', lon), ('elev', elev), ('staname', text_hdr['sta_name']), \
            ('ondate', 1900001), ('offdate', 2100000)]
        with closing(dbopen(dbout, 'r+')) as db:
            tbl_site = db.schema_tables['site']
            tbl_site.record = tbl_site.addnull()
            for d in data:
                tbl_site.putv(d)

def _is_origin_unique(data, tbl_origin):
    text_hdr, integer_hdr, real_hdr = data['text_header'], \
        data['integer_header'], data['real_header']
    lat, lon, depth = real_hdr['origin_lat'], real_hdr['origin_lon'], \
        real_hdr['origin_depth']
    if lat == None: lat = -999.0000
    if lon == None: lon = -999.0000
    if depth == None: depth = -999.0000
    year, month, day, hm = text_hdr['origin_y'], text_hdr['origin_m'], \
        text_hdr['origin_d'], text_hdr['origin_hm']
    time = _text_hdr_time_2_epoch(year, month, day, hm)
    with freeing(tbl_origin.subset('lat == %f && lon == %f && depth == %f '\
        '&& time == %f' % (lat, lon, depth, time))) as tbl_subset:
        if tbl_subset.record_count == 0: return True
        else: return False

def _is_site_unique(data, dbout):
    text_hdr, integer_hdr, real_hdr = data['text_header'], \
        data['integer_header'], data['real_header']
    sta_code, lat, lon, elev = text_hdr['sta_code'], real_hdr['sta_lat'], \
        real_hdr['sta_lon'], real_hdr['sta_elev']
    if lat == None: lat = -999.0000
    if lon == None: lon = -999.0000
    if elev == None: elev = -999.0000
    with closing(dbopen(dbout, 'r+')) as db:
        tbl_site = db.schema_tables['site']
        tbl_site = tbl_site.subset('sta =~ /%s/ && lat == %f && lon == %f ' \
            '&& elev == %f' % (sta_code, lat, lon, elev))
        if tbl_site.record_count == 0: return True
        else: return False

def _text_hdr_time_2_epoch(year, month, day, hm):
    from antelope.stock import str2epoch
    return str2epoch('%d/%d/%d  %d:%d:00.0' % (month, day, year, hm/100, hm%100))

def _build_chan_code(data):
    samprate = data['real_header']['samprate']
    if samprate < 0.000001: band_code = 'Q'
    elif samprate >= 0.000001 and samprate < 0.00001: band_code = 'T'
    elif samprate >= 0.00001 and samprate < 0.0001: band_code = 'P'
    elif samprate >= 0.0001 and samprate < 0.001: band_code = 'R'
    elif samprate >= 0.001 and samprate < 0.05: band_code = 'U'
    elif samprate >= 0.05 and samprate < 0.5: band_code = 'V'
    elif samprate >= 0.5 and samprate <= 1: band_code = 'L'
    elif samprate > 1 and samprate < 10: band_code = 'M'
    elif samprate >= 10 and samprate < 80: band_code = 'B'
    elif samprate >= 80 and samprate < 250: band_code = 'H'
    data_type = data['text_header']['data_type']
    if data_type == None: instrument_code = 'N'
    elif data_type[0] == '0': instrument_code = 'N'
    elif data_type[0] == '1': instrument_code = 'N'
    elif data_type[0] == '2': instrument_code = 'N'
    elif data_type[0] == '3': instrument_code = 'H'
    elif data_type[0] == '4': instrument_code = 'H' #this is not the correct code...
    else: raise(Exception('Conversion of response/Fourier amplitude spectra not ' \
        'currently supported.'))
    component = data['text_header']['component']
    hang = data['integer_header']['horiz_orien']
    if component == 'up': component = 'Z'
    elif hang == 0 or hang == 180: component = 'N'
    elif hang == 90 or hang == 270: component = 'E'
    elif (hang > 0 and hang < 90) or (hang > 180 and hang < 270): \
        component = '1'
    elif (hang > 90 and hang < 180) or (hang > 270 and hang < 360): \
        component = '2'
    return '%s%s%s' % (band_code, instrument_code, component)

def _get_segtype(data):
    data_type = data['text_header']['data_type']
    if data_type == None: segtype = 'A'
    elif data_type[0] == '0': segtype = 'A'
    elif data_type[0] == '1': segtype = 'A'
    elif data_type[0] == '2': segtype = 'A'
    elif data_type[0] == '3': segtype = 'V'
    elif data_type[0] == '4': segtype = 'V' #this is not the correct code...
    else: raise(Exception('Conversion of response/Fourier amplitude spectra not ' \
        'currently supported.'))
    return segtype

def _get_calib_value(data):
    text_hdr, integer_hdr, real_hdr = data['text_header'], \
        data['integer_header'], data['real_header']
    return 1

def _process_file(infile, dbout):
    print 'processing %s' % infile
    data = {}
    data['text_header'] = _read_text_header(infile)
    data['integer_header'] = _read_integer_header(infile)
    data['real_header'] = _read_real_header(infile)
    data['time_series'] = \
        _read_time_series_data(infile, data['integer_header']['nsamp'])
    _write_data(data, dbout)

def _parse_command_line():
    from argparse import ArgumentParser
    parser = ArgumentParser()
    parser.add_argument('input_file', type=str, nargs='+', \
        help='File to process')
    parser.add_argument('dbout', type=str, help='Output database')
    args = parser.parse_args()
    return args


#def _configure_logging(logfile=False):
#    import logging
#    logger = logging.getLogger(sys.argv[0])
#    logger.setLevel(logging.DEBUG)
#    if logfile:
#        fh = logging.FileHandler(logfile)
#        fh.setLevel(logging.INFO)
#        logger.addHandler(fh)
#    sh = logging.StreamHandler()
#    sh.setLevel(logging.ERROR)
#    logger.addHandler(sh)
#    return logger

def _log_null_values(data):
    """Log null values read from SMC file."""
    #use the 'inspect' module to get a stack trace
    pass

def _main():
    args = _parse_command_line()
#    logger = _configure_logging()
    dbout = args.dbout
    for f in args.input_file:
        with open(f, 'r') as infile:
            _process_file(infile, dbout)

if __name__ == '__main__': sys.exit(_main())
else: sys.exit('Not a module to import!!')
