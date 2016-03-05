import logging
import time
import subprocess
import shutil
from loctools3D.core_tools import Station, eval_dict
from math import pi

tt_calculator = 'fm3d'

def _configure_logging(logfile='generate_ttimes_fm3d.log', level=None):
    import logging
    if level == None:
        level = logging.INFO
    elif level.upper() == 'DEBUG':
        level = logging.DEBUG
    else:
        level = logging.INFO
    for name in (__name__,
                 'loctools3D.core_tools'):
        logger = logging.getLogger(name)
        logger.setLevel(level)
        if level == logging.DEBUG:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::'\
                    '%(funcName)s:: %(message)s',
                                          datefmt='%Y%j %H:%M:%S')
        else:
            formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s:: '\
                    '%(message)s',
                                          datefmt='%Y%j %H:%M:%S')
        file_handler = logging.FileHandler(logfile)
        file_handler.setLevel(level)
        file_handler.setFormatter(formatter)
        logger.addHandler(file_handler)
        stream_handler = logging.StreamHandler()
        stream_handler.setLevel(level)
        stream_handler.setFormatter(formatter)
        logger.addHandler(stream_handler)

def _parse_args():
    from argparse import ArgumentParser
    from os import getcwd
    from os.path import join
    parser = ArgumentParser()
    parser.add_argument('db', type=str, help='input database')
    parser.add_argument('-p', '--pf', type=str, help='parameter file')
    parser.add_argument('-s', '--subset', type=str, help='station subset')
    parser.add_argument('-o', '--output', type=str, help='output directory')
    parser.add_argument('-t', '--ttgrid_output', action='store_true', help='output travel-times in Antelope ttgrid format')
    args = parser.parse_args()
    args.db = join(getcwd(), args.db)
    return args

def _parse_pfile(pfile):
    import re
    from os.path import splitext
    from antelope.stock import pfin, pfread
    if pfile == None:
        pfile =  eval_dict(pfread('3Drelocate').pf2dict())
    else:
        if splitext(pfile)[1] != '.pf':
            pfile = '%s.pf' % pfile
        pfile =  eval_dict(pfin(pfile).pf2dict())
    pattern = re.compile(r'\$\{[A-Za-z]+\}')
    for key in ('frechet.in',
                'gridsave.in',
                'interfaces.in',
                'mode_set.in',
                'vgrids_P',
                'vgrids_S'):
        string = pfile[key]
        match = pattern.search(string)
        while match != None:
            string = '%s%s%s'\
                    % (string[:match.start()],
                       os.environ[string[match.start() + 2: match.end() - 1]],
                       string[match.end():])
            match = pattern.search(string)
        pfile[key] = string
    return pfile

def _check_pfile(pfile):
    exit_flag = False
    prop_grid = pfile['propagation_grid']
    for vgrids_infile in [open(pfile[vgrid], 'r') for vgrid in ('vgrids_P', 'vgrids_S')]:
        vgrids_infile.readline()
        vgrids_nr, vgrids_nlat, vgrids_nlon = vgrids_infile.readline().split()
        vgrids_dr, vgrids_dlat, vgrids_dlon = vgrids_infile.readline().split()
        vgrids_minr, vgrids_minlat, vgrids_minlon = vgrids_infile.readline().split()
        vgrids_infile.close()
        vgrids_nr = int(vgrids_nr)
        vgrids_nlat = int(vgrids_nlat)
        vgrids_nlon = int(vgrids_nlon)
        vgrids_dr = float(vgrids_dr)
        vgrids_dlat = float(vgrids_dlat) * 180.0 / pi
        vgrids_dlon = float(vgrids_dlon) * 180.0 / pi
        vgrids_minr = float(vgrids_minr)
        vgrids_minlat = float(vgrids_minlat) * 180.0 / pi
        vgrids_minlon = float(vgrids_minlon) * 180.0 / pi
        vgrids_maxr = vgrids_minr + (vgrids_nr - 1) * vgrids_dr
        vgrids_maxlat = vgrids_minlat + (vgrids_nlat - 1) * vgrids_dlat
        vgrids_maxlon = vgrids_minlon + (vgrids_nlon - 1) * vgrids_dlon
        vgrids_violation_msg = "Propagation grid must be entirely contained by the"\
                " second layer of the velocity grid. Edit the parameter file to "\
                "satisfy the below requirement(s):"
        print_vgrids_violation_msg = True
        if prop_grid['minr'] < (vgrids_minr + vgrids_dr):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minr'] <= %.4f" \
                    % ((vgrids_minr + vgrids_dr), (vgrids_maxr - vgrids_dr)))
        if prop_grid['minr'] + (prop_grid['nr'] - 1) * prop_grid['dr'] > \
                    (vgrids_maxr - vgrids_dr):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minr']  + "\
                    "(propagation_grid['nr'] - 1) * propagation_grid['dr'] <= "\
                    "%.4f" % ((vgrids_minr + vgrids_dr), (vgrids_maxr - vgrids_dr)))
        if prop_grid['minlat'] < (vgrids_minlat + vgrids_dlat):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minlat'] <= %.4f" \
                    % ((vgrids_minlat + vgrids_dlat), (vgrids_maxlat - vgrids_dlat)))
        if prop_grid['minlat'] + (prop_grid['nlat'] - 1) * prop_grid['dlat'] > \
                    (vgrids_maxlat - vgrids_dlat):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minlat']  + "\
                    "(propagation_grid['nlat'] - 1) * propagation_grid['dlat'] <= "\
                    "%.4f" % ((vgrids_minlat + vgrids_dlat),
                            (vgrids_maxlat - vgrids_dlat)))
        if prop_grid['minlon'] < (vgrids_minlon + vgrids_dlon):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minlon'] <= %.4f" \
                    % ((vgrids_minlon + vgrids_dlon), (vgrids_maxlon - vgrids_dlon)))
        if prop_grid['minlon'] + (prop_grid['nlon'] - 1) * prop_grid['dlon'] > \
                    (vgrids_maxlon - vgrids_dlon):
            exit_flag = True
            if print_vgrids_violation_msg:
                logger.error(vgrids_violation_msg)
                print_vgrids_violation_msg = False
            logger.info("\t%.4f <= propagation_grid['minlon']  + "\
                    "(propagation_grid['nlon'] - 1) * propagation_grid['dlon'] <= "\
                    "%.4f" % ((vgrids_minlon + vgrids_dlon),
                            (vgrids_maxlon - vgrids_dlon)))
    interfaces_infile = open(pfile['interfaces.in'], 'r')
    interfaces_infile.readline()
    interfaces_nlat, interfaces_nlon = interfaces_infile.readline().split()
    interfaces_dlat, interfaces_dlon = interfaces_infile.readline().split()
    interfaces_minlat, interfaces_minlon = interfaces_infile.readline().split()
    interfaces_infile.close()
    interfaces_nlat = int(interfaces_nlat)
    interfaces_nlon = int(interfaces_nlon)
    interfaces_dlat = float(interfaces_dlat) * 180.0 / pi
    interfaces_dlon = float(interfaces_dlon) * 180.0 / pi
    interfaces_minlat = float(interfaces_minlat) * 180.0 / pi
    interfaces_minlon = float(interfaces_minlon) * 180.0 / pi
    interfaces_maxlat = interfaces_minlat + (interfaces_nlat - 1) \
            * interfaces_dlat
    interfaces_maxlon = interfaces_minlon + (interfaces_nlon - 1) \
            * interfaces_dlon
    interfaces_violation_msg = "Propagation grid must be entirely contained "\
            "by the second layer of the interfaces grid. Edit the parameter "\
            "file to satisfy the below requirement(s):"
    print_interfaces_violation_msg = True
    if prop_grid['minlat'] < (interfaces_minlat + interfaces_dlat):
        exit_flag = True
        if print_interfaces_violation_msg:
            logger.error(interfaces_violation_msg)
            print_interfaces_violation_msg = False
        logger.info("\t%.4f <= propagation_grid['minlat'] <= %.4f" \
                % ((interfaces_minlat + interfaces_dlat),
                   (interfaces_maxlat - interfaces_dlat)))
    if prop_grid['minlat'] + (prop_grid['nlat'] - 1) * prop_grid['dlat'] > \
                (interfaces_maxlat - interfaces_dlat):
        exit_flag = True
        if print_interfaces_violation_msg:
            logger.error(interfaces_violation_msg)
            print_interfaces_violation_msg = False
        logger.info("\t%.4f <= propagation_grid['minlat']  + "\
                "(propagation_grid['nlat'] - 1) * propagation_grid['dlat'] <= "\
                "%.4f" % ((interfaces_minlat + interfaces_dlat),
                          (interfaces_maxlat - interfaces_dlat)))
    if prop_grid['minlon'] < (interfaces_minlon + interfaces_dlon):
        exit_flag = True
        if print_interfaces_violation_msg:
            logger.error(interfaces_violation_msg)
            print_interfaces_violation_msg = False
        logger.info("\t%.4f <= propagation_grid['minlon'] <= %.4f" \
                % ((interfaces_minlon + interfaces_dlon),
                   (interfaces_maxlon - interfaces_dlon)))
    if prop_grid['minlon'] + (prop_grid['nlon'] - 1) * prop_grid['dlon'] > \
                (interfaces_maxlon - interfaces_dlon):
        exit_flag = True
        if print_interfaces_violation_msg:
            logger.error(interfaces_violation_msg)
            print_interfaces_violation_msg = False
        logger.info("\t%.4f <= propagation_grid['minlon']  + "\
                "(propagation_grid['nlon'] - 1) * propagation_grid['dlon'] <= "\
                "%.4f" % ((interfaces_minlon + interfaces_dlon),
                          (interfaces_maxlon - interfaces_dlon)))
    if exit_flag:
        sys.exit(-1)

def _create_station_list(args, pfile):
    from antelope.datascope import closing, dbopen
    dbpath = args.db
    subset = args.subset
    prop_grid = pfile['propagation_grid']
    minlon = float(prop_grid['minlon'])
    dlon = float(prop_grid['dlon'])
    nlon = int(prop_grid['nlon'])
    minlat = float(prop_grid['minlat'])
    dlat = float(prop_grid['dlat'])
    nlat = int(prop_grid['nlat'])
    maxlon = minlon + dlon * (nlon - 1)
    maxlat = minlat + dlat * (nlat - 1)
    station_list = []
    with closing(dbopen(dbpath, 'r')) as db:
        tbl_site = db.schema_tables['site']
        if subset: tbl_site = tbl_site.subset(subset)
        subset = 'lat > %f && lat < %f && lon > %f && lon < %f' \
                % (minlat, maxlat, minlon, maxlon)
        tbl_site = tbl_site.subset(subset)
        tbl_site = tbl_site.sort('sta', unique=True)
        for record in tbl_site.iter_record():
            sta, lat, lon, elev = record.getv('sta',
                                              'lat',
                                              'lon',
                                              'elev')
            station_list += [Station(sta, lat, lon, elev)]
    return station_list

def _write_propgrid(pfile):
    refinement_factor = 5
    ncells = 10
    prop_grid = pfile['propagation_grid']
    if os.path.isfile('propgrid.in'):
        os.remove('propgrid.in')
    outfile = open('propgrid.in', 'w')
    outfile.write('%8s\t%8s\t%8s\t\t\t%s\n'
            % (prop_grid['nr'],
               prop_grid['nlat'],
               prop_grid['nlon'],
               '# of propagation grid points in r, lat and lon'))
    outfile.write('%8s\t%8s\t%8s\t\t%s\n'
            % (prop_grid['dr'],
               prop_grid['dlat'],
               prop_grid['dlon'],
               'grid intervals in r (km) lat,long (deg)'))
    outfile.write('%8s\t%8s\t%8s\t\t%s\n'
            % ((prop_grid['minr'] + (prop_grid['nr'] - 1)  * prop_grid['dr'] \
                    - pfile['earth_radius']),
               prop_grid['minlat'],
               prop_grid['minlon'],
               'origin of the grid height (km),lat,long (deg)'))
    outfile.write('%8s\t%8s\t\t\t\t\t%s\n'
            % (refinement_factor,
               ncells,
               'refinement factor and # of propgrid cells in refined source '\
                    'grid'))
    outfile.close()

def _configure_logger():
    logger = logging.getLogger(sys.argv[0])
    logger.setLevel(logging.DEBUG)
    formatter = logging.Formatter(fmt='%(asctime)s::%(levelname)s::%(message)s',
        datefmt='%Y%j %H:%M:%S')
    fh = logging.FileHandler('%s.log' % sys.argv[0])
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(formatter)
    logger.addHandler(fh)
    sh = logging.StreamHandler()
    sh.setLevel(logging.INFO)
    sh.setFormatter(formatter)
    logger.addHandler(sh)

def gen_sta_tt_maps(station_list, phase, if_write_binary=True):
#Generate a travel time map for each station in the station list
    logger.info('Starting travel time calculation for %s phase.' % phase)
    for sta in sorted(station_list):
        logger.info('[sta: %s] Generating %s-wave travel times.' %  (sta.sta, phase))
#Elevation can be set to a large negative number to glue the source to
#the surface
#Elevation is in km and negative
        #_write_sources_file(sta.elev * -1, sta.lat, sta.lon)
        _write_sources_file(0.0, sta.lat, sta.lon) #!!!! SET SOURCE TO 0 DEPTH
        tt_calc_subprocess = subprocess.Popen(tt_calculator,
                                              stdout=subprocess.PIPE,
                                              stderr=subprocess.STDOUT)
        for line in tt_calc_subprocess.stdout:
            logger.info('[sta: %s] %s' % (sta.sta, line.rstrip()))
        outfile = '%s.%s.traveltime' % (sta.sta, phase)
        try:
            logger.info('[sta: %s] Moving arrtimes.dat to %s' % (sta.sta, outfile))
            shutil.move('arrtimes.dat', outfile)
        except IOError:
            logger.error('[sta: %s] Could not move arrtimes.dat to %s'\
                    % (sta.sta, outfile))
            continue
        if if_write_binary:
            try:
                _tt_ascii_2_binary(sta.sta, phase)
            except IndexError:
                logger.error('[sta: %s] Could not write binary file' % sta.sta)
        logger.info('[sta: %s] Finished travel time calculation.' % sta.sta)

def _write_sources_file(depth, lat, lon):
    #Write  the sources.in file, which has this form:
    # 1                                number of sources
    # 0                                source is local/teleseismic (0/1)
    # 5.00  33.0   -116.0      position depth(km),lat(deg),long(deg)
    # 1                                number of paths from this source
    # 1                                number of sections on the path
    # 0 1           define the path sections
    # 1            define the velocity type along the path
    numsrc = 1
    teleflag = 0
    numpaths = 1
    numsections = 1
    veltype = 1
    outfile = open('sources.in','w')
    outfile.write(" %i\n %i\n %.4f %.4f %.4f\n %i\n %i\n %i %i\n %i\n"
        % (numsrc, teleflag, depth, lat, lon, numpaths, numsections, 0, 2,
        veltype))
    outfile.close()
    #Path sections are described in more detail in the FMM README
    # For the first arrival from a source propagating at the surface [ 0 2 ]
    #  is the path section

def _tt_ascii_2_binary(sta, phase):
    '''
    Convert an ascii traveltime file to binary format
    Also puts the header in a separate ascii file
    just put "bin" and "hdr" in front of the filename
    '''
    from array import array
    infile = '%s.%s.traveltime' % (sta, phase)
    bin_path = 'bin.%s' % infile
    hdr_path = 'hdr.%s' % infile
    logger.info('[sta: %s] Reading arrival times from %s' % (sta, infile))
    #Open and read header
    infile = open('%s' % infile, 'r')
#Number of grid points
    tmp = infile.readline().strip().split()
    nz = eval(tmp[0])
    nlat = eval(tmp[1])
    nlon = eval(tmp[2])
#Spacing of grid points
    tmp = infile.readline().strip().split()
    dz = eval(tmp[0])
    dlat = eval(tmp[1])
    dlon = eval(tmp[2])
#Origin of grid points
    tmp = infile.readline().strip().split()
    oz = eval(tmp[0])
    olat = eval(tmp[1])
    olon = eval(tmp[2])
    tmp = infile.readline().strip().split()
    narr = eval(tmp[0])                         #number of sets of arrival times
    tmp = infile.readline().strip().split()
    null1 = eval(tmp[0])
    null2 = eval(tmp[1])
    null3 = eval(tmp[2])                         #source and path for arrival time ???
#Now read the traveltimes into a list
    lines = infile.readlines()
    data = []
    for line in lines:
        data.append( float(line) )
    infile.close()
#Now output in binary format
    outfile = open(bin_path,'w')
#It is apparently faster to turn this into an array
    out_array = array('d',data)
    out_array.tofile(outfile)
    outfile.close()

#Now Write the header
    outfile = open(hdr_path,'w')
    outfile.write(" %i %i %i\n %.4f %.6f %.6f\n %.5f %.5f %.5f\n %i\n %i %i "\
            "%i" %\
            (nz,
             nlat,
             nlon,
             dz,
             dlat,
             dlon,
             oz,
             olat,
             olon,
             narr,
             null1,
             null2,
             null3))
    outfile.close()
    logger.info('[sta: %s] Finished writing %s-wave arrival times to %s and %s' \
            % (sta, phase, bin_path, hdr_path))
    #return data #Don't forget to remove this!!

def read_arrtimes(fnam='arrtimes.dat',ifplot=0): #default name
    '''
    Read arrival times from file fnam
    '''
    logger.info('Reading arrival times from %s' % fnam)
#Open and read header
    fid = open(fnam,'r')
#Number of grid points
    tmp = fid.readline().strip().split()
    nz = eval(tmp[0])
    nlat = eval(tmp[1])
    nlon = eval(tmp[2])
#Spacing of grid points
    tmp = fid.readline().strip().split()
    dz = eval(tmp[0])
    dlat = eval(tmp[1])
    dlon = eval(tmp[2])
#Origin of grid points
    tmp = fid.readline().strip().split()
    oz = eval(tmp[0])
    olat = eval(tmp[1])
    olon = eval(tmp[2])
    fid.readline()
    fid.readline()
#Now read the traveltimes and sort into a matrix
    data = empty((nlat, nlon, nz))
    for ix in range(nlon):
        for iy in range(nlat):
            for iz in range(nz):
                tmp = float(fid.readline().strip())
                data[iy, ix, iz] = tmp
    fid.close()
#Create vectors of geographic coordinates
    elon = olon + (dlon * nlon);
    elat = olat + (dlat * nlat);
    lonvec = linspace(olon, elon, nlon);
    latvec = linspace(olat, elat, nlat);
#Now plot
    if ifplot:
        flon,flat = load_faults()
        plt.figure(figsize=(6, 6))
        plt.plot(flon, flat, 'k')
        plt.axis([-118, -115, 32, 35])
        plt.pcolor(lonvec, latvec, data[:, :, 6])
    return data

def _create_dummy_receivers_file(pfile):
    outfile = open('receivers.in', 'w')
    outfile.write('1\n')
    prop_grid = pfile['propagation_grid']
    minlat = prop_grid['minlat']
    minlon = prop_grid['minlon']
    maxz = pfile['earth_radius'] - prop_grid['minr']
    maxlat = minlat + (prop_grid['nlat'] - 1) * prop_grid['dlat']
    maxlon = minlon + (prop_grid['nlon'] - 1) * prop_grid['dlon']
    minz = maxz - (prop_grid['nr'] - 1) * prop_grid['dr']
    midlat = (minlat + maxlat) / 2.0
    midlon = (minlon + maxlon) / 2.0
    midz = (minz + maxz) / 2.0
    outfile.write('%f    %f    %f\n1\n1\n1' % (midz, midlat, midlon))
    outfile.close()

def write_antelope_ttgrid(station_list, calc_S_tt, pfile):
    prop_grid = pfile['propagation_grid']
    ttgrid_compile_input = open('ttgrid_compile_input', 'w')
    ttgrid_compile_input.write('name\tfm3d_grid\n')
    ttgrid_compile_input.write('method\tfm3d\n')
    ttgrid_compile_input.write('model\tdefault\n')
    ttgrid_compile_input.write('P\tyes\n')
    if calc_S_tt:
        ttgrid_compile_input.write('S\tyes\n')
    else:
        ttgrid_compile_input.write('S\tno\n')
    ttgrid_compile_input.write('stations\t%d\n' % len(station_list))
    for sta in station_list:
        ttgrid_compile_input.write('%s\t%f\t%f\t%f\n' % (sta.sta, sta.lat, sta.lon, sta.elev))
    #count the number of nodes along the z-axis with depth >= 0
    nr = len([1 for ir in range(prop_grid['nr']) if (pfile['earth_radius'] -\
            (prop_grid['minr']+ ir * prop_grid['dr'])) >= 0])
    #ttgrid_compile_input.write('sources\t%d\n' % \
    #        (prop_grid['nlat'] * prop_grid['nlon'] * prop_grid['nr']))
    ttgrid_compile_input.write('sources\t%d\n' % \
            (prop_grid['nlat'] * prop_grid['nlon'] * nr))
    for ilon in range(prop_grid['nlon']):
        for ilat in range(prop_grid['nlat']):
            #for ir in range(prop_grid['nr']):
            for ir in range(nr):
                ttgrid_compile_input.write('%f\t%f\t%f\n' % \
                        (prop_grid['minlat'] + ilat * prop_grid['dlat'],
                         prop_grid['minlon'] + ilon * prop_grid['dlon'],
                         pfile['earth_radius'] - (prop_grid['minr'] \
                                 + ir * prop_grid['dr'])))
    ttgrid_compile_input.write('ttimes\n')
    for ilon in range(prop_grid['nlon']):
        for ilat in range(prop_grid['nlat']):
            #for ir in range(prop_grid['nr']):
            for ir in range(nr):
                logger.info('Writing travel times for node at {} {} {}.'.format(
                    prop_grid['minlat'] + ilat * prop_grid['dlat'],
                    prop_grid['minlon'] + ilon * prop_grid['dlon'],
                    pfile['earth_radius'] - (prop_grid['minr'] \
                        + ir * prop_grid['dr'])))
                for sta in station_list:
                    if calc_S_tt:
                        P_ttfile = open('%s.P.traveltime' % sta.sta, 'r')
                        S_ttfile = open('%s.S.traveltime' % sta.sta, 'r')
                        for j in range(ir + ilat + ilon + 5):
                            P_ttfile.readline()
                            S_ttfile.readline()
                        P_tt = float(P_ttfile.readline())
                        S_tt = float(S_ttfile.readline())
                        P_ttfile.close()
                        S_ttfile.close()
                        if P_tt == -1.0:
                            P_tt = 100.0
                        if S_tt == -1.0:
                            S_tt = 100.0
                        ttgrid_compile_input.write('%f\t%f\t' % (P_tt, S_tt))
                    else:
                        P_ttfile = open('%s.P.traveltime' % sta.sta, 'r')
                        for j in range(ir + ilat + ilon + 5):
                            P_ttfile.readline()
                        P_tt = float(P_ttfile.readline())
                        P_ttfile.close()
                        if P_tt == -1.0:
                            P_tt = 100.0
                        ttgrid_compile_input.write('%f\t' % P_tt)
                ttgrid_compile_input.write('\n')
    ttgrid_compile_input.close()
    subprocess.call(["ttgrid_compile < ttgrid_compile_input > ttgrid"], shell=True)

if __name__ == '__main__':
    from logging import getLogger
    args = _parse_args()
    _configure_logging()
    logger = getLogger(__name__)
    try:
        if args.output:
            tt_dir = args.output
        else:
            tt_dir = 'tt_maps_%d' % int(time.time())
        logger.info('Creating working directory - %s.' % tt_dir)
        if os.path.exists(tt_dir):
            logger.error('Directory %s already exists. Aborting.' % tt_dir)
            sys.exit(-1)
        os.mkdir(tt_dir)
    except OSError as err:
        logger.error('Could not create working directory. Make sure you have '\
                'write permission  for %s' % tt_dir)
        logger.error(err)
        sys.exit(-1)
    try:
        os.chdir(tt_dir)
    except OSError as err:
        logger.error('Could not navigate to working directory.')
        logger.error(err)
        sys.exit(-1)
    pfile = _parse_pfile(args.pf)
    logger.info('Checking parameter file.')
    _check_pfile(pfile)
    station_list = _create_station_list(args, pfile)
    print sorted([sta.sta for sta in station_list])
    input_files = ('mode_set.in',
                   'interfaces.in',
                   'gridsave.in',
                   'frechet.in')
    for input_file in input_files:
        try:
            logger.info('Copying %s to working directory' % pfile[input_file])
            shutil.copyfile(pfile[input_file], input_file)
        except IOError as err:
            logger.error('Could not copy %s to working directory' % input_file)
            sys.exit(-1)
    logger.info('Creating dummy receivers.in file.')
    _create_dummy_receivers_file(pfile)
    _write_propgrid(pfile)
    for phase in ('P', 'S'):
        input_file = 'vgrids_%s' % phase
        try:
            logger.info('Copying %s to working directory' % pfile[input_file])
            shutil.copyfile(pfile[input_file], 'vgrids.in')
        except IOError as err:
            logger.error('Could not copy %s to working directory' % input_file)
            sys.exit(-1)
        gen_sta_tt_maps(station_list, phase)
    for input_file in input_files + ('vgrids.in',
                                     'arrivals.dat',
                                     'frechet.dat',
                                     'propgrid.in',
                                     'receivers.in',
                                     'sources.in'):
        try:
            logger.info('Removing %s from working directory' % input_file)
            os.remove(input_file)
        except IOError as err:
            logger.error('Could not remove %s from working directory' %\
                    input_file)
    if args.ttgrid_output:
        logger.info('Converting travel-time files to Antelope ttgrid format...'\
                ' This is slow... Very slow.')
        write_antelope_ttgrid(station_list, pfile['calc_S_tt'], pfile)
    sys.exit(0)
