'''
A submodule to provide core functionality and classes needed by various
software components in the 3DSeisTools package.
'''
if 'os' not in locals(): import os
if 'time' not in locals(): import time
if 'logging' not in locals(): import logging
if 'struct' not in locals(): import struct
from math import sqrt
from numpy import append,\
                  arange,\
                  array,\
                  asarray,\
                  c_,\
                  dot,\
                  empty,\
                  linalg,\
                  linspace,\
                  nonzero,\
                  ones,\
                  zeros
import numpy as np

logger = logging.getLogger(__name__)

class LinearIndex:
    '''
    A class to convert between 1D and 3D indices. The z-index varies
    fastest, y-index second fastest and x-index slowest.
    '''
    def __init__(self, nx, ny, nz):
        import numpy as np
        self.index_1D = np.zeros(nx * ny * nz, dtype=np.ndarray)
        self.index_3D = np.zeros([nx, ny, nz], dtype=np.int)
        self.nx = nx
        self.ny = ny
        self.nz = nz
        i, x = 0, 0
        while x < nx:
            y = 0
            while y < ny:
                z = 0
                while z < nz:
                    self.index_1D[i] = [x, y, z]
                    self.index_3D[x, y, z] = i
                    i += 1
                    z += 1
                y += 1
            x += 1

    def convert_to_1D(self, x, y, z):
        '''
        Given a 3D index, return the corresponding 1D index.
        '''
        return self.index_3D[x, y, z]

    def convert_to_3D(self, i):
        '''
        Given a 1D index, return the corresponding 3D index.
        '''
        return self.index_1D[i]

def grid_search_dbgrassoc(qx, qy, qz, arrivals, tt_map_dir):
    from antelope.datascope import destroying, dbDATABASE_NAME
    import subprocess
#Create temporary database detection table
    with destroying(dbtmp(schema='CSS3.0')) as db:
        tbl_detection = db.lookup(table='detection')
        for arrival in arrivals:
            tbl_detection.record = tbl_detection.addnull()
            tbl_detetction.putv(('sta', arrival.sta),
                                ('chan', 'YYY'),
                                ('time', arrival.time),
                                ('state', 'a'),
                                ('filter', 'None'))
        subprocess.call(["dbgrassoc",
                         "-pf tmp_dbgrassoc.pf",
                         db.query(dbDATABASE_NAME),
                         db.query(dbDATABASE_NAME),
                         "%s/ttgrid" % tt_map_dir])
        tbl_origin = db.lookup(table='origin')
        for record in tbl_origin.iter_record():
            print record.getv('lat', 'lon', 'depth', 'time', 'auth')
        tbl_origin.record = 0
        lon, lat, depth, time = tbl_origin.getv('lon', 'lat', 'depth', 'time')
        ix = find_nearest_index(lon, qx)[0]
        iy = find_nearest_index(lat, qy)[0]
        iz = find_nearest_index(depth, qz)[0]
        return ix, iy, iz, time

def parse_cfg(config_file):
    '''
    Parse .cfg configuration file and return dictionary of contents.

    Arguments:
    config_file - Path to configuration file.

    Returns:
    mydict - Dictionary of parameters parsed from config_file.

    Example:
    In [1]: from loctools3D.core_tools import parse_cfg

    In [2]: cfg_dict = parse_cfg('test_pf_2_cfg.cfg')

    In [3]: print cfg_dict
    {'misc': {'earth_radius': 6371.0,
              'tt_map_dir': '/Users/mcwhite/staging/tt_maps/June2010/'
             },
             'propagation_grid': {'minlon': -117.80,
                                  'dlon': 0.0327,
                                  'nlat': 76,
                                  'minlat': 32.5,
                                  'minz': 3.0,
                                  'dlat': 0.0273,
                                  'nr': 25,
                                  'dr': 2.0,
                                  'nlon': 73,
                                  'refinement_factor': 5,
                                  'ncells': 10
                                  },
             'location_parameters': {'buff1': 7,
                                     'buff2': 7,
                                     'dstep1': 5,
                                     'dstep2': 5,
                                     'nlat': 73,
                                     'nr': 25,
                                     'nlon': 76
                                     }
    }
    '''
    import ConfigParser
    config = ConfigParser.RawConfigParser()
    config.read(config_file)
    mydict = {}
    for section in config.sections():
        section_dict = {}
        for option in config.options(section):
            section_dict[option] = config.get(section, option)
        mydict[section] = section_dict
    return eval_dict(mydict)

def eval_dict(my_dict):
    '''
    Recursively typecast dictionary values of str-type to int-type
    or float-type values if appropriate.

    The method antelope.stock.ParameterFile.pf2dict returns a
    dictionary with all str-type values. This method is intended
    primarily to take such a dictionary and typecast integers to
    int-type values and floating points to float-type values.

    Arguments:
    my_dict - Dictionary to be typecast.

    Returns:
    my_dict - Typecasted dictionary.

    Example:
    In [1]: from antpy import eval_dict

    In [2]: my_dict = {'key1': '3',
       ...:           'key2': '4.5',
       ...:           'key3': {'key3A': 'A string.',
       ...:                    'key3B': 'Another string.',
       ...:                    'key3C': {'key3CA': '25',
       ...:                              'key3CB': '67.3'
       ...:                             }
       ...:                  }
       ...:          }

    In [3]: eval_dict(my_dict)
    Out[3]:
    {'key1': 3,
     'key2': 4.5,
     'key3': {'key3A': 'A string.',
              'key3B': 'Another string.',
              'key3C': {'key3CA': 25,
                        'key3CB': 67.3
                       }
              }
    }
    '''
    for key in my_dict:
        if isinstance(my_dict[key], dict):
            eval_dict(my_dict[key])
        else:
            if key in locals():
                continue
            try:
                my_dict[key] = eval(my_dict[key])
            except (NameError, SyntaxError):
                pass
    return my_dict

def verify_config_file(cfg_dict):
    tt_dir = cfg_dict['misc']['tt_map_dir']
    if tt_dir[-1] != '/':
        cfg_dict['misc']['tt_map_dir'] = '%s/' % tt_dir
    return cfg_dict

def find_containing_cube(px, py, pz, x_vec, y_vec, z_vec):
    '''
    NEEDS TO BE UPDATED
    Find the 8 endpoints for the cell which contains point px,py.
    We take advantage of the regular grid.
    Assumes the point is inside the volume defined by x_vec, y_vec,
    z_vec.
    Returns an array of size 8,3 where the rows contain x, y, z
    coordinates of the cubes endpoints
    Also returns indices of endpoints
    '''
    #Find the nearest node point and indices
    x_ind, x_node = find_nearest_index(px, x_vec)
    y_ind, y_node = find_nearest_index(py, y_vec)
    z_ind, z_node = find_nearest_index(pz, z_vec)
    #Now check if the 3 coordinates of p are greater or less than the
    #node it is nearest.
    if px >= x_node:
    #px is east of the nearest node
        #if px is on the x boundary, return a duplicate point
        xi, xn = (x_ind, x_vec[x_ind]) if px == max(x_vec)\
                else (x_ind + 1, x_vec[x_ind + 1])
    else:
    #px is west of the nearest node
        #if px is on the x boundary, return a duplicate point
        xi, xn = (x_ind, x_vec[x_ind]) if px == min(x_vec)\
                else (x_ind - 1, x_vec[x_ind - 1])
    if py >= y_node:
    #py is north of the nearest node
        #if py is on the y boundary, return a duplicate point
        yi, yn = (y_ind, y_vec[y_ind]) if py == max(y_vec)\
                else (y_ind + 1, y_vec[y_ind + 1])
    else:
    #px is south of the nearest node
        yi, yn = (y_ind, y_vec[y_ind]) if py == min(y_vec)\
                else (y_ind - 1, y_vec[y_ind - 1])
    if pz <= z_node:
    #pz is above the nearest node
        zi, zn = (z_ind, z_vec[z_ind]) if pz == max(z_vec)\
                else (z_ind + 1, z_vec[z_ind + 1])
    else:
    #pz is below the nearest node
        zi, zn = (z_ind, z_vec[z_ind]) if pz == min(z_vec)\
                else (z_ind - 1, z_vec[z_ind - 1])
    #Add new endpoints to define the cube
    endpoints = []
    endpoints.append([x_node, y_node, z_node])
    endpoints.append([xn, y_node, z_node])
    endpoints.append([xn, yn, z_node])
    endpoints.append([x_node, yn, z_node])
    endpoints.append([x_node, y_node, zn])
    endpoints.append([xn, y_node, zn])
    endpoints.append([xn, yn, zn])
    endpoints.append([x_node, yn, zn])
    #Add indices
    indices = []
    indices.append([x_ind, y_ind, z_ind])
    indices.append([xi, y_ind, z_ind])
    indices.append([xi, yi, z_ind])
    indices.append([x_ind, yi, z_ind])
    indices.append([x_ind, y_ind, zi])
    indices.append([xi, y_ind, zi])
    indices.append([xi, yi, zi])
    indices.append([x_ind, yi, zi])
    return endpoints, indices

def find_nearest(nparray, value):
    '''
    NEEDS TO BE UPDATED
    Returns the nearest item in nparray to value
    '''
    idx = (abs(nparray - value)).argmin()
    return nparray.flat[idx]

def find_nearest_index(px, x_vec):
    '''
    NEEDS TO BE UPDATED
    Find the nearest x in x_vec
    returns index
    '''
    best_ind = 0
    shortest = float('inf')
    for ii in range(len(x_vec)):
        if abs(x_vec[ii] - px) < shortest:
            shortest = abs(x_vec[ii] - px)
            best_ind = ii
    return best_ind, x_vec[best_ind]

def read_predicted_travel_times(arrivals, tt_dir, nx, ny, nz):
    n = nx * ny * nz * 8
    predicted_travel_times = {}
    for arrival in arrivals:
        if arrival.sta not in predicted_travel_times:
            predicted_travel_times[arrival.sta] = {}
        data = open(os.path.join(tt_dir, 'bin.%s.%s.traveltime' % \
                (arrival.sta, arrival.phase)), 'r').read()
        predicted_travel_times[arrival.sta][arrival.phase] = \
                [struct.unpack('d', data[i: i + 8])[0] for i in range(0, n, 8)]
    return predicted_travel_times


class Locator:
    '''
    An object class to provide functionality to locate earthquake.
    Location parameter configuration is stored in this object class.
    '''
    def __init__(self, cfg_dict):
        '''
        Initialize locator object with a dictionary of paramaters
        parsed from .cfg file by loctools3D.core_tools.parse_cfg().

        Arguments:
        cfg_dict - Dictionary returned by loctools3D.core_tools.parse_cfg()
        '''
        for key in cfg_dict:
            setattr(self, key, cfg_dict[key])
        if self.dbgrassoc_grid_search:
            pfile = open('tmp_dbgrassoc.pf', 'w')
            pfile.write('process_time_window\t 500.0\n')
            pfile.write('process_ncycle\t\t 20\n')
            pfile.write('process_tcycle\t\t 0.0\n')
            pfile.write('use_associated_stations_for_best_grid\t no\n\n')

            pfile.write('grid_params &Arr{\n')
            pfile.write('\t fm3d &Arr{\n')
            pfile.write('\t\t nsta_thresh\t 10\n')
            pfile.write('\t\t nxd\t 11\n')
            pfile.write('\t\t nyd\t 11\n')
            pfile.write('\t\t cluster_twin\t 1.5\n')
            pfile.write('\t\t try_S\t yes\n')
            pfile.write('\t\t associate_S\t no\n')
            pfile.write('\t\t reprocess_S\t no\n')
            pfile.write('\t\t drop_if_on_edge\t yes\n')
            pfile.write('\t\t auth\t dbgrassoc\n')
            pfile.write('\t\t algorithm\t dbgrassoc\n')
            pfile.write('\t\t P_deltim\t 0.1\n')
            pfile.write('\t\t S_deltim\t 0.2\n')
            pfile.write('\t\t phase_sifter\t a\n')
            pfile.write('\t\t P_channel_sifter\t .*\n')
            pfile.write('\t\t S_channel_sifter\t .*\n')
            pfile.write('\t\t P_det_tmin\t 10.0\n')
            pfile.write('\t\t priority\t 1\n')
            pfile.write('\t\t use_dwt\t no\n')
            pfile.write('\t\t use_dwts\t no\n')
            pfile.write('\t}\n')
            pfile.write('}\n')


    def locate_eq(self, event):
        '''
        NEEDS TO BE UPDATED
        Locate an earthquake based on the arrivals in event, traveltime
        files which are already saved.
        '''
        prop_params = self.propagation_grid
        earth_rad = self.earth_radius
        tt_map_dir = self.tt_map_dir
#Get Propagation grid parameters
        minlat = prop_params['minlat']
        nlat = prop_params['nlat']
        dlat = prop_params['dlat']
        maxlat = minlat + (nlat - 1) * dlat
        minlon = prop_params['minlon']
        nlon = prop_params['nlon']
        dlon = prop_params['dlon']
        maxlon = minlon + (nlon - 1) * dlon
        minr = prop_params['minr']
        nr = prop_params['nr']
        dr = prop_params['dr']
        maxr = minr + (nr - 1) * dr
        minz = earth_rad - maxr
        nz = nr
        dz = dr
        maxz = earth_rad - minr
        li  =  LinearIndex(nlon, nlat, nz)
#Build geographic coordinate axes
        qlat = linspace(minlat, maxlat + dlat, nlat, False)
        qlon = linspace(minlon, maxlon + dlon, nlon, False)
#The next line is causes the coordinate system to be left-handed
        qdep = linspace(maxz, minz - dz, nz, False)
        start_time = time.time()
        arrivals = []
#Compile all the arrival time data available
        for arrival in event.arrivals:
#Make sure the needed travel-time file exist
            if arrival.phase == 'S' and not self.use_S:
                continue
            else:
                if not os.path.isfile('%s/bin.%s.%s.traveltime'
                        % (self.tt_map_dir, arrival.sta, arrival.phase)):
                    logger.info("No %s-wave travel time file for station %s, "\
                            "omitting from inversion." \
                            % (arrival.phase, arrival.sta))
                    continue
                arrivals += [arrival]
#Make sure there are at least pfile['nobs'] arrivals to use in relocation
        if len(arrivals) < self.nobs:
            logger.info("[evid: %d] Only %d valid arrivals found. Skipping "\
                    "relocation." % (event.evid, len(arrivals)))
            return None, None
        stations = [arrival.sta for arrival in arrivals]
        #predicted_travel_times = read_predicted_travel_times(stations,
        #                                                     tt_map_dir,
        #                                                     nlon,
        #                                                     nlat,
        #                                                     nz)
        predicted_travel_times = read_predicted_travel_times(arrivals,
                                                             tt_map_dir,
                                                             nlon,
                                                             nlat,
                                                             nz)
        #for sta in predicted_travel_times:
        #    for phase in predicted_travel_times[sta]:
        #        print sta, phase, len(predicted_travel_times[sta][phase])
#Perform a grid search
        logger.debug("[evid: %d] Starting grid search." % event.evid)
        qx = range(0, nlon - 1)
        qy = range(0, nlat - 1)
        qz = range(0, nz - 1)
        minx, miny, minz, otime, ha = grid_search_abs(qx,
                                                      qy,
                                                      qz,
                                                      arrivals,
                                                      predicted_travel_times,
                                                      li)
        logger.debug("[evid: %d] Grid search complete." % event.evid)
#        print minx, miny, minz, otime
        #logger.debug("[evid: %d] Starting dbgrassoc grid search." % event.evid)
        #minx, miny, minz, otime = grid_search_dbgrassoc(qx,
        #                                                qy,
        #                                                qz,
        #                                                arrivals,
        #                                                tt_map_dir)
        #logger.debug("[evid: %d] dbgrassoc grid search complete." % event.evid)
        #print minx, miny, minz, otime
        #print '!!!'
        #print event.time - otime
        #print '!!!'
#Best-fit grid point
        glon = qlon[minx]
        glat = qlat[miny]
        gz = qdep[minz]
#        new_origin =  Origin(glat,
#                             glon,
#                             gz,
#                             otime,
#                             '3Dreloc',
#                             arrivals=event.arrivals,
#                             evid=event.evid,
#                             nass=len(event.arrivals),
#                             ndef=len(arrivals))
#        return new_origin
        logger.debug("[evid: %d] Starting sub-grid location inversion." %\
                event.evid)
#Get subgrid location
        dz = -dr
        arrival_times = [arrival.time for arrival in arrivals]
        #for i in range(10):#This is really a while loop, but like this in case it is degenerate
        #    c, resid, tt_updated, sigma, resid_std =\
        #            self.get_subgrid_loc(minx,
        #                                 miny,
        #                                 minz,
        #                                 arrivals,
        #                                 predicted_travel_times,
        #                                 li)
        #    loc_change = c * [dlon, dlat, dz]
#Find the best-fit source location in geographic coordinates
        #    newloc = [newlon, newlat, newz] =\
        #            asarray([glon, glat, gz]) + loc_change
        #    ix = nonzero(qlon == find_nearest(qlon, newlon))[0][0]
        #    iy = nonzero(qlat == find_nearest(qlat, newlat))[0][0]
        #    iz = nonzero(qdep == find_nearest(qdep, newz))[0][0]
        #    if minx == ix and miny == iy and minz == iz:
        #        break
        #    minx, miny, minz = ix, iy, iz
###############
        #This is for testing new inversion algorithm
        result = self.get_subgrid_loc_new(minx,
                                          miny,
                                          minz,
                                          arrivals,
                                          predicted_travel_times,
                                          li,
                                          qlon,
                                          qlat,
                                          qdep)
        if result:
            u, tt_updated, sdobs = result
        else:
            return None, None
        origerr = Origerr(sdobs)
        newloc = [newlon, newlat, newz] = \
                [minlon + u[0] * dlon,
                        minlat + u[1] * dlat,
                        earth_rad - minr + u[2] * dz]
        if newloc[2] < 0:
            newloc[2] = 0.0
            newz =  0.0
        otime = u[3]
        #print event.lon, event.lat, event.depth, event.time
        #print newlon, newlat, newz, otime
        #print sdobs
################
#Make sure origin is within boundary of velocity model
        if newloc[0] < min(qlon) or newloc[0] > max(qlon) or\
                newloc[1] < min(qlat) or newloc[1] > max(qlat) or\
                newloc[2] < min(qdep) or newloc[2] > max(qdep):
            return None, None
        logger.debug("[evid: %d] Sub-grid location inversion complete." %\
                event.evid)
#Update calculated travel times in Event object
        for event_arrival in event.arrivals:
            for arrival in arrivals:
                if event_arrival.phase == arrival.phase and\
                        event_arrival.sta == arrival.sta:
                    event_arrival.tt_calc = tt_updated[arrival.sta]
        elapsed_time = time.time() - start_time
        logger.info("[evid: %d] Relocation took %.3f seconds" %\
                (event.evid, elapsed_time))
        new_origin =  Origin(newlat,
                             newlon,
                             newz,
                             otime,
                             '3Dreloc',
                             arrivals=event.arrivals,
                             evid=event.evid,
                             nass=len(event.arrivals),
                             ndef=len(arrival_times))
        #print event.lat - newlat, event.lon - newlon, event.depth - newz, sqrt((event.lat - newlat)**2 + (event.lon - newlon)**2)
        cfg_dict = {'earth_radius': earth_rad,\
                    'tt_map_dir': tt_map_dir,\
                    'propagation_grid': self.propagation_grid}
        logger.debug('[evid: %d] Updating predicted arrival times.' %\
                event.evid)
        new_origin.update_predarr_times(cfg_dict, predicted_travel_times)
        logger.debug('[evid: %d] Predicted arrival times updated.' %\
                event.evid)
        return new_origin, origerr

    def get_subgrid_loc_new(self, ix, iy, iz, arrivals, pred_tts, li, qlon, qlat, qdep):
        '''
        NEEDS TO BE UPDATED
        '''
#Test least squares on real data
        #stas = [arrival.sta for arrival in arrivals]
        #P_arrivals = [arrival for arrival in arrivals if arrival.phase == 'P']
        #S_arrivals = [arrival for arrival in arrivals if arrival.phase == 'S']
        arrival_times = [arrival.time for arrival in arrivals]
        #P_arrival_times = [arrival.time for arrival in arrivals if \
        #        arrival.phase == 'P']
        #S_arrival_times = [arrival.time for arrival in arrivals if \
        #        arrival.phase == 'S']
#Calculate forward deriatives making sure that each calculation
#involves two unique points
        ind = li.convert_to_1D(ix, iy, iz)
        t0 = array([arrival.time - pred_tts[arrival.sta][arrival.phase][ind] \
                for arrival in arrivals]).mean()
        u = [ix, iy, iz, t0]
        #print u[0], u[1], u[2], u[3]
        #Ptt000 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #        P_arrivals])
        #Stt000 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #        S_arrivals])
        tt000 = array([pred_tts[arrival.sta][arrival.phase][ind] for arrival in\
                arrivals])
        if ix == li.nx - 1:
        #    Pdt_dx = None
        #    Sdt_dx = None
            dt_dx = None
        else:
            ind = li.convert_to_1D(ix + 1, iy, iz)
        #    Ptt100 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Stt100 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pdt_dx = Ptt100 - Ptt000
        #    Sdt_dx = Stt100 - Stt000
            tt100 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            dt_dx = tt100 - tt000
        if iy == li.ny - 1:
        #    Pdt_dy = None
        #    Sdt_dy = None
            dt_dy = None
        else:
            ind = li.convert_to_1D(ix, iy + 1, iz)
        #    Ptt010 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Stt010 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pdt_dy = Ptt010 - Ptt000
        #    Sdt_dy = Stt010 - Stt000
            tt010 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            dt_dy = tt010 - tt000
        if iz == li.nz - 1:
        #    Pdt_dz = None
        #    Sdt_dz = None
            dt_dz = None
        else:
            ind = li.convert_to_1D(ix, iy, iz + 1)
        #    Ptt001 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Stt001 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pdt_dz = Ptt001 - Ptt000
        #    Sdt_dz = Stt001 - Stt000
            tt001 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            dt_dz = tt001 - tt000
#Calculate backward derivatives making sure that each calculation
#involves two unique points
        if ix == 0:
        #    Pbdt_dx = None
        #    Sbdt_dx = None
            bdt_dx = None
        else:
            ind = li.convert_to_1D(ix - 1, iy, iz)
        #    Pbtt100 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Sbtt100 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pbdt_dx = Ptt000 - Pbtt100
        #    Sbdt_dx = Stt000 - Sbtt100
            btt100 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            bdt_dx = tt000 - btt100
        if iy == 0:
        #    Pbdt_dy = None
        #    Sbdt_dy = None
            bdt_dy = None
        else:
            ind = li.convert_to_1D(ix, iy - 1, iz)
        #    Pbtt010 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Sbtt010 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pbdt_dy = Ptt000 - Pbtt010
        #    Sbdt_dy = Stt000 - Sbtt010
            btt010 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            bdt_dy = tt000 - btt010
        if iz == 0:
        #    Pbdt_dz = None
        #    Sbdt_dz = None
            bdt_dz = None
        else:
            ind = li.convert_to_1D(ix, iy, iz - 1)
        #    Pbtt001 = array([pred_tts[arrival.sta]['P'][ind] for arrival in \
        #            P_arrivals])
        #    Sbtt001 = array([pred_tts[arrival.sta]['S'][ind] for arrival in \
        #            S_arrivals])
        #    Pbdt_dz = Ptt000 - Pbtt001
        #    Sbdt_dz = Stt000 - Sbtt001
            btt001 = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                    arrival in arrivals])
            bdt_dz = tt000 - btt001
#Calculate central derivative (average) ensuring each independant
#derivative was calculated using two unique points.
        #Pdt_dx = [deriv for deriv in (Pdt_dx, Pbdt_dx) if deriv != None]
        #Pdt_dx = sum(Pdt_dx) / len(Pdt_dx)
        #Sdt_dx = [deriv for deriv in (Sdt_dx, Sbdt_dx) if deriv != None]
        #Sdt_dx = sum(Sdt_dx) / len(Sdt_dx)
        #dt_dx = append(Pdt_dx, Sdt_dx)
        dt_dx = [deriv for deriv in (dt_dx, bdt_dx) if deriv != None]
        dt_dx = sum(dt_dx) / len(dt_dx)
        #Pdt_dy = [deriv for deriv in (Pdt_dy, Pbdt_dy) if deriv != None]
        #Pdt_dy = sum(Pdt_dy) / len(Pdt_dy)
        #Sdt_dy = [deriv for deriv in (Sdt_dy, Sbdt_dy) if deriv != None]
        #Sdt_dy = sum(Sdt_dy) / len(Sdt_dy)
        #dt_dy = append(Pdt_dy, Sdt_dy)
        dt_dy = [deriv for deriv in (dt_dy, bdt_dy) if deriv != None]
        dt_dy = sum(dt_dy) / len(dt_dy)
        #Pdt_dz = [deriv for deriv in (Pdt_dz, Pbdt_dz) if deriv != None]
        #Pdt_dz = sum(Pdt_dz) / len(Pdt_dz)
        #Sdt_dz = [deriv for deriv in (Sdt_dz, Sbdt_dz) if deriv != None]
        #Sdt_dz = sum(Sdt_dz) / len(Sdt_dz)
        #dt_dz = append(Pdt_dz, Sdt_dz)
        dt_dz = [deriv for deriv in (dt_dz, bdt_dz) if deriv != None]
        dt_dz = sum(dt_dz) / len(dt_dz)
#Build and condition residual vector
#######################################################
#CHECK THIS!!!!!!!!!!!
        #residuals = arrival_times - tt000
        #residuals = residuals - residuals.mean()
#The above two lines are Amir's, the below line is Malcolm's
        #P_residuals = P_arrival_times - (t0 + Ptt000)
        #S_residuals = S_arrival_times - (t0 + Stt000)
        #residuals = append(P_residuals, S_residuals)
        residuals = arrival_times - (t0 + tt000)
#######################################################
#Create a matrix of the spatial derivatives of travel-times
        A = c_[dt_dx, dt_dy, dt_dz, ones(len(dt_dx))]
#Find the change in position which best fits the residuals in a
#least-squares sense
#Let delta_r represent the change in position
        delta_u, residues, rank, sigma = linalg.lstsq(A, residuals)

#Compute updated travel times
        u_prime = array([ix, iy, iz, t0]) + delta_u
        #print u_prime[0], u_prime[1], u_prime[2], u_prime[3]

        tt_updated_temp = tt000 + (A * delta_u).sum(axis=1)
        tt_updated = {}
        i = 0
        for arrival in arrivals:
            if arrival.sta not in tt_updated:
                tt_updated[arrival.sta] = {}
            tt_updated[arrival.sta][arrival.phase] = tt_updated_temp[i]
            i += 1

#calculate new set of residuals
        #residuals = arrival_times - (u_prime[3] + [tt_updated[sta] for sta in stas])
        residuals = arrival_times - (u_prime[3] + \
                [tt_updated[arrival.sta][arrival.phase] \
                for arrival in arrivals])
        j = 0
        while True:
#calculate the derivative w.r.t. x-axis in the containing cube
            if delta_u[0] > 0:
                if ix == li.nx - 1:
                    #this should probably be zeros.
                    dt_dx = zeros(len(arrivals))
                ind = li.convert_to_1D(ix + 1, iy, iz)
                #dt_dx = array([pred_tts[sta][ind] for sta in stas]) - tt000
                dt_dx = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals]) - tt000
            elif delta_u[0] < 0:
                if ix == 0:
                    #this should probably be zeros.
                    dt_dx = zeros(len(arrivals))
                ind = li.convert_to_1D(ix - 1, iy, iz)
                #dt_dx = tt000 - array([pred_tts[sta][ind] for sta in stas])
                dt_dx = tt000 - \
                        array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals])
            else:
                #this should probably be zeros.
                dt_dx = zeros(len(arrivals))
#calculate the derivative w.r.t. y-axis in the containing cube
            if delta_u[1] > 0:
                if iy == li.ny - 1:
                    #this should probably be zeros.
                    dt_dy = zeros(len(arrivals))
                ind = li.convert_to_1D(ix, iy + 1, iz)
                #dt_dy = array([pred_tts[sta][ind] for sta in stas]) - tt000
                dt_dy = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals]) - tt000
            elif delta_u[1] < 0:
                if iy == 0:
                    #this should probably be zeros.
                    dt_dy = zeros(len(arrivals))
                ind = li.convert_to_1D(ix, iy - 1, iz)
                #dt_dy = tt000 - array([pred_tts[sta][ind] for sta in stas])
                dt_dy = tt000 - \
                        array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals])
            else:
                #this should probably be zeros.
                dt_dy = zeros(len(arrivals))
#calculate the derivative w.r.t. z-axis in the containing cube
            if delta_u[2] > 0:
                if iz == li.nz - 1:
                    #this should probably be zeros.
                    dt_dz = zeros(len(arrivals))
                ind = li.convert_to_1D(ix, iy, iz + 1)
                #dt_dz = array([pred_tts[sta][ind] for sta in stas]) - tt000
                dt_dz = array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals]) - tt000
            elif delta_u[2] < 0:
                if iz == 0:
                    #this should probably be zeros.
                    dt_dz = zeros(len(arrivals))
                ind = li.convert_to_1D(ix, iy, iz - 1)
                #dt_dz = tt000 - array([pred_tts[sta][ind] for sta in stas])
                dt_dz = tt000 - \
                        array([pred_tts[arrival.sta][arrival.phase][ind] for \
                        arrival in arrivals])
            else:
                #this should probably be zeros.
                dt_dz = zeros(len(arrivals))
            A = c_[dt_dx, dt_dy, dt_dz, ones(len(dt_dx))]
            delta_u, residues, rank, sigma = linalg.lstsq(A, residuals)
#Compute updated travel times
            u_prime = u_prime + delta_u
            #print u_prime[0], u_prime[1], u_prime[2], u_prime[3]
            tt_updated_temp = array([tt_updated[arrival.sta][arrival.phase] \
                    for arrival in arrivals]) + (A * delta_u).sum(axis=1)
            tt_updated = {}
            i = 0
            for arrival in arrivals:
                if arrival.sta not in tt_updated:
                    tt_updated[arrival.sta] = {}
                tt_updated[arrival.sta][arrival.phase] = tt_updated_temp[i]
                i += 1
#calculate new set of residuals
            residuals_prime = arrival_times - (u_prime[3] + \
                    [tt_updated[arrival.sta][arrival.phase] for \
                    arrival in arrivals])
            if j > self.max_iterations:
                logger.debug("Maximum number of iterations exceeded.")
                return None
            j += 1
            delta_residuals = residuals.std() - residuals_prime.std()
            #delta_r = sqrt(sum([x ** 2 for x in delta_u[:2]]))
            if delta_residuals < self.sdobs_stable_thresh:
            #if delta_r < 0.02:
                logger.debug("sdobs stablilized below threshold - %f" % self.sdobs_stable_thresh)
                break
            residuals = residuals_prime
        return u_prime, tt_updated, residuals.std()

    def get_subgrid_loc(self, ix, iy, iz, arrivals, pred_tts, li):
        '''
        NEEDS TO BE UPDATED
        '''
#Test least squares on real data
        stas = [arrival.sta for arrival in arrivals]
        arrival_times = [arrival.time for arrival in arrivals]
#Calculate forward deriatives making sure that each calculation
#involves two unique points
        ind = li.convert_to_1D(ix, iy, iz)
        t0 = array([arrival.time - pred_tts[arrival.sta][ind] for arrival in arrivals]).mean()
        tt000 = array([pred_tts[sta][ind] for sta in stas])
        if ix == li.nx - 1:
            dt_dx = None
        else:
            ind = li.convert_to_1D(ix + 1, iy, iz)
            tt100 = array([pred_tts[sta][ind] for sta in stas])
            dt_dx = tt100 - tt000
        if iy == li.ny - 1:
            dt_dy = None
        else:
            ind = li.convert_to_1D(ix, iy + 1, iz)
            tt010 = array([pred_tts[sta][ind] for sta in stas])
            dt_dy = tt010 - tt000
        if iz == li.nz - 1:
            dt_dz = None
        else:
            ind = li.convert_to_1D(ix, iy, iz + 1)
            tt001 = array([pred_tts[sta][ind] for sta in stas])
            dt_dz = tt001 - tt000
#Calculate backward derivatives making sure that each calculation
#involves two unique points
        if ix == 0:
            bdt_dx = None
        else:
            ind = li.convert_to_1D(ix - 1, iy, iz)
            btt100 = array([pred_tts[sta][ind] for sta in stas])
            bdt_dx = tt000 - btt100
        if iy == 0:
            bdt_dy = None
        else:
            ind = li.convert_to_1D(ix, iy - 1, iz)
            btt010 = array([pred_tts[sta][ind] for sta in stas])
            bdt_dy = tt000 - btt010
        if iz == 0:
            bdt_dz = None
        else:
            ind = li.convert_to_1D(ix, iy, iz - 1)
            btt001 = array([pred_tts[sta][ind] for sta in stas])
            bdt_dz = tt000 - btt001
#Calculate central derivative (average) ensuring each independant
#derivative was calculated using two unique points.
        dt_dx = [deriv for deriv in (dt_dx, bdt_dx) if deriv != None]
        dt_dx = sum(dt_dx) / len(dt_dx)
        dt_dy = [deriv for deriv in (dt_dy, bdt_dy) if deriv != None]
        dt_dy = sum(dt_dy) / len(dt_dy)
        dt_dz = [deriv for deriv in (dt_dz, bdt_dz) if deriv != None]
        dt_dz = sum(dt_dz) / len(dt_dz)
#Build and condition residual vector
#######################################################
#CHECK THIS!!!!!!!!!!!
        #residuals = arrival_times - tt000
        #residuals = residuals - residuals.mean()
#The above two lines are Amir's, the below line is Malcolm's
        residuals = arrival_times - (t0 + tt000)
#######################################################
#Create a matrix of the spatial derivatives of travel-times
        A = c_[dt_dx, dt_dy, dt_dz]
#Find the change in position which best fits the residuals in a
#least-squares sense
#Let delta_r represent the change in position
        delta_r, residues, rank, sigma = linalg.lstsq(A, residuals)
#Compute updated travel times
        tt_updated_temp = tt000 + (A * delta_r).sum(axis=1)
        tt_updated = {}
        i = 0
        for sta in stas:
            tt_updated[sta] = tt_updated_temp[i]
            i += 1
#Compute variance-covariance matrix
        A = c_[dt_dx, dt_dy, dt_dz, dt_dx * 0 + 1] #Add origin time 'derivative'

        sigma = dot(A.transpose(), A) #There is probably more to it than this...
        return delta_r, residues, tt_updated, sigma, residuals.std()

    def fix_boundary_search(self, qx, nx):
        '''
        NEEDS TO BE UPDATED
        When performing a grid search on a subgrid, make sure you don't go off the edges
          qx         search vectors, these will be modified then returned
          nx         max index [li.nx]
        '''
        for ix in range(len(qx)):
            if qx[ix] < 0:
                qx[ix] = 0
            if qx[ix] >= nx:
                qx[ix] = nx - 1
        newqx = uniq(qx)
        return newqx

def uniq(input):
    '''
    NEEDS TO BE UPDATED
    Remove duplicate items from a list. Preserves order.
    '''
    output = []
    for x in input:
        if x not in output:
            output.append(x)
    return output

class Station:
    '''
    A container class for station location data.
    '''
    def __init__(self, sta, lat, lon, elev):
        '''
        Initialize Station object.

        Arguments:
        sta - Station code.
        lat - Station latitude.
        lon - Station longitude.
        elev - Statio elevation.
        '''
        self.sta = sta
        self.lat = lat
        self.lon = lon
        self.elev = elev

    def __str__(self):
        '''
        Return string representation of self object.
        '''
        ret = 'Station Object\n--------------\n'
        ret += 'sta:\t\t%s\n' % self.sta
        ret += 'lat:\t\t%s\n' % self.lat
        ret += 'lon:\t\t%s\n' % self.lon
        ret += 'elev:\t\t%s\n' % self.elev
        return ret

class Event():
    '''
    A container class for earthquake event data. Mirrors the Event
    table of the CSS3.0 databse schema.
    '''
    #def __init__(self, time, lat, lon, depth, mag, magtype=None, evid=None):
    def __init__(self,
                 prefor=None,
                 evid=None,
                 evname=None,
                 auth=None,
                 commid=None,
                 lddate=None,
                 origins=None):
        '''
        Initialize Event object.

        Arguments:
        prefor - Preferred origin ID.

        Keyword Arguments:
        evid - Event ID.
        evname - Event author.
        commid - Comment ID.
        lddate - Load date.
        origins - List of loctools3D.core_tools.Origin objects.

        Example:
        In [1]: from loctools3D.core_tools import Event, Origin

        In [2]: origin = Origin(33.4157,
                                -116.8622,
                                4.8910,
                                1275439331.718,
                                orid=287456,
                                nass=47,
                                ndef=47,
                                auth='ANF:vernon',
                                evid=202856,
                                algorithm='locsat:iasp91')

        In [3]: event = Event(prefor=287456,
                              evid=202856,
                              auth='ANF:vernon',
                              origins=[origin])

        In [4]: print event
        Event Object
        ------------
        evid:       202856
        evname:     None
        prefor:     287456
        auth:       ANF:vernon
        commid:     None
        lddate:     None
        origins:
                    Origin Object
                    -------------
                    lat:        33.4157
                    lon:        -116.8622
                    depth:      4.891
                    time:       1275439331.72
                    orid:       287456
                    evid:       202856
                    auth:       ANF:vernon
                    jdate:      None
                    nass:       47
                    ndef:       47
                    ndp:        None
                    grn:        None
                    srn:        None
                    etype:      None
                    review:     None
                    depdp:      None
                    dtype:      None
                    mb:     None
                    mbid:       None
                    ms:     None
                    msid:       None
                    None
                    mlid:       None
                    algorithm:      locsat:iasp91
                    commid:     None
                    lddate:     None
                    arrivals:
        '''
        import time as pytime
        self.evid = evid
        self.evname = evname
        self.auth = auth
        self.commid = commid
        self.lddate = lddate
        self.preferred_origin = None
        if origins == None: self.origins = []
        else: self.origins = origins
        self.set_prefor(prefor)

    def __str__(self):
        '''
        Return the string representation of loctools3D.core_tools.Event
        object.
        '''
        ret = 'Event Object\n------------\n'
        ret += 'evid:\t\t%s\n' % self.evid
        ret += 'evname:\t\t%s\n' % self.evname
        ret += 'prefor:\t\t%s\n' % self.prefor
        ret += 'auth:\t\t%s\n' % self.auth
        ret += 'commid:\t\t%s\n' % self.commid
        ret += 'lddate:\t\t%s\n' % self.lddate
        ret += 'origins:\n'
        if len(self.origins) == 0:
            ret += '\t\tNone\n'
        else:
            for i in range(len(self.origins)):
                for line in  ('%s' % self.origins[i]).split('\n'):
                    ret += '\t\t%s\n' % line
        return ret

    def set_prefor(self, prefor):
        '''
        Set self.prefor equal to new origin ID and set
        self.preferred_origin to point to the loctools3D.core_tools.Origin
        object referred to by that origin ID.

        Arguments:
        prefor - The origin ID (orid) of the preferred solution.

        Example:
        In [1]: from loctools3D.core_tools import Event, Origin

        In [2]: origin1 = Origin(43.7000,
                                 -79.4000,
                                 5.0,
                                 1398883850.648,
                                 'White',
                                 orid=1234,
                                 evid=1001)

        In [3]: origin2 = Origin(43.7050,
                                 -79.3981,
                                 7.3,
                                 1398883851.346,
                                 'White',
                                 orid=1235,
                                 evid=1001)

        In [4]: event = Event(prefor=1234,
                              evid=1001,
                              auth='White',
                              origins=[origin1, origin2])

        In [5]: print event.preferred_origin
        Origin Object
        -------------
        lat:        43.7
        lon:        -79.4
        depth:      5.0
        time:       1398883850.65
        orid:       1234
        evid:       1001
        auth:       White
        jdate:      None
        nass:       None
        ndef:       None
        ndp:        None
        grn:        None
        srn:        None
        etype:      None
        review:     None
        depdp:      None
        dtype:      None
        mb:     None
        mbid:       None
        ms:     None
        msid:       None
        ml:     None
        mlid:       None
        algorithm:      None
        commid:     None
        lddate:     None
        arrivals:


        In [6]: event.set_prefor(1235)
        Out[6]: 0

        In [7]: print event.preferred_origin
        Origin Object
        -------------
        lat:        43.705
        lon:        -79.3981
        depth:      7.3
        time:       1398883851.35
        orid:       1235
        evid:       1001
        auth:       White
        jdate:      None
        nass:       None
        ndef:       None
        ndp:        None
        grn:        None
        srn:        None
        etype:      None
        review:     None
        depdp:      None
        dtype:      None
        mb:     None
        mbid:       None
        ms:     None
        msid:       None
        ml:     None
        mlid:       None
        algorithm:      None
        commid:     None
        lddate:     None
        arrivals:
        '''
        self.prefor = prefor
        for i in range(len(self.origins)):
            if self.origins[i].orid == prefor:
                self.preferred_origin = self.origins[i]
                return 0
        if len(self.origins) == 0:
            return -1
        else:
            self.preferred_origin = self.origins[0]
            return 1

    def add_origin(self,
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
                   lddate=None):
        '''
        Add an loctools3D.core_tools.Origin object to the list of origins
        associated with this event.

        Arguments:
        lat - Latitude of earthquake hypocenter.
        lon - Longitude of earthquake hypocenter.
        depth - Depth of earthquake hypocenter.
        time - Epoch time of earthquake rupture.

        Keyword Arguments:
        These need to be FULLY described here. Procastinating on this,
        see below.  These fields are optional and exist for posterity
        and to mirror the Origin table of the CSS3.0 schema in whole.
        Refer to CSS3.0 schema for details
        (https://anf.ucsd.edu/pdf/css30.pdf).

        Example:
        In [1]: from loctools3D.core_tools import Event

        In [2]: event = Event(prefor=287456, evid=202856, auth='ANF:vernon')

        In [3]: print event
        Event Object
        ------------
        evid:       202856
        evname:     None
        prefor:     287456
        auth:       ANF:vernon
        commid:     None
        lddate:     None
        origins:
                    None


        In [4]: event.add_origin(33.4157,
                                 -116.8622,
                                 4.8910,
                                 1275439331.718,
                                 orid=287456,
                                 nass=47,
                                 ndef=47,
                                 auth='ANF:vernon',
                                 evid=202856,
                                 algorithm='locsat:iasp91')

        In [5]: print event
        Event Object
        ------------
        evid:       202856
        evname:     None
        prefor:     287456
        auth:       ANF:vernon
        commid:     None
        lddate:     None
        origins:
                    Origin Object
                    -------------
                    lat:        33.4157
                    lon:        -116.8622
                    depth:      4.891
                    time:       1275439331.72
                    orid:       287456
                    evid:       202856
                    auth:       ANF:vernon
                    jdate:      None
                    nass:       47
                    ndef:       47
                    ndp:        None
                    grn:        None
                    srn:        None
                    etype:      None
                    review:     None
                    depdp:      None
                    dtype:      None
                    mb:     None
                    mbid:       None
                    ms:     None
                    msid:       None
                    ml:     None
                    mlid:       None
                    algorithm:      locsat:iasp91
                    commid:     None
                    lddate:     None
                    arrivals:
        '''
        self.origins += [Origin(lat,
                                lon,
                                depth,
                                time,
                                auth,
                                orid=orid,
                                evid=evid,
                                arrivals=arrivals,
                                jdate=jdate,
                                nass=nass,
                                ndef=ndef,
                                ndp=ndp,
                                grn=grn,
                                srn=srn,
                                etype=etype,
                                review=review,
                                depdp=depdp,
                                dtype=dtype,
                                mb=mb,
                                mbid=mbid,
                                ms=ms,
                                msid=msid,
                                ml=ml,
                                mlid=mlid,
                                algorithm=algorithm,
                                commid=commid,
                                lddate=lddate)]
class Origerr():
    '''
    A container class for earthquake event data. Mirrors the Origerr
    table of the CSS3.0 databse schema.
    '''
    def __init__(self,
                 sdobs,
                 orid=None,
                 sxx=None,
                 syy=None,
                 szz=None,
                 stt=None,
                 sxy=None,
                 sxz=None,
                 syz=None,
                 stx=None,
                 sty=None,
                 stz=None,
                 smajax=None,
                 sminax=None,
                 strike=None,
                 sdepth=None,
                 stime=None,
                 conf=None,
                 commid=None,
                 lddate=None):
        self.sdobs = sdobs
        self.orid = orid
        self.sxx = sxx
        self.syy = syy
        self.szz = szz
        self.stt = stt
        self.sxy = sxy
        self.sxz = sxz
        self.syz = syz
        self.stx = stx
        self.sty = sty
        self.stz = stz
        self.smajax = smajax
        self.sminax = sminax
        self.strike = strike
        self.sdepth = sdepth
        self.stime = stime
        self.conf = conf
        self.commid = commid
        self.lddate = lddate

    def set_orid(self, orid):
        self.orid = orid

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
                 lddate=None):
        '''
        Initialize Origin object.

        Arguments:
        lat - Latitude of earthquake hypocenter.
        lon - Longitude of earthquake hypocenter.
        depth - Depth of earthquake hypocenter.
        time - Epoch time of earthquake rupture.

        Keyword Arguments:
        These need to be FULLY described here. Procastinating on this,
        see below.  These fields are optional and exist for posterity
        and to mirror the Origin table of the CSS3.0 schema in whole.
        Refer to CSS3.0 schema for details
        (https://anf.ucsd.edu/pdf/css30.pdf).

        Example:
        In [1]: from loctools3D.core_tools import Origin, Arrival

        In [2]: arrivals = [Arrival('SND',
                                  1276817657.230,
                                  'P',
                                  chan='HHZ')]

        In [3]: arrivals += [Arrival('FRD',
                                   1276817656.000,
                                   'P',
                                   chan='HHZ')]

        In [4]: origin = Origin(32.7103,
                                -115.9378,
                                3.44,
                                1276817637.470,
                                'White',
                                orid=235993,
                                evid=2010168,
                                nass=6,
                                ndef=64)

        In [5]: print origin
        Origin Object
        -------------
        lat:        32.7103
        lon:        -115.9378
        depth:      3.44
        time:       1276817637.47
        orid:       235993
        evid:       2010168
        auth:       White
        jdate:      None
        nass:       6
        ndef:       64
        ndp:        None
        grn:        None
        srn:        None
        etype:      None
        review:     None
        depdp:      None
        dtype:      None
        mb:     None
        mbid:       None
        ms:     None
        msid:       None
        ml:     None
        mlid:       None
        algorithm:      None
        commid:     None
        lddate:     None
        arrivals:
        '''

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

    def __str__(self):
        '''
        Returns the string representation of loctools3D.core_tools.Origin
        object.
        '''
        ret = 'Origin Object\n-------------\n'
        ret += 'lat:\t\t%s\n' % self.lat
        ret += 'lon:\t\t%s\n' % self.lon
        ret += 'depth:\t\t%s\n' % self.depth
        ret += 'time:\t\t%s\n' % self.time
        ret += 'orid:\t\t%s\n' % self.orid
        ret += 'evid:\t\t%s\n' % self.evid
        ret += 'auth:\t\t%s\n' % self.auth
        ret += 'jdate:\t\t%s\n' % self.jdate
        ret += 'nass:\t\t%s\n' % self.nass
        ret += 'ndef:\t\t%s\n' % self.ndef
        ret += 'ndp:\t\t%s\n' % self.ndp
        ret += 'grn:\t\t%s\n' % self.grn
        ret += 'srn:\t\t%s\n' % self.srn
        ret += 'etype:\t\t%s\n' % self.etype
        ret += 'review:\t\t%s\n' % self.review
        ret += 'depdp:\t\t%s\n' % self.depdp
        ret += 'dtype:\t\t%s\n' % self.dtype
        ret += 'mb:\t\t%s\n' % self.mb
        ret += 'mbid:\t\t%s\n' % self.mbid
        ret += 'ms:\t\t%s\n' % self.ms
        ret += 'msid:\t\t%s\n' % self.msid
        ret += 'ml:\t\t%s\n' % self.ml
        ret += 'mlid:\t\t%s\n' % self.mlid
        ret += 'algorithm:\t\t%s\n' % self.algorithm
        ret += 'commid:\t\t%s\n' % self.commid
        ret += 'lddate:\t\t%s\n' % self.lddate
        ret += 'arrivals:\n'
        for i in range(len(self.arrivals)):
            ret += '%s' % self.arrivals[i]
        return ret

    def update_predarr_times(self, cfg_dict, pred_tts):
        '''
        Update the loctools3D.core_tools.Arrival.tt_calc and
        loctools3D.core_tools.Arrival.predarr fields for each Arrival object in
        loctools3D.core_tools.Origin object's arrivals attribute.

        Arguments:
        cfg_dict - Configuration dictionary as returned by
        loctools3D.core_tools.parse_cfg()

        Caveats:
        This functionality is currently only implemented for P-wave
        arrivals.

        Example:
        In [1]: import sys

        In [2]: import os

        In [3]: sys.path.append('%s/data/python' % os.environ['ANTELOPE'])

        In [4]: from antelope.datascope import closing, dbopen

        In [5]: import loctools3D.core_tools as core_tools

        In [6]: import loctools3D.ant as ant

        In [7]: cfg_dict = core_tools.parse_cfg('test_pf_2_cfg.cfg')

        In [8]: locator = core_tools.Locator(cfg_dict)

        In [9]: with closing(dbopen('/Users/mcwhite/staging/dbs/'\
                                    'June2010/June2010', 'r')) as db:
           ...:     tbl_event = db.schema_tables['event']
           ...:     tbl_event = tbl_event.subset('evid == 202856')
           ...:     events = ant.create_event_list(tbl_event)
           ...:

       In [10]: new_origin = locator.locate_eq(events[0].preferred_origin)

       In [11]: for arrival in new_origin.arrivals:
          ....:     print arrival.phase, arrival.predarr
          ....:
        S None
        P None
        S None
        P None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        S None
        S None
        P None
        S None
        S None
        S None
        S None
        S None
        S None
        P None
        S None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        P None
        S None
        S None
        S None
        P None
        S None
        P None

        In [12]: new_origin.update_predarr_times(cfg_dict)
        Out[12]: 0

        In [13]: for arrival in new_origin.arrivals:
            ...:     print arrival.phase, arrival.predarr
            ...:
        S None
        P 1275439337.07
        S None
        P 1275439337.66
        P 1275439338.11
        S None
        P 1275439338.22
        S None
        P 1275439338.29
        S None
        P 1275439339.54
        S None
        S None
        S None
        P 1275439339.66
        S None
        S None
        S None
        S None
        S None
        S None
        P 1275439344.33
        S None
        S None
        P 1275439337.06
        S None
        P 1275439335.57
        S None
        P 1275439335.58
        S None
        P 1275439335.84
        S None
        P 1275439336.15
        S None
        P 1275439336.19
        S None
        P 1275439336.4
        S None
        P 1275439336.6
        S None
        P 1275439336.6
        S None
        S None
        S None
        P 1275439337.19
        S None
        P 1275439333.59
        '''
        #Get Propagation grid paramters
        ttdir = cfg_dict['tt_map_dir']
        prop_params = cfg_dict['propagation_grid']
        earth_rad = cfg_dict['earth_radius']
        nlat = prop_params['nlat']
        nlon = prop_params['nlon']
        nr = prop_params['nr']
        nz = nr
        li  =  LinearIndex(nlon, nlat, nz)
        olon = prop_params['minlon']
        olat = prop_params['minlat']
        #oz = prop_params['minz']
        origin_r = prop_params['minr']
        dlon = prop_params['dlon']
        dlat = prop_params['dlat']
        dr = prop_params['dr']
        dz = dr
        #Build vectors of geographic coordinates
        qlon = linspace(olon, dlon * nlon + olon, nlon, False)
        qlat = linspace(olat, dlat * nlat + olat, nlat, False)
        qdep = linspace(earth_rad - origin_r,
                        earth_rad - (origin_r + (nr - 1) * dr),
                        nr)
        endpoints, indices = find_containing_cube(self.lat,
                                                  self.lon,
                                                  self.depth,
                                                  qlat,
                                                  qlon,
                                                  qdep)
        for arrival in self.arrivals:
            if arrival.phase == 'P':
                if not os.path.isfile('%sbin.%s.traveltime'
                        % (ttdir, arrival.sta)):
                    continue
                ttvec = []
                for i in range(len(indices)):
                    index, endpoint = indices[i], endpoints[i]
                    #li1D = li.get_1D(index[1], index[0], index[2])
                    li1D = li.convert_to_1D(index[1], index[0], index[2])
                    #ttvec += [read_tt_vector([arrival.sta], li1D, ttdir)[0]]
                    ttvec += [pred_tts[arrival.sta][li1D]]
                dtt_dlat =  0 if endpoints[1][0] == endpoints[0][0] else\
                        (ttvec[1] - ttvec[0]) /\
                        (endpoints[1][0] - endpoints[0][0])
                dtt_dlon = 0 if endpoints[3][1] == endpoints[0][1] else\
                        (ttvec[3] - ttvec[0]) /\
                        (endpoints[3][1] - endpoints[0][1])
                dtt_ddep = 0 if endpoints[4][2] == endpoints[0][2] else\
                        (ttvec[4] - ttvec[0]) /\
                        (endpoints[4][2] - endpoints[0][2])
                delta_lon = self.lon - endpoints[0][1]
                delta_lat = self.lat - endpoints[0][0]
                delta_dep = self.depth - endpoints[0][2]
                tt = ttvec[0] + (dtt_dlon * delta_lon)\
                            + (dtt_dlat * delta_lat)\
                            + (dtt_ddep * delta_dep)
                predarr = self.time + tt
                arrival.tt_calc = tt
                arrival.predarr = predarr
        return 0

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
        '''
        Initialize loctools3D.core_tools.Arrival object.

        Arguments:
        sta - Station name.
        time - Epoch time of arrival observation.
        phase - Phase type (Eg. P, Pn, Pb, Pg, S, Sn, Sb, Sg)
        chan - Channel observation made on.
        deltim - Standard deviation of observed arrival time.
        qual - Signal onset quality (i: impulsive, e: emergent, w: weak).
        arid - Arrival ID.
        '''
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
        '''
        Return string representation for loctools3D.core_tools.Arrival
        object.
        '''
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

def grid_search_abs(qx, qy, qz, arrivals, pred_tts, linear_index):
    '''
    Find the minimum of the absolute value of the calculated origin
    time following Ben-Zion et al., 1992 (JGR)
    '''
    best_misfit = float('inf')
    nx = len(qx)
    ny = len(qy)
    nz = len(qz)
    stas = [arrival.sta for arrival in arrivals]
    for i in range(len(qx)):
        for j in range(len(qy)):
            for k in range(len(qz)):
                index = linear_index.convert_to_1D(i, j, k)
                if min([pred_tts[arrival.sta][arrival.phase][index] for \
                        arrival in arrivals]) < 0:
                    continue
                estimated_origin_times = [arrival.time -\
                        pred_tts[arrival.sta][arrival.phase][index] for \
                        arrival in arrivals]
                if len(estimated_origin_times) == 0:
                    continue
                origin_time = sum(estimated_origin_times) /\
                        len(estimated_origin_times)
                residuals = [estimated_origin_time - origin_time for\
                        estimated_origin_time in estimated_origin_times]
                misfit = sum([abs(residual) for residual in residuals])
                if misfit < best_misfit:
                    best_misfit = misfit
                    x, y, z = qx[i], qy[j], qz[k]
                    best_origin_time = origin_time
    return x, y, z, best_origin_time, best_misfit

#def grid_search_abs(self, arrsta, qx, qy, qz, arrvec, li):
##Find the minimum of the absolute value of the
##        calculated origin time following Ben-Zion et al., 1992 (JGR)
##   sta          list of station names; strings
##   qx,qy,qz    vectors of indices to search through
##   arrvec      vector of absolute arrivals in the same order as sta
##   li          Linear_index class for the entire traveltime grid
#    from numpy import array,indices #AAA MAY NEED TO DELETE
#    best_misfit=100000.0
#    search_inds=LinearIndex(len(qx),len(qy),len(qz))
#    for ix in range(len(qx)):  #Loop over the three vectors, searching every point
#        for iy in range(len(qy)):
#            for iz in range(len(qz)):
#                calctt=array([]); #initialize the calculated tt vector
#                ind=li.get_1D(qx[ix],qy[iy],qz[iz]) #Find the vector index
#                calctt=read_tt_vector(arrsta,
#                                      ind,
#                                      self.misc['tt_map_dir']) #Make traveltime vector from calculated times
#                if min(calctt)<0: #If the traveltime <0, this gridpoint is null
#                    continue
#                #Compute misfit measurements
#                #mini=calctt.argmin() #We will compute the origin time from the smallest traveltime Following Pavlis et al., 2004
#                orivec=arrvec-calctt #Take the difference
#                #otime=orivec[mini] #Origin time 
#                otime=orivec.mean()
#                res=orivec-otime
#                #weight=1/abs(res)/(1/abs(res)).sum()  #We can weight by e.g., residual here
#                #misfit=(abs(res)*weight).sum() #Weighted absolute value
#                misfit=(abs(res)).sum() #Absolute value
#                if misfit<best_misfit:
#                    best_misfit=misfit
#                    minx,miny,minz=qx[ix],qy[iy],qz[iz]
#                    best_ot=otime
#    return minx,miny,minz,best_ot,best_misfit
#    #return minx,miny,minz,origin_mean,misfit
