'''
db2kml_py.py

Create Google Earth KML source from 
database origin and/or site tables

@author      Rob Newman <robertlnewman@gmail.com> 858.822.1333
@created     2010-05-26
@modified    2011-11-17
@version     1.0
@license     MIT-style license
@credits     Rewrite of an original Perl script (db2kml) by 
             Michael West of the Volcano Observatory, University 
             of Alaska Fairbanks. This verions uses the Antelope 
             Python interface and adds some more functionality 
             with dbsubsets etc. Also add verbosity options.
'''

import sys
import os
import time
import math
import zipfile
from optparse import OptionParser

# Import Antelope modules
sys.path.append('%s/local/data/python/antelope' % os.environ['ANTELOPE'])
import datascope as antdb
import stock as antstock

def configure():
    """Gather command line
    options"""
    usage = "Usage: %prog [options]"
    parser = OptionParser(usage=usage)
    parser.add_option("-v", action="store_true", dest="verbose", help="verbose output", default=False)
    parser.add_option("-x", action="store_true", dest="debug", help="debug output", default=False)
    parser.add_option("-p", action="store", type="string", dest="pf", help="pf", metavar="", default=False)
    parser.add_option("-t", action="store", type="string", dest="filetype", help="filetype", metavar="", default=False)
    (options, args) = parser.parse_args()
    if options.verbose:
        verbose = True
    else:
        verbose = False
    if options.debug:
        debug = True
    else:
        debug = False
    if not options.pf:
        pfs_tuple = list(antstock.pffiles('db2kml_py'))
        pfs_tuple.reverse() # Reverse order to search local pf dir first
        for p in pfs_tuple:
            if os.path.isfile(p):
                pfname = p
                break
        if verbose or debug:
            print "Used PFPATH to determine which parameter file to use and found '%s'" % pfname
    else:
        if not os.path.isfile(options.pf):
            print "Command line defined parameter file '%s' does not exist. Exiting" % options.pf
            sys.exit(-1)
        else:
            pfname = options.pf
    if options.filetype:
        file_type = options.filetype
    else:
        file_type = 'all'
    return verbose, debug, pfname, file_type

def get_pf(pf, verbosity=0):
    """Get values from the parameter file
    """
    if verbosity > 0:
        print "- Read configuration parameter file (pf): %s" % pf
    if not os.path.isfile(pf):
        print "ERROR: cannot locate parameter file (pf): %s" % pf
        sys.exit()
    config = antstock.pfget_arr(pf, 'config')
    styles = antstock.pfget_arr(pf, 'styles')
    stations = antstock.pfget_arr(pf, 'stations')
    if not styles['imagepath'].endswith('/'):
        err_str = []
        err_str.append('ERROR: imagepath URL must contain a trailing / in parameter file.')
        err_str.append('Imagepath currently defined as: %s\n' % styles['imagepath'])
        print err_str
        sys.exit()
    headers = antstock.pfget_arr(pf, 'headers')
    pf_result = {'config': config, 'styles': styles, 'headers': headers, 'stations': stations}
    return pf_result

def calc_verbosity(verbose=False, debug=False):
    """Calculate the verbosity
    of the script"""
    verbosity = 0
    if verbose:
        verbosity = 1
    if debug:
        verbosity = 2
    return verbosity

def calc_magtype(ev_dict):
    """Determine the magnitude
    and scale to use per event
    Assumes a netmag table
    is present"""
    if ev_dict['magnitude'] > 0:
        mag = ev_dict['magnitude']
        mag_sc = ev_dict['magtype']
    elif ev_dict['mb'] > 0:
        mag = ev_dict['mb']
        mag_sc = "Mb"
    elif ev_dict['ms'] > 0:
        mag = ev_dict['ms']
        mag_sc = "Ms"
    elif ev_dict['ml'] > 0:
        mag = ev_dict['ml']
        mag_sc = "Ml"
    else:
        mag = '-'
        mag_sc = ""
    return mag, mag_sc
 
def get_orig_records(pf, verbosity=0):
    """Return a list of all events
    from the database
    """
    fields = ['time', 'lat', 'lon', 'depth', 'auth', 'mb', 'ms', 'ml', 'magnitude', 'magtype']
    db_pointer = antdb.dbopen(pf['config']['db'], 'r');
    db_pointer.lookup(table='origin')
    tbl_event = antdb.dblookup(db_pointer, table='event')
    if antdb.dbquery(tbl_event, 'dbTABLE_PRESENT') > 0:
        if verbosity > 1:
            print " - Join event table"
        db_pointer.join('event')
        db_pointer.subset('orid==prefor')
    else:
        if verbosity > 1:
            print " * NOTE: Cannot join 'event' table"
    tbl_netmag = antdb.dblookup(db_pointer, table='netmag')
    if tbl_netmag.query('dbTABLE_PRESENT'):
        if verbosity > 1:
            print " - Join netmag table"
        db_pointer.join('netmag', outer=True)
    else:
        if verbosity > 1:
            print " * NOTE: Cannot join 'netmag' table"
    my_expr = pf['config']['expr']
    if pf['config']['expr']:
        for mex in pf['config']['expr']:
            db_pointer.subset('%s' % mex)
            if verbosity > 0:
                print " - Origin subset '%s' resulted in %d records" % (mex, db_pointer.query('dbRECORD_COUNT'))
    my_sort_fields = pf['config']['sort_fields']
    if db_pointer.query('dbRECORD_COUNT') < 1:
        print "* ERROR: Database subsets failed to return any records. Check expr in config section of parameter file."
        sys.exit(1)
    for msf in my_sort_fields:
        db_pointer.sort('%s' % msf)
    if db_pointer.query('dbRECORD_COUNT') < 1:
        print "* ERROR: Database sorts failed. Check sort_fields in parameter file."
        sys.exit(1)
    else:
        if verbosity > 0:
            print "- Number of records: %s" % db_pointer.query('dbRECORD_COUNT')
        oridout = ["\t<Folder>\n"]
        oridout.append("\t\t<name>Earthquakes</name>\n")
        oridout.append("\t\t<visibility>1</visibility>\n")
        for i in range(db_pointer.query('dbRECORD_COUNT')):
            ev_dict = {}
            db_pointer[3] = i
            for f in fields:
                ev_dict[f] = db_pointer.getv(f)[0]
                if f == 'time':
                    # Convert to XML standard time
                    ev_dict['time_xml'] = antstock.epoch2str(ev_dict[f], "%Y-%m-%dT%H:%M:%SZ")
                    ev_dict['time_readable'] = antstock.epoch2str(ev_dict[f], "%Y-%m-%d %H:%M:%S UTC")
            mag, mag_sc = calc_magtype(ev_dict)
            if mag != '-':
                style_mag = "mag_" + str(int(math.ceil(mag)))
            else:
                style_mag = "mag_0"
            style_mag_id = pf['styles']['styleinfo'][style_mag]['id']
            oridout.append("\t\t\t<Placemark>\n")
            oridout.append("\t\t\t\t<name></name>\n") # keep blank else too much noise on the map
            oridout.append("\t\t\t\t<styleUrl>#%s</styleUrl>\n" % style_mag_id)
            oridout.append("\t\t\t\t<Point><coordinates>%s,%s</coordinates></Point>\n" % (ev_dict['lon'],ev_dict['lat']))
            oridout.append("\t\t\t\t<TimeStamp><when>%s</when></TimeStamp>\n" % ev_dict['time_xml'])
            oridout.append("\t\t\t\t<description>\n")
            oridout.append("<![CDATA[\n")
            oridout.append("<p><strong>Latitude:</strong> %s</p>\n" % ev_dict['lat'])
            oridout.append("<p><strong>Longitude:</strong> %s</p>\n" % ev_dict['lon'])
            oridout.append("<p><strong>Depth (km):</strong> %s</p>\n" % ev_dict['depth'])
            oridout.append("<p><strong>Magnitude:</strong> %s %s</p>\n" % (mag, mag_sc))
            oridout.append("<p><strong>Origin time:</strong> %s</p>\n" % ev_dict['time_readable'])
            oridout.append("<p><strong>Author:</strong> %s</p>\n" % ev_dict['auth'])
            oridout.append("]]>\n")
            oridout.append("\t\t\t\t</description>\n")
            oridout.append("\t\t\t</Placemark>\n")
        oridout.append("\t</Folder>\n")
    return ''.join(oridout)

def create_site(meta_dict_site, visibility, style):
    """Create KML for site icons
    """
    siteplace = ["\t\t<Placemark>\n\t\t\t<name>%s</name>\n" % meta_dict_site['sta']]
    siteplace.append("\t\t\t<visibility>%d</visibility>\n" % visibility)
    siteplace.append("\t\t\t<description>\n")
    siteplace.append("\t\t\t\t<![CDATA[\n")
    siteplace.append("<p><strong>%s</strong></p>\n" % meta_dict_site['sta'])
    for sKey in sorted(meta_dict_site.iterkeys()):
        sVal = meta_dict_site[sKey]
        if sKey == 'elev':
            sVal = str(sVal) + ' meters'
        if isinstance(sVal, int) and sVal < 0:
            sVal = '-'
        if sKey is not 'sta':
            siteplace.append("<p><strong>%s:</strong> %s</p>\n" % (sKey.capitalize(), sVal))
    siteplace.append("\t\t\t\t]]>\n")
    siteplace.append("\t\t\t</description>\n")
    siteplace.append("\t\t\t<styleUrl>#%s</styleUrl>\n" % style)
    siteplace.append("\t\t\t<Style><IconStyle><scale>0.7</scale><color>FFFFFFFF</color></IconStyle></Style>\n")
    siteplace.append("\t\t\t<Point>\n\t\t\t\t<altitudeMode>absolute</altitudeMode>\n")
    siteplace.append("\t\t\t\t<coordinates>%s,%s,%s</coordinates>\n" % (meta_dict_site['lon'], meta_dict_site['lat'], meta_dict_site['elev']))
    siteplace.append("\t\t\t</Point>\n\t\t</Placemark>\n")
    return ''.join(siteplace)

def get_site_records(dbmaster, staexpr, fields, visibility, inactive, verbosity=0):
    """Get all the sites 
    in the dbmaster
    """
    try:
        dbm = antdb.dbopen(dbmaster, 'r')
    except:
        print "* Cannot open database '%s'" % dbmaster
    else:
        dbm.lookup(table='site')
        for nex in staexpr:
            dbm.subset('%s' % nex )
        if not inactive or inactive == 0:
            if verbosity > 0:
                print "- Subsetting database '%s' for active stations" % dbmaster
            dbm.subset('offdate < 0') # Just get active stations
        if dbm.query('dbRECORD_COUNT') < 1:
            if len(staexpr) > 0:
                print "* ERROR: Dbmaster database (%s) generated view contains no records. Check your expressions." % dbmaster
            else:
                print "* ERROR: Dbmaster database (%s) generated view contains no records." % dbmaster
            sys.exit(1)
        sitestr = ["\t<Folder>"]
        sitestr.append("\t\t<visibility>%d</visibility>" % visibility)
        sitestr.append("\t\t<name>Stations</name>")

        for i in range(dbm.query('dbRECORD_COUNT')):
            dbm[3] = i
            per_sta_info = {}
            for f in fields:
                per_sta_info[f] = dbm.getv(f)[0]
                if f == 'elev':
                    per_sta_info[f] = per_sta_info[f] * 1000 # convert km to meters for correct GE rendering

            if inactive and inactive == 1:
                inactive_offdate = dbm.getv('offdate')[0]
                if inactive_offdate < 0: # dbNULL value is -1
                    stastyle = 'activeStation'
                else:
                    stastyle = 'inactiveStation'
            else:
                stastyle = 'activeStation'
            sitestr.append(create_site(per_sta_info, visibility, stastyle))
        sitestr.append("\t</Folder>")
    return ''.join(sitestr)

def generate_legend(legendurl):
    """Generate legend
    """
    overlaycoords = '''
    <ScreenOverlay>
        <name>Legend image</name>
        <Icon>
            <href>%s</href>
        </Icon>
        <overlayXY x="0.02" y="0.02" xunits="fraction" yunits="fraction" />
        <screenXY x="0.02" y="0.02" xunits="fraction" yunits="fraction" />
        <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction" />
        <size x="-1" y="-1" xunits="pixels" yunits="pixels" />
    </ScreenOverlay>
'''
    return overlaycoords % legendurl

def kml_header():
    """Define basic kml 
    header string"""
    kml_header = '''<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">
    '''
    return kml_header

def kml_start(params):
    """Define basic kml 
    header string"""
    kmlstart = '''
    <Document>
        <name>%s</name>
        <open>1</open>
        <description>%s</description>
        <LookAt>
            <longitude>%s</longitude>
            <latitude>%s</latitude>
            <range>%s</range>
            <altitude>0</altitude>
            <tilt>0</tilt>
            <heading>0</heading>
        </LookAt>
    '''
    return kmlstart % (params[0], params[1], params[2], params[3], params[4])

def kml_network(params):
    """Add a Network Link"""
    kml_network = '''
    <NetworkLinkControl>
        <expires>%s</expires>
    </NetworkLinkControl>
    '''
    return kml_network % params
 
def kml_footer():
    """Create KML footer"""
    kml_footer = '''
    </Document>
    </kml>
    '''
    return kml_footer

def write_kml(target_file, outlist, verbosity=0, create_kmz=False):
    """Write KML out to disk
    and create KMZ if defined"""
    try:
        file = open(target_file, 'w')
    except IOError as (errno, strerror):
        print "I/O error({0}): {1}".format(errno, strerror)
    except ValueError:
        print "Could not convert data to an integer"
    else:
        final_str = ''.join(outlist)
        file.write(final_str)
        file.close()

        if os.path.exists(target_file) and create_kmz:
            file_substr = target_file[:-3] + 'kmz'
            if verbosity > 0:
                print "- Creating zipfile '%s'" % file_substr
            zf = zipfile.ZipFile(file_substr, mode='w')
            try:
                zf.write(target_file)
            finally:
                zf.close()
                os.remove(target_file)
                if verbosity > 0:
                    print "- Finished creating KMZ zipfile '%s'" % file_substr
        elif os.path.exists(target_file):
            if verbosity > 0:
                print "- Finished creating KML file '%s'" % target_file
        else:
            print "- KML file '%s' not created. Something went wrong" % target_file
    return

def kml_styles(pf_result, verbosity=0):
    """Create KML styles"""
    style_list = []
    if verbosity > 0:
        print "- Write out styles"
    my_styles = pf_result['styles']['styleinfo']
    my_sta_styles = pf_result['styles']['stylestation']
    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'events':
        for key in sorted(my_styles.iterkeys()):
            style_id = my_styles[key]['id']
            style_scale = my_styles[key]['scale']
            style_href = pf_result['styles']['imagepath'] + my_styles[key]['href']
            styleout = '''
            <Style id="%s">
                <ListStyle><ItemIcon><href>%s</href></ItemIcon></ListStyle>
                <IconStyle><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle>
                <LabelStyle><scale>%s</scale></LabelStyle>
                <BalloonStyle><text>$[description]</text><bgColor>ffffffff</bgColor><textColor>ff000000</textColor></BalloonStyle>
            </Style>
            '''
            style_list.append(styleout % (style_id, style_href, style_scale, style_href, style_scale))
    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'stations':
        for key in sorted(my_sta_styles.iterkeys()):
            style_id = my_sta_styles[key]['id']
            style_scale = my_sta_styles[key]['scale']
            style_href = pf_result['styles']['imagepath'] + my_sta_styles[key]['href']
            sta_styleout = '''
            <Style id="%s">
                <ListStyle><ItemIcon><href>%s</href></ItemIcon></ListStyle>
                <IconStyle><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle>
                <LabelStyle><scale>%s</scale></LabelStyle>
                <BalloonStyle><text>$[description]</text><bgColor>ffffffff</bgColor><textColor>ff000000</textColor></BalloonStyle>
            </Style>
            '''
            style_list.append(sta_styleout % (style_id, style_href, style_scale, style_href, style_scale))
    return ''.join(style_list)

def main():
    """Main functionality 
    for creating KML files
    """

    verbose, debug, pf, file_type = configure()
    verbosity = calc_verbosity(verbose, debug) 

    if verbosity > 0:
        print "Start of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())

    pf_result = get_pf('%s' % pf, verbosity)

    outstr = []
    outstr.append(kml_header())
    if pf_result['config']['network_link']:
        expires_time = time.time() + pf_result['config']['network_link']['refresh_rate']
        outstr.append(kml_network(expires_time))
    outstr.append(kml_start([
        pf_result['headers']['name'], 
        pf_result['headers']['description'], 
        str(pf_result['headers']['look_at']['longitude']), 
        str(pf_result['headers']['look_at']['latitude']), 
        str(pf_result['headers']['look_at']['range'])
    ]))

    if verbosity > 0:
        print "- Write out legend link"
    outstr.append(generate_legend(pf_result['headers']['legend_url']))

    if verbosity > 0:
        print "- Generating styles"
    outstr.append(kml_styles(pf_result, verbosity))

    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'events':
        if verbosity > 0:
            print "- Generating event icons"
        outstr.append(get_orig_records(pf_result, verbosity))

    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'stations':
        if verbosity > 0:
            print "- Generating station icons from dbmaster '%s'" % pf_result['stations']['dbmaster']
        outstr.append(
            get_site_records(
                pf_result['stations']['dbmaster'], 
                pf_result['stations']['expr'], 
                pf_result['stations']['fields'], 
                pf_result['stations']['visibility'], 
                pf_result['stations']['display_inactive'],
                verbosity
            )
        )

    outstr.append(kml_footer())

    if pf_result['config']['create_kmz']:
        write_kml(pf_result['config']['out_file'], outstr, verbosity, True)
    else:
        write_kml(pf_result['config']['out_file'], outstr)

    if verbosity > 0:
        print "End of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())

    return 0

if __name__ == '__main__':
    status = main()
    sys.exit(status)
