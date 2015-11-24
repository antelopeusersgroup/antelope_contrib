'''
db2kml_py.py

Create Google Earth KML source from
database origin and/or site tables

@author      Juan Reyes
@credits     Rewrite of an original Perl script (db2kml) by
             Michael West of the Volcano Observatory, University
             of Alaska Fairbanks.
@credits 2   Rewrite 2 by Rob Newman <robertlnewman@gmail.com>
'''

import sys
import os
import time
import math
import zipfile
from optparse import OptionParser

# Import Antelope modules
import antelope.datascope as datascope
import antelope.stock as stock

def configure():
    """Gather command line
    options"""
    usage = "Usage: %prog [options] database output_file.kml"
    parser = OptionParser(usage=usage)
    parser.add_option("-v", action="store_true", dest="verbose", help="verbose output", default=False)
    parser.add_option("-x", action="store_true", dest="debug", help="debug output", default=False)
    parser.add_option("-p", action="store", type="string", dest="pf", help="pf", default='db2kml_py.pf')
    parser.add_option("-t", action="store", type="string", dest="filetype", help="filetype", metavar="", default=False)
    (options, args) = parser.parse_args()

    if len(args) != 2:
        parser.error("incorrect number of arguments")

    database = args[0]
    out_file = args[1]

    verbose = False
    debug = False
    file_type = 'all'

    if options.verbose:
        verbose = True

    if options.debug:
        debug = True

    if verbose:
        print "- Read configuration parameter file (pf): %s" % options.pf

    pf = stock.pfupdate(options.pf)

    #if not options.pf:
    #    pfs_tuple = list(stock.pffiles('db2kml_py'))
    #    pfs_tuple.reverse() # Reverse order to search local pf dir first
    #    for p in pfs_tuple:
    #        if os.path.isfile(p):
    #            pfname = p
    #            break
    #    if verbose or debug:
    #        print "Used PFPATH to determine which parameter file to use and found '%s'" % pfname
    #else:
    #    if not os.path.isfile(options.pf):
    #        print "Command line defined parameter file '%s' does not exist. Exiting" % options.pf
    #        sys.exit(-1)
    #    else:
    #        pfname = options.pf

    if options.filetype:
        file_type = options.filetype

    return database, out_file, verbose, debug, pf, file_type

def get_pf(pf, verbosity=0):
    """Get values from the parameter file
    """

    #pf = stock.pfread(pf)

    config = pf['config']

    styles = pf['styles']
    if verbosity:
        print "%s" % styles

    stations = pf['stations']
    if not styles['imagepath'].endswith('/'):
        err_str = []
        err_str.append('ERROR: imagepath URL must contain a trailing / in parameter file.')
        err_str.append('Imagepath currently defined as: %s\n' % styles['imagepath'])
        print err_str
        sys.exit()
    headers = pf['headers']
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

def get_orig_records(database, pf, verbosity=0):
    """Return a list of all events
    from the database
    """
    fields = ['time', 'lat', 'lon', 'depth', 'auth', 'mb', 'ms', 'ml', 'magnitude', 'magtype']
    db_pointer = datascope.dbopen(database, 'r');
    db_pointer = db_pointer.lookup(table='origin')
    tbl_event = db_pointer.dblookup(table='event')
    if tbl_event.dbquery('dbTABLE_PRESENT') > 0:
        if verbosity > 1:
            print " - Join event table"
        db_pointer = db_pointer.join('event')
        db_pointer = db_pointer.subset('orid==prefor')
    else:
        if verbosity > 1:
            print " * NOTE: Cannot join 'event' table"
    tbl_netmag = db_pointer.dblookup(table='netmag')
    if tbl_netmag.query('dbTABLE_PRESENT'):
        if verbosity > 1:
            print " - Join netmag table"
        db_pointer = db_pointer.join('netmag', outer=True)
    else:
        if verbosity > 1:
            print " * NOTE: Cannot join 'netmag' table"
    my_expr = pf['config']['expr']
    if pf['config']['expr']:
        for mex in pf['config']['expr']:
            db_pointer = db_pointer.subset('%s' % mex)
            if verbosity > 0:
                print " - Origin subset '%s' resulted in %d records" % (mex, db_pointer.query('dbRECORD_COUNT'))
    my_sort_fields = pf['config']['sort_fields']
    if db_pointer.query('dbRECORD_COUNT') < 1:
        print "* ERROR: Database subsets failed to return any records. Check expr in config section of parameter file."
        sys.exit(1)
    for msf in my_sort_fields:
        db_pointer = db_pointer.sort('%s' % msf)
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
            db_pointer.record = i
            for f in fields:
                ev_dict[f] = db_pointer.getv(f)[0]
                if f == 'time':
                    # Convert to XML standard time
                    ev_dict['time_xml'] = stock.epoch2str(ev_dict[f], "%Y-%m-%dT%H:%M:%SZ")
                    ev_dict['time_readable'] = stock.epoch2str(ev_dict[f], "%Y-%m-%d %H:%M:%S UTC")
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
    siteplace.append("\t\t\t<visibility>%s</visibility>\n" % visibility)
    siteplace.append("\t\t\t<description>\n")
    siteplace.append("\t\t\t\t<![CDATA[\n")
    siteplace.append("<p><strong>%s_%s</strong></p>\n" % (meta_dict_site['snet'],meta_dict_site['sta']))
    siteplace.append("<hr>\n")
    for sKey in sorted(meta_dict_site.iterkeys()):
        sVal = meta_dict_site[sKey]
        if sKey == 'sta' or sKey == 'snet':
            pass
        else:
            if sKey == 'elev':
                sVal = str(sVal) + ' meters'
            if isinstance(sVal, int) and sVal < 0:
                sVal = '-'
            siteplace.append("<p><strong>%s:</strong> %s</p>\n" % (sKey.capitalize(), sVal))
    siteplace.append("\t\t\t\t]]>\n")
    siteplace.append("\t\t\t</description>\n")
    siteplace.append("\t\t\t<styleUrl>#%s</styleUrl>\n" % style)
    #siteplace.append("\t\t\t<Style><IconStyle><scale>0.7</scale><color>FFFFFFFF</color></IconStyle></Style>\n")
    siteplace.append("\t\t\t<Point>\n\t\t\t\t<altitudeMode>absolute</altitudeMode>\n")
    siteplace.append("\t\t\t\t<coordinates>%s,%s,%s</coordinates>\n" % (meta_dict_site['lon'], meta_dict_site['lat'], meta_dict_site['elev']))
    siteplace.append("\t\t\t</Point>\n\t\t</Placemark>\n")
    return ''.join(siteplace)

def get_site_records(dbmaster, stylestation, staexpr, fields, visibility, inactive, verbosity=0):
    """Get all the sites
    in the dbmaster
    """
    if verbosity > 0:
        print "- Subsetting database '%s' for %s" % (dbmaster,staexpr)

    try:
        db = datascope.dbopen(dbmaster, 'r')
    except:
        sys.exit( "* Cannot open database '%s'" % dbmaster )


    dbm = db.lookup(table='site')
    if not dbm.query('dbTABLE_PRESENT'):
        sys.exit( "* Cannot open site table '%s'" % dbmaster )


    test_net = db.lookup(table='snetsta')
    if test_net.query('dbTABLE_PRESENT'):
        if verbosity > 1:
            print " - Join snetsta table"
        dbm = dbm.join('snetsta', outer=True)
    else:
        if verbosity > 1:
            print " * NOTE: Cannot join 'snetsta' table"


    for nex in staexpr:
        if nex:
            dbm = dbm.subset('%s' % nex )

    if not stock.yesno(inactive):
        if verbosity > 0:
            print "- Subsetting database '%s' for active stations" % dbmaster
        dbm = dbm.subset('offdate == NULL || offdate > %s' % stock.yearday(stock.now()) )


    if dbm.query('dbRECORD_COUNT') < 1:
        if len(staexpr) > 0:
            print "* ERROR: Dbmaster database (%s) generated view contains no records. Check your expressions." % dbmaster
        else:
            print "* ERROR: Dbmaster database (%s) generated view contains no records." % dbmaster
        sys.exit(1)

    sitestr = ["\t<Folder>"]
    sitestr.append("\t\t<visibility>%s</visibility>" % visibility)
    sitestr.append("\t\t<name>Stations</name>")

    for i in range(dbm.query('dbRECORD_COUNT')):
        dbm.record = i
        per_sta_info = {}
        for f in fields:
            per_sta_info[f] = dbm.getv(f)[0]
            if f == 'elev':
                per_sta_info[f] = per_sta_info[f] * 1000 # convert km to meters for correct GE rendering

            if f == 'snet':
                if per_sta_info[f] in stylestation:
                    stastyle = per_sta_info[f]
                else:
                    stastyle = 'others'

        #if inactive:
        #    if dbm.getv('offdate')[0] < 0: # dbNULL value is -1
        #        stastyle = 'activeStation'
        #    else:
        #        stastyle = 'inactiveStation'
        #else:
        #    stastyle = 'activeStation'
        # Lets color them by network...

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
    '''
    return kmlstart % (params[0], params[1])

def kml_lookat(params):
    """Define basic kml
    header string"""
    kmlstart = '''
        <LookAt>
            <longitude>%s</longitude>
            <latitude>%s</latitude>
            <range>%s</range>
            <altitude>0</altitude>
            <tilt>0</tilt>
            <heading>0</heading>
        </LookAt>
    '''
    return kmlstart % (params[0], params[1], params[2])

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
            try:
                icon_style_scale = my_styles[key]['iconscale']
            except:
                icon_style_scale = my_styles[key]['scale']
            style_href = pf_result['styles']['imagepath'] + my_styles[key]['href']
            styleout = '''
            <Style id="%s">
                <ListStyle><ItemIcon><href>%s</href></ItemIcon></ListStyle>
                <IconStyle><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle>
                <LabelStyle><scale>%s</scale></LabelStyle>
                <BalloonStyle><text>$[description]</text><bgColor>ffffffff</bgColor><textColor>ff000000</textColor></BalloonStyle>
            </Style>
            '''
            style_list.append(styleout % (style_id, style_href, icon_style_scale, style_href, style_scale))
    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'stations':
        for key in sorted(my_sta_styles.iterkeys()):
            style_id = my_sta_styles[key]['id']
            style_scale = my_sta_styles[key]['scale']
            try:
                icon_style_scale = my_sta_styles[key]['iconscale']
            except:
                icon_style_scale = my_sta_styles[key]['scale']
            style_href = pf_result['styles']['imagepath'] + my_sta_styles[key]['href']
            sta_styleout = '''
            <Style id="%s">
                <ListStyle><ItemIcon><href>%s</href></ItemIcon></ListStyle>
                <IconStyle><scale>%s</scale><Icon><href>%s</href></Icon></IconStyle>
                <LabelStyle><scale>%s</scale></LabelStyle>
                <BalloonStyle><text>$[description]</text><bgColor>ffffffff</bgColor><textColor>ff000000</textColor></BalloonStyle>
            </Style>
            '''
            style_list.append(sta_styleout % (style_id, style_href, icon_style_scale, style_href, style_scale))
    return ''.join(style_list)

def main():
    """Main functionality
    for creating KML files
    """

    database, out_file, verbose, debug, pf, file_type = configure()
    verbosity = calc_verbosity(verbose, debug)

    if verbosity > 0:
        print "Start of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())

    pf_result = get_pf(pf, verbosity)

    outstr = []
    outstr.append(kml_header())
    if pf_result['config']['network_link']:
        expires_time = time.time() + pf_result['config']['network_link']['refresh_rate']
        outstr.append(kml_network(expires_time))

    outstr.append(kml_start([
        pf_result['headers']['name'],
        pf_result['headers']['description'],
    ]))

    if pf_result['headers']['set_look_at']:
        outstr.append(kml_lookat([
            str(pf_result['headers']['look_at']['longitude']),
            str(pf_result['headers']['look_at']['latitude']),
            str(pf_result['headers']['look_at']['range'])
        ]))

    if pf_result['headers']['generate_legend']:
        if verbosity > 0:
            print "- Write out legend link"
        outstr.append(generate_legend(pf_result['headers']['legend_url']))

    if verbosity > 0:
        print "- Generating styles"
    outstr.append(kml_styles(pf_result, verbosity))

    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'events':
        if verbosity > 0:
            print "- Generating event icons"
        outstr.append(get_orig_records(database, pf_result, verbosity))

    if not pf_result['config']['subset'] or pf_result['config']['subset'] == 'stations':
        if verbosity > 0:
            print "- Generating station icons from database '%s'" % database
        outstr.append(
            get_site_records(
                database,
                pf_result['styles']['stylestation'],
                pf_result['stations']['expr'],
                pf_result['stations']['fields'],
                pf_result['stations']['visibility'],
                pf_result['stations']['display_inactive'],
                verbosity
            )
        )

    outstr.append(kml_footer())

    if pf_result['config']['create_kmz']:
        write_kml(out_file, outstr, verbosity, True)

    write_kml(out_file, outstr)

    if verbosity > 0:
        print "End of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime())

    return 0

if __name__ == '__main__':
    status = main()
    sys.exit(status)
