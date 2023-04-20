
"""
db2kml_py.py

Create Google Earth KML source from
database origin and/or site tables

@author      Jennifer Eakins
@credits     Rewrite of Juan Reyes' rewrite of an original 
             Perl script (db2kml) by Michael West of the 
             Volcano Observatory, University of Alaska Fairbanks.
@updates     Make Python3 compatible 
             Move away from print statements to simplekml 
             Depth colorization
             Dynamic symbol styles based on ANF api
            

"""

import sys
import os
import time
import math
from pathlib import Path
import shutil

import argparse
import simplekml
import tempfile
import warnings

import re
import requests 
from bisect import bisect_right, bisect_left
from PIL import Image

# Import Antelope modules
import antelope.datascope as datascope
import antelope.stock as stock


def parseepoch( t ):
    """Parse epoch time and covert to day only with no hours.
    """

    if not int( t ) or t > stock.now():
        t = stock.now()

    return stock.str2epoch( stock.strdate( t ) )


def parseArgs(argv=None):
  """Set up collection of arguments
  """

  parser = argparse.ArgumentParser(description="generate kml for sites and origins from db")

  group = parser.add_argument_group('Debugging', 'higher levels of vebosity')
  group.add_argument("-d", "--debug", action="store_true", help="really verbose, useful for debugging only")
  group.add_argument("-v", "--verbose", action="store_true", help="verbose print, shows progress of script")
  group.add_argument("-q", "--quiet", action="store_true", help="suppress any std output")
  group.add_argument("-t", "--testoverlay",  action='store_const', const='logotest.kmz', help="test overlay/logo placement by saving to a kmz file called logotest.kmz")


  parser.add_argument("db", type=str, help="input database")
  parser.add_argument("kmlfile", type=str, help="kml or kmz output file")

  parser.add_argument("-p", "--pf", default="db2kml_py.pf", help="parameter file")

  kgroup = parser.add_argument_group('KML options', 'kml plotting behavior modifications')
  kgroup.add_argument("-z", action='store_true', help="save output as kmz")
  kgroup.add_argument("-k", nargs='?', const='kmzfiles', metavar='img_savedir', help="save images used for kmz in this directory")
  kgroup.add_argument("-c", default="all", const="all", nargs="?", choices=["stations","origins","all"], help="generate kml for stations, origins, or both (default: %(default)s)") 
  kgroup.add_argument("-opm",  metavar='field:model', help="origin placemark plotting model, maps to field model for origin placemarks in pf") 


  return(parser.parse_args(argv),parser)

def get_pf(pf,args):
    """Get values from the parameter file
 
       Tests that headers section of pf file has been modified from default

       Returns:  pf_result - a dictionary containing 'config', 'styles', 'headers' and 'overlays'
    """

    config   = pf['config']
    styles   = pf['styles']
    headers  = pf['headers']
    overlays = pf['overlays']

    # bare minimum check to see if headers has been updated from default pf settings
    
    hn = "Change Name Here or script wil die!!"
    hd = "Your network described here"
      
    if headers['name'] == hn :
       sys.exit("FATAL: No edit found for name in headers section!\nYou must modify the headers section of %s, or specify a modified pf with the -p option" % args.pf )
    elif re.search(hd,headers['description']): 
       sys.exit("FATAL: check headers description!\nYou must modify the headers section of %s, or specify a modified pf with the -p option" % args.pf )
    else: 
       dprint("Passed minimal check for pf modifications")

    # insert check here to verify that a default catchall of 'others' is defined in styles

    if 'others' not in styles['stations']['by_network'].keys():
       sys.exit("Must define 'others' named array in styles:stations:by_network\nCheck your pf file")

    pf_result = {'config': config, 'styles': styles, 'headers': headers, 'overlays': overlays}

    return pf_result

def calc_magtype(ev_dict):
    """Determine the magnitude and scale to use per event

       Assumes a netmag table is present

       Argument: ev_dict contains origin or site information collected from datascope database

       Returns: magnitude value, and magnitude type
    """

    if ev_dict['magnitude'] != -99.99 :
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
        mag_sc = '-'
    return mag, mag_sc

def get_magscale(pfstyle,mag):
    """Return a scale factor given an origin  magnitude from database
       using magscale as defined in pf file

       If magnitude is null (-99.99) or less than the smallest value 
       in magscale from pf, then use the smallest value in magscale
    """

    mymagsize = pfstyle['magscale']

    # convert strings to ints 
    mymagsize = {int(k):v for k,v in mymagsize.items()}
    mymagsize = dict(sorted(mymagsize.items()))

    if mag == '-' or mag < mymagsize[0]:
       return mymagsize[0]

    msr = bisect_right(list(mymagsize.keys()),float(mag))

    if msr:
       dprint("get_magscale: Find rightmost value less than or equal to ", mag)
       dprint("get_magscale: index of this value is: ", mag)
       dprint("get_magscale: maybe magscale to use is: ", list(mymagsize.values())[msr-1])
       return list(mymagsize.values())[msr-1]

    raise ValueError

def choose_right_value (num_style, val):
    """Given a database value, val (perhaps depth or magnitude), 
       map that value to a key:value in the num_style dictionary.

       For example, you have an event with a depth of 3.2km, what
       is the proper color to map that to given depthcolor in the pf file?
 
       Returns: list item of the mapped value (i.e. num_style[mapped_value])
    """

    dprint("choose_right_value: from dictionary:", num_style)
    dprint("choose_right_value: keys of dictionary:", num_style.keys())
    dprint("choose_right_value: checking value %s" % val)

    br = bisect_right(list(num_style.keys()), float(val))
    bl = bisect_left(list(num_style.keys()), float(val))

    dprint("choose_right_value: output of bisect_right:", br)

    if br:
       dprint("choose_right_value: Find rightmost value less than or equal to ", val)
       dprint("choose_right_value: index of this value is: ", br)
       dprint("choose_right_value: maybe value to use is: ", list(num_style.values())[br-1])

       # despite examples https://docs.python.org/3/library/bisect.html, I need to return list value
       #return num_style[br-1]

       return list(num_style.values())[br-1]
 
    elif br == 0 :
       dprint("choose_right_value: Value %s is smaller than defined in input dictionary." %  val)
       if br != len(list(num_style.keys())):
          rval = list(num_style.values())[br]
          dprint("choose_right_value: Choosing %s instead (the leftmost value gte %s)"  % ( list(num_style.values())[br], val) )
          return list(num_style.values())[br]
       raise ValueError
   

def get_origin_records(database, pf, opmtype_style, ofol, sfol, sharedstyle, icon_method):
    """Collect a list of events from the database 

    Subset(s) from pf file are applied

    Calls: 
       generate_origin_pts - which styles and plots the points, adding them to the kml file
    """

    vprint ("\n - Collecting origin info from database: %s" % database)

    fields = ['time', 'lat', 'lon', 'depth', 'auth', 'mb', 'ms', 'ml', 'magnitude', 'magtype', 'review']

    db = datascope.dbopen(database, 'r');
    dborigin = db.lookup(table='origin')
    dbevent  = db.lookup(table='event')

    if dbevent.query('dbTABLE_PRESENT') > 0:
        vprint ("  - Join event table")
        dbj = dborigin.join('event')
        if stock.yesno(pf_result['config']['origins']['prefor_only']):
           vprint ("   - Per prefor_only in pf, only plotting prefor")
           dbj = dbj.subset('orid==prefor')
    else:
        vprint (" * NOTE: Cannot join 'event' table")

    dbnetmag = db.lookup(table='netmag')

    if dbnetmag.query('dbTABLE_PRESENT'):
        vprint ("  - Join netmag table")
        dbj = dbj.join('netmag', outer=True)
    else:
        vprint (" * NOTE: Cannot join 'netmag' table")

    my_expr = pf['config']['origins']['subsets']

    if my_expr:
        for mex in my_expr:
            if mex == '':
               continue
               
            dprint("mex is:", mex)
            dbj = dbj.subset('%s' % mex)
            vprint ("   - Origin subset '%s' resulted in %d records" % (mex, dbj.query('dbRECORD_COUNT')) )

    my_sort_fields = pf['config']['origins']['sort_fields']

    if dbj.query('dbRECORD_COUNT') < 1:
        print ("* ERROR: Database subsets failed to return any records. Check expr in config section of parameter file.")
        sys.exit(1)

    sortstring = []

    for msf in my_sort_fields:
        sortstring.append(msf)

    dbj = dbj.sort(sortstring)

    if dbj.query('dbRECORD_COUNT') < 1:
        print ("* ERROR: Database sorts failed. Check sort_fields in parameter file.")
        sys.exit(1)
    else:
        vprint ("   - Number of origins to plot: %s" % dbj.query('dbRECORD_COUNT'))

        for i in range(dbj.query('dbRECORD_COUNT')):
            ev_dict = {}
            dbj.record = i
            for f in fields:
                ev_dict[f] = dbj.getv(f)[0]
                if f == 'time':
                    # Convert to XML standard time
                    ev_dict['time_xml'] = stock.epoch2str(ev_dict[f], "%Y-%m-%dT%H:%M:%SZ")
                    ev_dict['time_readable'] = stock.epoch2str(ev_dict[f], "%Y-%m-%d %H:%M:%S UTC")

            #mag, mag_sc = calc_magtype(ev_dict)

            generate_origin_pts (opmtype_style,icon_method,ev_dict,pf_result)

    return "" 

# end get_origin_records

def generate_origin_pts (opmtype_style,icon_method,ev_dict,pf_result):
   """For a single origin from the database (collected within get_origin_records)

    Use the styling determined from choose_right_value 
    and add the placemark for the origin to the kml file, origins directory

    Argument icon_method should be magnitude, depth, or default

    Calls:  
       - choose_right_value - map origin depth or magnitude to appropriate depthcolor or magdepth
       - calc_magtype - maps magnitude type and value (null magnitude replaced with '-'
   """

   # cleanup mag and magtype as needed
   mag, mag_sc = calc_magtype(ev_dict)

   dprint("\nwithin generate_origin_pts: opmtype_style is: ", opmtype_style)

   if icon_method == 'default':
      mystyle_id = choose_right_value(dict_str2ints(opmtype_style), -9999)
      dprint("after choose_right_value for icon_method=default, mystyle_id is: ", mystyle_id)
      myscaleval = mystyle_id['scale']
      mynamed_style = mystyle_id['namedstyle']
   

   elif icon_method == 'depth':
      dprint('Trying both magnitude sizing and depth colorization for event with magnitude:', ev_dict['magnitude'], 'and depth:', ev_dict['depth'] )
      mystyle_id = choose_right_value(dict_str2ints(opmtype_style),  ev_dict['depth'])
      dprint("after choose_right_value for icon_method=depth, mystyle_id is: ", mystyle_id)

      if isinstance(mystyle_id['namedstyle'], dict):
         mynamed_style = choose_right_value(dict_str2ints(mystyle_id['namedstyle']), ev_dict['magnitude'])
      else: 
         mynamed_style = mystyle_id['namedstyle']

      dprint("mystyle_id for this point with depth of", ev_dict['depth'], "is: ", mystyle_id)
      
   elif icon_method == 'magnitude':
      dprint('Trying magnitude sizing for event with magnitude: %s %s' % (ev_dict['magnitude'],ev_dict['magtype'] ))

      if mag == "-":
         mystyle_id = choose_right_value(dict_str2ints(opmtype_style),  0)
      else: 
         mystyle_id = choose_right_value(dict_str2ints(opmtype_style),  mag)

      dprint("after choose_right_value for icon_method=magnitude, mystyle_id is: ", mystyle_id)

      if isinstance(mystyle_id['scale'], dict):
         scaledict = dict_str2ints(mystyle_id['scale'])
         dprint("styling name is: %s \n\tscaledict is: %s" %(mystyle_id['stylename'], scaledict))

         if mag == '-':
            # hard code choice that it magnitude is null/-99.99, choose the first value for magnitude scaling 
            mystyle_id = choose_right_value(dict_str2ints(opmtype_style), 0)
            myscaleval = choose_right_value(scaledict, 0) 
            dprint("null magnitude found, using a scale value of %s" % myscaleval) 
         else:  
            mystyle_id = choose_right_value(dict_str2ints(opmtype_style), mag)
            myscaleval = choose_right_value(scaledict, float(mag))
            dprint("setting scalefactor to %s for point with magnitude %s" % (myscaleval, mag) ) 

      else:
         myscaleval = mystyle_id['scale']

      # HERE - this may need a check for type dict for mystyle_id['namedstyle']
      #if isinstance(mystyle_id['namedstyle'], dict):
      #   print("DEBUG: namedstyle is a dict")
      #   mynamed_style = mystyle_id['namedstyle']
      #else:
      #   print("DEBUG: namedstyle is not a dict")
      #   mynamed_style = mystyle_id['namedstyle']

      mynamed_style = mystyle_id['namedstyle']

      dprint("\n\t  myscaleval is:", myscaleval, "for event at depth %s and magnitude %s" % (ev_dict['depth'], mag))

   else:
      print(' * ERROR * Should never get here, something is very broken' ) 
      sys.exit()
 
   # now generate placemark for kml 
   pnt = ofol.newpoint(coords=[(ev_dict['lon'],ev_dict['lat'])])

   pnt.style = mynamed_style 

   if stock.yesno(pf_result['config']['origins']['indicate_when']):
      pnt.timestamp.when =  ev_dict['time_xml']

   pnt.description = otext(ev_dict['lat'],ev_dict['lon'],ev_dict['depth'],ev_dict['time_readable'],mag,mag_sc,ev_dict['auth'],ev_dict['review'])

   dprint("pnt.description is: ", pnt.description)
   dprint("pnt.style is: %s \nfor %s " % (pnt.style, pnt.id)  )

   dprint("scaleval is:", pnt.style.iconstyle.scale, "for event at depth %s and magnitude %s" % (ev_dict['depth'], mag))


def get_site_records(dbmaster, pf_result):
    """Get all the site info from the dbmaster 

    Subset(s) from pf file are applied

    Returns: a dictionary of dictionaries, allsite
       - allsite keys are snet, values are dictionary of dictionaries
         with key of 'sta' and values that are 'field1:val1", etc.' 
    """

    staexpr = pf_result['config']['stations']['subsets']
    use_deployment = pf_result['config']['stations']['use_deployment']
    inactive = pf_result['config']['stations']['display_inactive']
    my_sort_fields = pf_result['config']['stations']['sort_fields']

    try:
        db = datascope.dbopen(dbmaster, 'r')
    except:
        sys.exit( "* Cannot open database '%s'" % dbmaster )

    if stock.yesno(use_deployment):
       dbm = db.lookup(table='deployment')
       dbm = dbm.join('site', outer=True)
       fields = ['snet', 'sta', 'lat', 'lon', 'elev', 'staname', 'time', 'endtime'] 

       if not stock.yesno(inactive):
          # double negative (not displaying inactive stations)
          vprint ("  - Subsetting database '%s' for active stations based on deployment.endtime" % dbmaster)
          dbm = dbm.subset('endtime == NULL || endtime > %s' % stock.now() )
          vprint ("   - dbsubset for active stations resulted in %d records" % (dbm.query('dbRECORD_COUNT')) )

    else:
       dbm = db.lookup(table='site')
       fields = ['snet', 'sta', 'lat', 'lon', 'elev', 'staname', 'ondate', 'offdate'] 

       if not stock.yesno(inactive):
          # double negative (not displaying inactive stationss)
          vprint ("  - Subsetting database '%s' for active stations based on site.offdate" % dbmaster)
          dbm = dbm.subset('offdate == NULL || offdate > %s' % stock.yearday(stock.now()) )
          vprint ("   - dbsubset for active stations resulted in %d records" % (dbm.query('dbRECORD_COUNT')) )

    dbm = dbm.join('snetsta', outer=True)

    try: 
       precision = pf_result['config']['stations']['precision']
       vprint("    - Truncating lat/lon for site to %s places" % precision)
    except:
       vprint("    - Using full lat/lon precision")
       dbq = dbm.lookup(field='lat', record='dbNULL')
       print("dbq is:", dbq)

       precision = dbq.query("dbFIELD_FORMAT")
       precision = str(precision).split('.')[1][0]
       dprint("precision defined from schema is:", precision)

    sortstring = []
    for msf in my_sort_fields:
        sortstring.append(msf)

    dbm = dbm.sort(sortstring)

    for nex in staexpr:
        if nex:
            vprint ("  - Subsetting database based on expression '%s'" % nex )
            dbm = dbm.subset('%s' % nex )
            vprint ("   - dbsubset '%s' resulted in %d records" % (nex, dbm.query('dbRECORD_COUNT')) )

    if dbm.query('dbRECORD_COUNT') < 1:
        if len(staexpr) > 0:
            print ("* ERROR: Dbmaster database (%s) generated view contains no records. Check your expressions." % dbmaster)
        else:
            print ("* ERROR: Dbmaster database (%s) generated view contains no records." % dbmaster)
        sys.exit(1)

    mynet = ''
    
    allsite = {}
 
    dprint("Starting loop over range of dbm records") 

    for i in range(dbm.query('dbRECORD_COUNT')):
        dbm.record = i

        for f in fields:  
           if f == 'snet':

              mynet = dbm.getv('snet')[0]

              try:
                 allsite[mynet]  
              except:
                 vprint ("\n  - Gathering site info for network '%s'" % mynet )
                 allsite[mynet]  = {}
    
           elif f == 'sta':

              mysta = dbm.getv('sta')[0]
              try:
                 allsite[mynet][mysta]  
              except:
                 allsite[mynet][mysta] = {}
                 vprint ("   - Collecting site info for station '%s'" % mysta )

           elif f == 'elev':
              allsite[mynet][mysta]['elev']  = dbm.getv('elev')[0] * 1000

           elif f == 'time':
              time = parseepoch( dbm.getv('time')[0] )
              # this gets rid of stations prior to 1970
              #if time < 0: continue
              allsite[mynet][mysta]['time']  = time 
              allsite[mynet][mysta]['time_xml']  = stock.epoch2str(time, "%Y-%m-%dT%H:%M:%SZ")
              allsite[mynet][mysta]['time_readable']  = stock.epoch2str(time, "%m/%d/%Y (%j)")

           elif f == 'endtime':
              endtime = parseepoch( dbm.getv('endtime')[0] )

              if not int(endtime) or stock.now() < endtime:
                 endtime = stock.now()

              allsite[mynet][mysta]['endtime']  = endtime 
              allsite[mynet][mysta]['endtime_xml']  = stock.epoch2str(endtime, "%Y-%m-%dT%H:%M:%SZ")
              allsite[mynet][mysta]['endtime_readable']  = stock.epoch2str(endtime, "%m/%d/%Y (%j)")

           elif f == 'ondate':
              time = stock.epoch( dbm.getv('ondate')[0] )
              # from original code: 
              # this gets rid of stations prior to 1970, which is not what we want 
              #if time < 0: continue
              allsite[mynet][mysta]['time']  = time 
              allsite[mynet][mysta]['time_xml']  = stock.epoch2str(time, "%Y-%m-%dT%H:%M:%SZ")
              allsite[mynet][mysta]['time_readable']  = stock.epoch2str(time, "%m/%d/%Y (%j)")

           elif f == 'offdate':
              offdate = dbm.getv('offdate')[0]

              if (offdate == -1) or (stock.now() < stock.epoch(offdate)) :
                 endtime = stock.now()
              else: 
                 endtime = stock.epoch(offdate)

              allsite[mynet][mysta]['endtime']  = endtime 
              allsite[mynet][mysta]['endtime_xml']  = stock.epoch2str(endtime, "%Y-%m-%dT%H:%M:%SZ")
              allsite[mynet][mysta]['endtime_readable']  = stock.epoch2str(endtime, "%m/%d/%Y (%j)")

           elif f == 'lat' or f == 'lon':		
              allsite[mynet][mysta][f] = round(dbm.getv(f)[0],int(precision))
              dprint("Precision adjusted lat/lon:", allsite[mynet][mysta][f])


           else:		# anything else 
              allsite[mynet][mysta][f] = dbm.getv(f)[0]
    
    dprint("Time: %s Endtime: %s" % (allsite[mynet][mysta]['time_readable'], allsite[mynet][mysta]['endtime_readable'] ))

    dprint("allsite is:\n", allsite)
    return(allsite)

# end get_site_records

def generate_site_pts(allsite,station_style,sfol):
   """Generate site placemarks For each station collected within get_site_records 

    Create a snet based folder for viewing in Google Earth list view
    and add a placemark for the site to the kml file, Stations/snet directory
    or to Stations/others

   """

   sfnames = {}

   dprint("allsite.keys is: ", allsite.keys())
   vprint("\n Gather site placemarks into named folders based on snet")

   for key in allsite.keys():
      # create a network named folder in sfol if key exists in station_style
      # otherwise, create "others" folder if it does not exist

      # had previously used by_network[net]['stylename']
      # but I decided calling folders the same as the by_network key was sufficient 
    
      dprint("station_style.keys is: ", station_style.keys())
      if key in station_style.keys():
         # if true, this means no folder for this station group exists
         # but we do have style info (i.e. it does not have to plot into 'others')

         if key not in sfnames.keys():		
            # populate the sfnames dict
            sfnames[key] = key
            netfol = sfol.newfolder(name=key,open=0)
            vprint("  - Created station snet folder named", key)

      else:
         if 'others' not in sfnames.values():
            netfol = sfol.newfolder(name='others',open=0)
            othersfol = netfol
            vprint("  - Created catch-all snet folder named 'others'")
         else: 
            # do something here to force folder to be used to be pre-existing 'others' folder
            dprint("folder named 'others' already exists")
            netfol = othersfol 

         sfnames[key] = 'others' 
 
      # now loop through each of the sta's in allsite for this network/key

      for s in allsite[key].keys():
         # now generate placemark for kml 
         lat = allsite[key][s]['lat']
         lon = allsite[key][s]['lon']
         pnt = netfol.newpoint(coords=[(lon,lat)],name=s)


         try: 
            pnt.style = station_style[key]['namedstyle']
         except:
            pnt.style = station_style['others']['namedstyle']
        
         # this is likely a problem since scale is now set in sharedstyle???
         
         try:
            pnt.iconstyle.scale = station_style[key]['scale']
         except:
            pnt.iconstyle.scale = station_style['others']['scale']

         if stock.yesno(pf_result['config']['stations']['indicate_when']):
            pnt.timestamp.when =  allsite[key][s]['time_xml']

         if stock.yesno(pf_result['config']['stations']['suppress_label']):
            pnt.style.labelstyle.scale = 0 

         pnt.description = stext(key,s,allsite)

   return ""
# end generate_site_pts

def href2kmz(kmzdir,myurl,altname):
    """Modify href if using kmz with saved images

    Checks if requested img is on local disc, web image, or api call
    Opens image and determines img format

    kmzdir is system's default tmp dir (see mk_kmzdir) 
    altname is the alternate name for file (needed especially for api collected images)
    
    Returns: href for use in icon styling
    """

    if not altname:
       altname = "dummy"

    if kmzdir: 

       # need to program a try/except for this to deal with non-http, i.e. file://
       try: 
          dprint("attempting api collected image %s" % myurl)
          img_file = requests.get(myurl, stream=True).raw 
       except:
          # does myurl start with "file://"
          if(re.match("file://", myurl)):
             dprint ("%s uses 'file://' convention" % myurl) 
             # strip off leading "file://"
             img_file =  myurl.removeprefix("file://")
          else:
             img_file = myurl

       dprint("img_file is: ", img_file)
       
       try:
          img = Image.open(img_file)
       except:
          sys.exit("ERROR:  Failed to open image file from %s" % myurl) 
          
       ext = img.format

       dprint("image format is:", ext)
       dprint("before any rename, filename is:", Path(str(myurl)).name)

       # Need to regex the filename to see if it has "?" or "&" or "#" or "%23" or "%26", 
       #        if so, it's an api call

       regex = '[@#&%]' 

       # Compile the regex
       r = re.compile(regex)

       if r.search(Path(str(myurl)).name): 
          myfilename = altname + '.' +  ext
          dprint("Found an api file match.  Will set filename to %s" % myfilename)
       else:
          myfilename = Path(str(myurl)).name
 
       dprint("filename to be saved is:", myfilename)

       kmlhref = kmzdir + '/' + myfilename

       dprint("kmlhref inside href2kmz is:", kmlhref)
 
       #img = img.save(kmlhref)

       if os.path.isfile(kmlhref):
          # file exists
          dprint("image file %s already exists" % kmlhref)
       else: 
          img = img.save(kmlhref)
          vprint("      - Adding %s to kmz image directory" % myfilename)

    else: 
       kmlhref = myurl

    return kmlhref

def plot_overlays(overlays,kmzdir,args):
    """plot overlay images such as a logo 

    if overlay has plotthis set to 'no' in pf, skip plotting 

    if args.testoverlay, interrupt full kml/kmz generation 
       to produce test file that has no site or origin placemarks
    """

    # only open a overlays folder if there are overlays to plot

    ovfol = ''
    ocnt = 1

    vprint ("\n - Add overlay images")

    for o in overlays: 

       try:
          if stock.yesno(overlays[o]['plotthis']):
             dprint("Plotting overlay image for ", o)
             vprint("  - Per plotthis in pf file, plotting overlay:", o)
             if not ovfol:
                ovfol = kml.newfolder(name='overlays',open=0)
          else:
             vprint("  -> Per plotthis in pf file, skipping overlay:", o)
             dprint("Skipping plotting of overlay image for", o)
             continue
       except:
          dprint("no plotthis parameter for", o, "...continuing")

       vprint("   - Placing overlay:", o)
       oxy = overlays[o]['overlayXY']
       sxy = overlays[o]['screenXY']
       sz  = overlays[o]['size']

       oxu = 'simplekml.Units.' + oxy['xunits']
       oyu = 'simplekml.Units.' + oxy['yunits']

       sxu = 'simplekml.Units.' + sxy['xunits']
       syu = 'simplekml.Units.' + sxy['yunits']

       szxu = 'simplekml.Units.' + sz['xunits']
       szyu = 'simplekml.Units.' + sz['yunits']

       screen = ovfol.newscreenoverlay(name=overlays[o]['name'])

       # I have tried a million times and cannot get the icon to show up in list view
       #screen.style.liststyle.itemicon.href = screen.icon.href
    
       if kmzdir:
          # fix the href for img_file
          screen.icon.href = href2kmz(kmzdir,overlays[o]['imgsrc'],'ov' + str(ocnt))

          # BUG  - I cannot get the list icon to show up
          #screen.style.liststyle.itemicon.href = screen.icon.href
          # increment cnt in case of api file nameing needs
          ocnt += 1
 
       else:
          screen.icon.href = str(overlays[o]['imgsrc'])


       screen.overlayxy = simplekml.OverlayXY(x=oxy['x'],y=oxy['y'],xunits=oxu,yunits=oyu)
       screen.screenxy  = simplekml.ScreenXY(x=sxy['x'],y=sxy['y'],xunits=sxu,yunits=syu)


       screen.rotation = overlays[o]['rotation'] 

       # a way to scale the overlay image
       screen.size.x = sz['x']
       screen.size.y = sz['y']
       screen.size.xunits  = szxu
       screen.size.yunits  = szyu

       # still can't get image to show up in list view
       #screen.liststyle.itemicon.href = screen.icon.href
       #screen.style.liststyle.itemicon.href = screen.icon.href
       #screen.iconstyle.icon.href = screen.icon.href

    if args.testoverlay:
       print("\nTEST MODE: saving to %s to check overlay placement(s)" % args.testoverlay)
       try:
          kml.savekmz(args.testoverlay)

          for o in overlays: 
             oxy = overlays[o]['overlayXY']
             sxy = overlays[o]['screenXY']
             sz  = overlays[o]['size']
             vprint("Paramters for overlay %s:" % o)
             vprint(" - overlayXY: %s %s %s %s" % (oxy['x'], oxy['xunits'], oxy['y'], oxy['yunits']))
             vprint(" - screenXY: %s %s %s %s" % (sxy['x'], sxy['xunits'], sxy['y'], sxy['yunits']))
             vprint(" - size: %s %s %s %s" % (sz['x'], sz['xunits'], sz['y'], sz['yunits']))

       except:
          print("Some exception occurred when saving kmz", args.testoverlay)

       print("Review overlay settings in Google Earth by opening", args.testoverlay)
       sys.exit()

    return() 


def add_camera(pf_result,obj):
   """ Add a starting visibility for the kml 
 
       Allows Google Earth to zoom automatically to given location       
   """ 

   vprint(" - Adding camera info to object: '%s'" % obj.name)
   cam  = pf_result['headers']['camera']

   obj.camera.latitude  = cam['latitude']
   obj.camera.longitude = cam['longitude']
   obj.camera.altitude  = cam['altitude']
   obj.camera.tilt      = cam['tilt']

# end add_camera      

def generate_lookat(pf_result,plottype,folder):
   """ Add LookAt information for an object (normally a folder containing placemarks)
 
       Allows Google Earth to zoom automatically to a given location       
   """ 

   la = pf_result['styles'][plottype]['look_at']
   vprint ("    - Adding lookat to object:", folder.name)

   folder.lookat.latitude  = la['latitude']
   folder.lookat.longitude = la['longitude']
   folder.lookat.range     = la['range']

# end generate_lookat

def map_sta_stylename (mydict, pfstyle ):
   """ because simplekml does not let you set the style.id, here is a dict to map 
       the random number given to the style to something useful
       along with other parameters needed.  Used for sites.  See map_useful_stylename for origins.

       Called by kml_styles

       Calls href2kmz

       Returns: my_style
   """

   my_style = {}

   for key in sorted(mydict.keys()):
      my_style[key] = {}

      stastat_fields = ['scale', 'imagesrc']
    
      for ss in stastat_fields: 
          my_style[key][ss] = mydict[key][ss]
    

      my_style[key]['namedstyle'] = 'style{0}'.format(key)

      dprint("namedstyle within map_sta_stylename is:", my_style[key]['namedstyle'])

      # try to get href with kmz
      fullhref = my_style[key]['imagesrc'] 

      my_style[key]['style_href'] = href2kmz(kmzdir,fullhref,my_style[key]['namedstyle'])

      dprint("my_style for %s is: %s" % (key,my_style))

      try:
         style_scale = mydict[key]['iconscale']
      except:
         style_scale = mydict[key]['scale']

      my_style[key]['scale'] = style_scale 

      dprint("style_href for ",key, "is:", my_style[key]['style_href'])

   return(my_style)

# end map_sta_stylename

def map_useful_stylename (mydict, pfstyle, icon_method):
   """ because simplekml does not let you set the style.id, here is a dict to map 
       the random number given to the style to something useful
       along with other parameters needed: style_href, scalefactor , scale

       Used for origins.  See map_sta_stylename for sites

       Called in kml_styles

       Calls href2kmz, get_sf_magscale
   """

   my_style = {}
 
   
   try:
      my_magscale = pfstyle['magscale']
      dprint("origin magnitude styling is: \n", my_magscale)
   except:
      sys.exit("Can't parse magscale from pf->styles->origin")

   for key in mydict.keys():
      my_style[key] = {}

      # i.e. -2dep-style, 3mag-style, etc.
      ustyle_name  = str(key) + str(icon_method)[:3] 
      dprint("pretty style_name is:", ustyle_name)
      dprint("my_style is:", my_style)

      my_style[key]['stylename']  = ustyle_name 
      my_style[key]['namedstyle'] = ustyle_name + '-style' 

      dprint("namedstyle is:", my_style[key]['namedstyle'])

      # fullhref build varies betwen stations (possibly non-api)  and origins (only api??)
      # for depth/mag pfstyle = origin_styles


      if icon_method : 	# for stations icon_method will be ""
         
         try: 
            mydict[key]['api_params']
            fullhref = mydict[key]['imgsrc'] + mydict[key]['api_params'] 
         except:
            fullhref = mydict[key]['imgsrc'] 

      else:
         print("Should never get here as map_useful_stylename is only for origins")
         sys.exit()
 
      dprint("complicated href:", fullhref)

      my_style[key]['style_href'] = href2kmz(kmzdir,fullhref,my_style[key]['namedstyle'])

      dprint("style_href before I set sharedstyle  is:", my_style[key]['style_href'])

      # need to put scale info into my_style
      sf = mydict[key]['scalefactor'] 

      # convert sf if scalefactor is 'magscale'
      # if sf is not 'magscale', get_sf_magscale will return 
      # either '1' or given scale from pfstyle

      dprint("Before get_sf_magscale shenanigans, sf is:", sf)

      sf = get_sf_magscale(sf,icon_method,pfstyle,key)
   
      dprint("After get_sf_magscale shenanigans, sf is:", sf)

      # indent may be incorrect here and have to go <<<
      my_style[key]['scale'] = sf 

   return(my_style)

# end map_useful_stylename

def get_sf_magscale (sf,icon_method,pfstyle,val):
   """ converts input sf (scalefactor) from dict (magscale) to numeric sf 
 
   Calls get_magscale(pfstyle,val)

   Called by map_useful_stylename

   Returns: newly mapped sf
   """

   if sf == 'magscale':
      dprint("scalefactor depends on magnitude map in magsacle for %s" % val )

      # if icon_method is magnitude, then pull out mapped scale factor 
      # if icon_method is depth, you have to create new style for each magnitude

      if icon_method == 'magnitude': 
         # get mapped magnitude scale
         dprint("Trying get_magscale")
         sf = get_magscale(pfstyle,val)
      else: 
         # insert magscale to mydict (append/map dictionary to key 'scale')
         sf = pfstyle['magscale']  

   else: 
      try:
         sf = float(sf)
      except ValueError:
         print("ERROR: scalefactor %s for %s is neither a float or 'magscale'" % (sf , val) )
         print("       - setting scalefactor to '1.0' ") 
         sf = 1 

   dprint("sf within get_sf_magscale is now:", sf)

   return(sf)

def make_sharedstyle(mystyle,icon_method):
   """ make a shared style using simplekml 

   Note: only shared styles which are used by a placemark show up in output kml/kmz file
   """

   # icon_method can be 'default', 'magnitude', or 'depth'
   # key is either a depth or a magnitude deliniation (compare later against actual values from db)

   dprint ("icon_method within make_sharedstyle is:", icon_method)

   for key in mystyle.keys():

      if isinstance(mystyle[key]['scale'], dict):
 
         mystyle[key]['namedstyle'] = {}

         for sckey in mystyle[key]['scale']:
            # this creates a style per each scalefactor, not the most efficient thing,
            # but I cannot scale each individual point when using a shared style.  Sigh.
            # On the plus side, the saved kmz will only contain the sharedstyles 
            # that are "used" by the origins

            dprint("Working on scaling %s for %s %s..." % (sckey, icon_method, key) )

            sharedstyle  = simplekml.Style()
            dprint("\n\tStyle id for varied scale value %s with key %s for type %s is: %s " % ( sckey, key, icon_method, sharedstyle.id ) )

            sharedstyle.iconstyle.scale = mystyle[key]['scale'][sckey] 

            sharedstyle.liststyle.itemicon.href = mystyle[key]['style_href'] 
            sharedstyle.iconstyle.icon.href     = mystyle[key]['style_href']
            sharedstyle.balloonstyle.text = "$[description]"
            sharedstyle.balloonstyle.bgColor = "ffffffff"
            sharedstyle.balloonstyle.Color = "ffffffff"
  
            ## original code had customizable scale for LableStyle... ignoring
            ## See https://developers.google.com/kml/documentation/extendeddata
            ## for discussion of using extended data for balloon filling text

            # modify namedstyle to 
            mystyle[key]['namedstyle'][sckey] = sharedstyle
  
            dprint("namedstyle for %s-%s is now:\n  %s" % (key, sckey, sharedstyle))
            dprint( "sharedstyle after balloonstyle setting:", sharedstyle)

      else: 
    
         dprint("namedstyle when scale is a value for key %s is:\n %s" % ( key, mystyle[key]['namedstyle']) )
         sharedstyle  = simplekml.Style()

         dprint("\n\t Style id is: ", sharedstyle.id, "\n" )

         # is there any case where mystyle[key]['scale'] is not defined??? Might need to check for that and set scale to 1?

         sharedstyle.iconstyle.scale = mystyle[key]['scale'] 
         sharedstyle.liststyle.itemicon.href = mystyle[key]['style_href'] 
         sharedstyle.iconstyle.icon.href     = mystyle[key]['style_href']
         sharedstyle.balloonstyle.text = "$[description]"
         sharedstyle.balloonstyle.bgColor = "ffffffff"
         sharedstyle.balloonstyle.Color = "ffffffff"

         mystyle[key]['namedstyle'] = sharedstyle 

         dprint( "sharedstyle after balloonstyle setting:", sharedstyle)



      dprint("useful style name to sharedstyle map:", mystyle[key]['namedstyle'])

      # mystyle is now a dictionary that maps the read-only style.id
      # to the "what I would call the style if I could set the id" style_name

      dprint("\nDictionary with mapped style info:\n", mystyle,"\n")

   dprint("sharedstyle within make_sharedstyle, outside for loop:", sharedstyle)

   return sharedstyle

# end make_sharedstyle

def parse_placemark_model(origin_styles,icon_method,opmmodel):
   """interpret placemark model for origins
      select color from depthcolor if placemark model uses dynamic coloring
      select magnitude scaling if model uses dynamic scaling
      
      Currently only 'depth', 'magnitude', or 'default'  are accepted as icon_method 

      Calls: build_api_href 

      Returns: mymodel

   """

   vprint ("     - Generating icon styles based on '%s'" % icon_method)

   mymodel = {}
 
   dprint("icon_method is:", icon_method, "opmmodel is:", opmmodel)

   if icon_method == 'depth' or icon_method == 'magnitude':
      myfields = ['depth', 'scalefactor',  'imgsrc', 'api_params']
   elif icon_method == 'magnitude':
      myfields = ['magnitude', 'scalefactor',  'imgsrc', 'api_params']
   elif icon_method == 'default':
      defpm = origin_styles['placemarks']['default']
      myfields = ['magnitude', 'scalefactor',  'imgsrc']
      myvalues = [ '-9999', defpm['scale'], defpm['imgsrc']  ]
      mymodel['-9999']  = dict(zip(myfields,myvalues))
   else:
      vprint("programming for interpretting a styling based on '%s' has not been done" % icon_method)
      sys.exit()

   # take named model, and pull in values for it
   if opmmodel:
      opmmodel = origin_styles['placemarks'][icon_method][opmmodel]

   for l in opmmodel: 
      if l.strip() == "":
         continue

      myvalues = str(l).split()
      myval   = str(l).split()[0]

      try: 
         myapis  = str(l).split()[3]	
      except: 
         myapis  = ""

      if(re.match("\?", myapis)):
         dprint("Chosen api values are %s" % myapis)

      elif myapis:
         if origin_styles['api_params'][myapis] :

            # if any of the values are 'depthcolor' or 'magscale', interpret 
            # this is now done later, for instance in generate_origin_pts
            #if 'depthcolor' in list(origin_styles['api_params'][myapis].values()):
               #   print("One of the params is depthcolor")
            #if 'magscale' in list(origin_styles['api_params'][myapis].values()):
               #   print("One of the params is magscale")

            api2add = build_api_href(origin_styles['api_params'][myapis],origin_styles,myval)
            dprint("api2add is:", api2add)

            myvalues[3] = api2add
                     
            mymodel[myval]  = dict(zip(myfields,myvalues))

         else:
            print("ERROR:  Can't find ", myapis, "in origin_styles['api_params'][myapis]")
            sys.exit()
      else:
         dprint("There are no api modifications")

      mymodel[myval]  = dict(zip(myfields,myvalues))

   return mymodel


def kml_styles(pf_result,args):
    """Create KML styles

       Calls: generate_lookat, parse_placemark_model, map_useful_stylename, make_sharedstyle, map_sta_stylename

       Returns: opmtype_style, station_style, ofol, sfol, icon_method, opmmodel, sharedstyle 

    """

    #vprint ("- Write out styles")
    #vprint ("- Generating icon styles")

    #should create a separate folder for station styles vs origin styles


    ofol = "" 
    origin_styles = pf_result['styles']['origins']
    opmtype_style = {}
    opmmodel = ""
    icon_method = ""

    sfol = "" 
    station_style   = {}
    sta_styles    = pf_result['styles']['stations']

    dprint("origin_styles = ", origin_styles)
    dprint("sta_styles = ", sta_styles)

    dprint("What are you attempting to plot?", args.c)

    if args.c == 'origins' or args.c == 'all': 

        vprint ("\n  - Generating icon styles for origins")
        ofol = kml.newfolder(name="origins",open=0)

        if origin_styles['look_at']:
            # add look_at info to folder
            generate_lookat(pf_result,'origins',ofol)


        if args.opm :
           dprint("an origin placemark option was chosen - ", args.opm)
           if args.opm == 'default':
              dprint("using default origin placemark plotting")
              icon_method = 'default'
              opmmodel = ''
           else: 
              try:
                 icon_method,opmmodel = str(args.opm).split(':') 
              except:
                 msg = "Cannot parse origin placemark model choice via -opm "
                 msg = msg +  args.opm + "\nVerify you split field:model with a ':'" 
                 sys.exit(msg)
        else:
           dprint("using default origin placemark plotting")
           icon_method = 'default'
           opmmodel = ''

        dprint("icon_method is:", icon_method)
        dprint("opmmodel is:", opmmodel)

        # check to see if icon_model is 'magnitude', 'depth', or ''
        allowed_im = ['magnitude', 'depth', 'default', '']

        if icon_method not in allowed_im:
           msg = "ERROR: Chosen origin placemark sorting field via -opm is not found: " 
           msg = msg + icon_method + "\nValid values are one of: "
           msg = msg +  ' or '.join(allowed_im) 
           sys.exit(msg)
                   
 
        # check to see if opmmodel exists, retrieve it

        if opmmodel and opmmodel in origin_styles['placemarks'][icon_method].keys():
           dprint("Interpretting placemark model", opmmodel)
        elif not opmmodel :
           dprint("using default rather than an opmmodel")

        else:
           print("Cannot find your chosen model -", opmmodel ,"- in styles{origins}{placemarks}{", icon_method, "} section of pf")
           sys.exit()
         
        mymodel = parse_placemark_model(origin_styles,icon_method,opmmodel)

        opmtype_style = map_useful_stylename (mymodel, origin_styles, icon_method)

        dprint("opmtype_style:\n", opmtype_style)

        # below makes numerous styles based on keys of opmtype_style
        # icon_method can be 'default', 'magnitude', or 'depth'

        sharedstyle = make_sharedstyle(opmtype_style,icon_method)


    if args.c == 'stations' or args.c == 'all': 
       vprint ("\n  - Generating icon styles for stations")
       sfol = kml.newfolder(name="stations",open=1)

       if pf_result['styles']['stations']['look_at']:
          # add look_at info to folder
          generate_lookat(pf_result,'stations',sfol)

       my_sta_styles = pf_result['styles']['stations']['by_network']

       # insert check here to verify that a default catchall of 'others' is defined in my_sta_styles

       if 'others' not in my_sta_styles.keys():
          sys.exit("ERROR: 'others' named array is undefined.  Check styles:stations:by_network\nCheck your pf file")

       #dprint("my_sta_styles is:", my_sta_styles)

       station_style = map_sta_stylename (my_sta_styles, pf_result['styles']['stations'])
       sharedstyle = make_sharedstyle(station_style,'station')

    # this dprint is pointless as shared styles don't show up in kml.kml() unless they have an object/placemark
    #dprint("Within, but at end of kml_styles:", kml.kml())

    return (opmtype_style, station_style, ofol, sfol, icon_method, opmmodel, sharedstyle) 

# end kml_styles

def build_api_href(apips,origin_styles,myval):
   """Interpret api parameters for origin styles

   If one of arrays in api_params has a param with regex = 'color', then value
   is validated via validate_hexcolor

   Calls:  
      - validate_hexcolor 
      - choose_right_value 
   
   Returns: apistring
   """

   apistring = '?'  
   apk = 0 
   for apk in apips.keys():
      apv = apips[apk]
      dprint("apk: %s  apv: %s" % (apk, apv))

      # logic hardcode... 
      # if key has regex match to "color", then pass through validate_hexcolor

      if(re.search('color', apk)):
         dprint("Found an api parameter that is likely a color")
         dprint("Going to run validate_hexcolor")

         if(re.search('depthcolor', apv)):
            dprint("Found an api parameter that is set to depthcolor")

            depthcolor = dict_str2ints(origin_styles['depthcolor'])
            mycolorname  = choose_right_value(depthcolor, float(myval))
            
            apv = validate_hexcolor(mycolorname)
            dprint("  Param: %s Depth: %s  Color: %s" % (apk, myval, apv))


      # This may be a problem/bug since I have not programmed anything
      if(re.search('magscale', apv)):
         print("Found an api parameter that is a magscale ")
         print("ERROR: have not programmed for this eventuallity")
         sys.exit()
         # Do somehting??
         # Can/cannot that the key = size?  
         # compare with default[size] and do math?
         

      if apistring == '?':
         # do not add "%26" (which is &) if it is the first parameter
         apistring = apistring + apk + '=' + apv
      else: 
         # add "%26" (which is &) if it is *not* the first parameter
         apistring = apistring + '%26' + apk + '=' + apv

   return(apistring)

def dict_str2ints (mydict):
   """need to convert keys of dictionary to 
      ints from str in order to get properly ordered
   """

   mydict = {int(k):v for k,v in mydict.items()}
   mydict = dict(sorted(mydict.items()))

   return mydict

def otext(lat,lon,depth,time,mag,magtype,auth,review):
   """ Gather text for putting in description balloon for origins
   """

   text = ''

   if review == '-':
      review = "no"
   elif review == 'y':
      review == "yes"

   startcdata = "<![CDATA[\n"
   endcdata = "]]>\n"

   tlat = "<p><strong>Latitude:</strong> %s</p>\n" % lat
   tlon = "<p><strong>Longitude:</strong> %s</p>\n" % lon
   tdepth = "<p><strong>Depth(km):</strong> %s</p>\n" % depth
   ttime = "<p><strong>Time:</strong> %s</p>\n" % time
   tmagnitude = "<p><strong>Magnitude:</strong> %s%s </p>\n" % (mag, magtype)
   tauth = "<p><strong>Author:</strong> %s</p>\n" % auth
   trev = "<p><strong>Reviewed?</strong> %s</p>\n" % review

   text = startcdata + tlat + tlon + tdepth + ttime + tmagnitude + tauth + trev + endcdata

   return(text)


def stext(snet, sta, allsite ):
   """ Gather text for putting in description balloon for site   
   """
 
   text = ''

   startcdata = "<![CDATA[\n"
   endcdata = "]]>\n"

   tsnetsta = "<p><strong>%s_%s</strong> </p>\n" % (snet, sta)
   hr = "<hr>\n"
   tstaname = "<p><strong>Name:</strong> %s</p>\n" % allsite[snet][sta]['staname']
   tlat = "<p><strong>Latitude:</strong> %s</p>\n" % allsite[snet][sta]['lat']
   tlon = "<p><strong>Longitude:</strong> %s</p>\n" % allsite[snet][sta]['lon']
   telev  = "<p><strong>Elev(m):</strong> %s</p>\n" % allsite[snet][sta]['elev']
   ton    = "<p><strong>Ondate:</strong> %s</p>\n" % allsite[snet][sta]['time_readable']
   toff   = "<p><strong>Offdate:</strong> %s</p>\n" % allsite[snet][sta]['endtime_readable']

   text = startcdata + tsnetsta + hr + tstaname + tlat + tlon + telev + ton + toff + endcdata

   return(text)

def validate_hexcolor(colorstr):
   """ validate color code, is it hex or no 

   See:  https://www.geeksforgeeks.org/how-to-validate-hexadecimal-color-code-using-regular-expression/

   Assumes that any non-hex string is a valid colorname - perilous!
 
   Returns: colorstr
   """

   # regex to check valid hexadecimal color code.
   # 3 or 6 characters using A-F and/or 0-9
   regex = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

   # Compile the ReGex
   r = re.compile(regex)

   # check colorstr if a '#' is prepended
   pstr = '#' + colorstr

   if(colorstr == None):
      # raise exception??
      return False

   # Return if the string matched the regex
   # most input strings will not have leading '#'

   if(re.search(r, colorstr)):
      # this assumes correcly formatted hex string (unlikely unless "#" is escaped in pf file) 
      return str  
   elif(re.search(r, pstr)):
      dprint("color", colorstr, "is a hex if I add a '#'")
      dprint("modifying", colorstr, "to have %23 prepended for api call" )
      pstr = '%23' + colorstr
      return pstr 
   else:
      # this assumes color string is something like "orange"
      return colorstr  

def mk_kmzdir(args):
    """ get path and keep/delete status of temporary 
        imagedir for kmz images

    Returns: kmzdir, cleanup
    """

    kprefix = 'kmzimgs'
    kmzdir = ''
    cleanup = ''

    if args.z:  
       vprint ("\n- KMZ generation requested")
       kmzdir = tempfile.mkdtemp(prefix=kprefix)  

       # can't use the auto-cleanup version TemporaryDirectory because of href2html appending of filename

       if args.k: 
          # tmpdir is left for manual cleanup after script exits
          vprint(" - per -k, tmp directory will not be rm'd:", kmzdir)
          cleanup = ''   
       else: 
          # tmpdir needs to be rm'd after script exits
          vprint(" - tmp directory will be cleaned up:", kmzdir)
          cleanup = True 

       dprint("Image directory for kmz generation: ", kmzdir)
       dprint("If you prefer a different tmp directory, change your TMPDIR environment variable")

    else:
       dprint ("\n- No KMZ generation requested")
       kmzdir = ''  

    return(kmzdir,cleanup)

def check_output_filename(out_file, args, pf_result):
    """check if out_file has .kml or .kmz and appends appropriate extension, if needed
       
       Returns: out_file 
    """

    vprint (" - Confirming output file name ")

    root, ext = os.path.splitext(out_file)    
    if not ext: 
       if args.z: 
          vprint("  - Appending '.kmz' to",  out_file )
          out_file = out_file + ".kmz"
       else:
          vprint("  - Saving kml file only" )
          vprint("  - Appending '.kml' to",  out_file )
          out_file = out_file + ".kml"
    elif ext != 'kml' or ext != 'kmz':             
       print("ERROR output file name", out_file, "must end with .kml, or .kmz if -z specified")
       sys.exit()
    elif ext == 'kml' and args.z:             
       print("ERROR output file name", out_file, "ends with .kml but -z specified")
       sys.exit()
    elif ext == 'kmz' and not args.z:             
       print("ERROR output file name", out_file, "ends with .kmz but -z not specified")
       sys.exit()
    else:
       vprint("  - ", out_file, "has acceptable extension")

    return out_file 


# main, taking it out of a def

if __name__ == '__main__':

    (args,parser) = parseArgs()
    database = args.db
    out_file = args.kmlfile

    # setup some verbose and debug printing
    # if debug, then also define vprint
    if args.debug:
       args.verbose = True 
       def dprint(*args):
          print("DEBUG:",*args),
    else:
       dprint = lambda *a: None      # do-nothing function

    if args.verbose:
       def vprint(*args):
          import inspect
          frame = inspect.stack()[1]
          print(*args), 
    else:
       vprint = lambda *a: None      # do-nothing function 

    if args.quiet:
       print  = lambda *a: None      # do-nothing function 

    vprint ("Start of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()))

    #dprint ("pf is", pf)

    if args.testoverlay:
       print("WARNING: Overlay test mode, -t, was specified.  Full kml/kmz will not be generated!")
       
    if args.k and not args.z:
       print("Warning: -z was not specified.  The -k option will be ignored.") 

    vprint ("\n- Read configuration parameter file (pf): %s" % args.pf)
    pf = stock.pfread(args.pf)

    vprint (" - Starting get_pf")

    pf_result  = get_pf(pf,args)

    dprint ("pf_result is", pf_result)

    headers  = pf_result['headers']
    overlays = pf_result['overlays']

    (kmzdir,cleanup)  = mk_kmzdir(args)

    out_file = check_output_filename(out_file,args,pf_result)
 
    imglist = []     # used for collecting hrefs for images, to be sorted later before kml.addfile

    vprint ("\n- Starting kml generation ")

    kml = simplekml.Kml(name=headers['name'],description=headers['description'],open=1)

    try:
       headers['camera']    
       add_camera(pf_result,kml.document)
    except:
       vprint (" -> Camera information not available, default zoom in effect" ) 


    try:
       headers['network_link']    
       if stock.yesno(headers['network_link']['ignore']):
          vprint(" -> Skipping network link, per ignore in pf")
       else: 
          vprint(" - Populating network link")
          nl = kml.newnetworklink(name=headers['network_link']['name'])
          nl.link.href = headers['network_link']['link']
          nl.link.author = "db2kml_py script" 
          # keep the user's Google Earth settings
          nl.link.refreshvisibility = 0 
          nl.link.viewrefreshmode  = simplekml.ViewRefreshMode.onrequest 

          kml.networklinkcontrol.refreshmode  = simplekml.RefreshMode.onexpire  
          kml.networklinkcontrol.expires = time.time() + float(headers['network_link']['expires'])
          # need to set "minRefreshPeriod" to make sure server access is not abused 
          # currently hardwired to 5 minutes
          kml.networklinkcontrol.minrefreshperiod = 300 
          kml.networklinkcontrol.refreshmode  = simplekml.RefreshMode.onexpire  
          kml.networklinkcontrol.expires = time.time() + float(headers['network_link']['expires'])
    except:
       vprint(" - Skipping network link")
  


    plot_overlays(overlays,kmzdir,args)

    dprint("After adding overlays:", kml.kml())

    vprint ("\n - Working on placemark styles")

    (opmtype_style, station_style, ofol, sfol, icon_method, opmmodel, sharedstyle) =  kml_styles(pf_result,args)

    dprint("After generating styles with kml_styles :", kml.kml())

    vprint ("\n - Collecting station info from database: %s" % database)

    # args.c defaults to 'all' if no args.c is chosen on command line, per argparse
 
    if args.c == 'origins':  
        # get_origin_records calls generate_origin_pts
        get_origin_records(database,pf_result,opmtype_style,ofol,sfol,sharedstyle,icon_method)
        dprint("After adding origin points to plot:", kml.kml())

    if args.c =='stations':  
        allsite = get_site_records( database, pf_result)
        generate_site_pts(allsite,station_style,sfol)

    if args.c == 'all':
        allsite = get_site_records( database, pf_result)
        generate_site_pts(allsite,station_style,sfol)

        get_origin_records(database,pf_result,opmtype_style,ofol,sfol,sharedstyle,icon_method)
        dprint("After adding origin points to plot:", kml.kml())

    if kmzdir:
       print("\n- Saving kmz file",  out_file)

       #the savekmz function of simplekml seems to be a bit dumb about duplicate hrefs
       # without the warn suppression below, you get frequent errors like:
       # zipfile.py:1506: UserWarning: Duplicate name: 'files/style0.PNG' return self._open_to_write(zinfo, force_zip64=force_zip64)
       # in an ideal world, the savekmz funciton would proceed like:
       #   "oh, same file, I'll only save one!"
       # instead savekmz adds that additional file to the kmz and then warns about it
       # I have not found a way around this so the kmz file does get bloated
       # and I have chosen to suppress the warnings about the duplicate files 

       warnings.simplefilter('ignore', UserWarning)
       kml.savekmz(out_file)

    else: 
       print("- Saving kml file", out_file)
       kml.save(out_file)

    # cleanup the imagedir for kmz image files unless -k 

    if args.k:
        vprint("Image files used for kmz can be found in %s" % kmzdir)
    else:
        if args.z:
           vprint("\n- Removing temporarily saved image files and directory %s" % kmzdir )
           # This removal is done automatically because TemporaryDirectory() is used
           try:
              shutil.rmtree(kmzdir)
           except:
              print("Error when attempting to cleanup %s" % kmzdir)

    vprint ("\nEnd of script at time %s" % time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()))


