'''
@author      Nikolaus Horn <Nikolaus.Horn@zamg.ac.at>
@created     2014-06-18
@modified    2014-06-18
@version     1.0
@license     MIT-style license
'''


# Import Antelope modules

import antelope.datascope as ds
import antelope.stock as stock
import getopt
import codecs
import urllib2
import json
import pprint

def usage():
    print sys.argv[0], "[-v] [-a auth] [-u url] dbname from to" 

def main():    
    BASE_URL="http://service.iris.edu/fdsnws/event/1/query?orderby=time&format=text&nodata=404"
    verbose=0
    archive=0
    opts = []
    args = []
    auth=''
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'a:u:v', '')
    except getopt.GetoptError:
        print "illegal option"
        usage()
        sys.exit(2)

    for o,a in opts:
        if o == '-v':
            verbose = 1
        elif o == '-a':	
            auth = a
        elif o == '-u':	
            BASE_URL = a
            
    if len(args) > 3 or len(args) < 3:
        usage()
        sys.exit(1)

    if len(args) == 3:
        dbname=args[0]
        t_from=str(args[1])
        t_to=str(args[2])

    t1 = stock.str2epoch(t_from)
    t2 = stock.str2epoch(t_to)
    if t2 < t1:
        t2=t1+t2
    ts1=stock.epoch2str(t1,'%Y-%m-%dT%H:%M:%S')    
    ts2=stock.epoch2str(t2,'%Y-%m-%dT%H:%M:%S')    
    if t1 >= t2:
        print "endtime MUST be AFTER starttime : %s >= %s" % (ts1,ts2)

    db= ds.dbopen( dbname, "r+" )
    dborigin=db.lookup(table='origin')
    dbevent=db.lookup(table='event')
    dbnetmag=db.lookup(table='netmag')

    dbq=db.lookup(table='origin',field='ml',record='dbNULL')
    [mlnull]=dbq.getv('ml')
    dbq=db.lookup(table='event',field='evname',record='dbNULL')
    evname_width=dbq.query('dbFIELD_SIZE')

    #proxies={'http':'http://138.22.156.44:3128'}
    MY_URL="%s&starttime=%s&endtime=%s" % (BASE_URL,ts1,ts2)
    url=urllib2.urlopen(MY_URL)
    data_string=url.read()
    for myline in data_string.splitlines():

        if myline[0]=='#':
            continue
    
        [evid,otimestr,lat,lon,depth,oauth,ocat,ocont,ocid,magtype,magnitude,magauth,regname]=myline.split('|')
        evid=int(evid)
        lat=float(lat)
        lon=float(lon)
        depth=float(depth)
        magnitude=float(magnitude)
        ts=otimestr.replace('T',' ')
        otime=stock.str2epoch(ts)
        magtype=str(magtype).lower()
        if verbose:
            print "evid %d lat %f lon %f :%s: %.1f" % (evid,lat,lon,magtype,magnitude)

        mb=ms=ml=mlnull    
        if magtype == 'ml':
            ml=magnitude
        if magtype == 'ms':
            ms=magnitude
        if magtype == 'mb':
            mb=magnitude
        # grn, srn seems to be unimplemenmted
        gr=stock.grnumber(lat,lon)    
        sr=stock.srnumber(lat,lon)    
        jdate=stock.epoch2str(otime,'%Y%j')
        if auth != '':
            oauth=auth
        orecno=dborigin.addv( ('time',otime),('lat',lat),('lon',lon),('depth',depth), 
                ('evid',evid),('orid',evid), ('jdate',jdate), 
                ('mb',mb),('ml',ml),('ms',ms), 
                ('nass',0),('ndef',0),('auth',oauth),('grn',gr),('srn',sr) ) 

        erecno=dbevent.addv(('evid',evid),('prefor',evid),('evname',regname[:evname_width]),('auth',oauth) )
        nmrecno=dbnetmag.addv(('evid',evid),('orid',evid),('magnitude',magnitude),('magtype',magtype),('auth',magauth) )
            
    return 0    

if __name__ == '__main__':
    status = main()
    sys.exit(status)
