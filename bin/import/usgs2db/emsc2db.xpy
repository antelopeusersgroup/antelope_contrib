'''
@author      Nikolaus Horn <Nikolaus.Horn@zamg.ac.at>
@created     2013-11-25
@modified    2011-11-17
@version     1.0
@license     MIT-style license
@credits     the ability to visit EGU this year!

'''


# Import Antelope modules

from antelope.datascope import *
from antelope.stock import *
import getopt
import codecs
import urllib2
import json
import pprint
import datetime

def usage():
    print sys.argv[0], "[-v] [-k keydb] [-p pfname] [-u url] [-i interval] dbname" 

def main():    
    BASE_URL="http://www.seismicportal.eu/fdsnws/event/1/query?limit=100&format=json"
    verbose=0
    archive=0
    opts = []
    args = []
    keydbname='keydb'
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'i:k:p:u:v', '')
    except getopt.GetoptError:
        print "illegal option"
        usage()
        sys.exit(2)

    for o,a in opts:
        if o == '-v':
            verbose = 1
        elif o == '-d':	
            ndays = int(a)
        elif o == '-p':	
            pf = a
        elif o == '-u':	
            BASE_URL = a
        elif o == '-k':	
            keydbname = a

    if len(args) > 1 or len(args) < 1:
        usage()
        sys.exit(1)

    if len(args) > 0:
        dbname=args[0]

    db= dbopen( dbname, "r+" )
    dborigin=db.lookup(table='origin')
    dbevent=db.lookup(table='event')
    dbnetmag=db.lookup(table='netmag')

    dbq=db.lookup(table='origin',field='ml',record='dbNULL')
    [mlnull]=dbgetv(dbq,'ml')
    dbq=dblookup(db,'','event','evname','dbNULL')
    evname_width=dbquery(dbq,dbFIELD_SIZE)

    kdb=dbopen(keydbname,'r+')
    idmatch=dblookup(kdb,'','idmatch','','')
    

    #proxies={'http':'http://138.22.156.44:3128'}
    url=urllib2.urlopen(BASE_URL)
    gj_string=url.read()
    obj=json.loads(gj_string)
    # These lines just iterate over that structure.
    #for ka, va in obj.iteritems():
    #    print ka
    data=obj['features']    
    i=len(data)    
    for index in range(i):
        fdata=data[index]
        #pprint.pprint(fdata)
#        for ka, va in fdata.iteritems():
    #        print ka
        geom_type=fdata['type']    
        geometry=fdata['geometry']    
        coordinates=geometry['coordinates']    
        lon=  float(coordinates[0])    
        lat=  float(coordinates[1])    
        depth=float(coordinates[2])    
        if depth < 0.0:
            depth=depth * -1.0
        
        properties=fdata['properties']    
        mb=ms=ml=mlnull    
        time=status=cdi=place=code=felt=mag=magtype=net=evtype=''    
        ml=mb=ms=mlnull
        # be sure to convert unicode objects to string objects by calling "str(xxx)", 
        # this prevents datascope  from CRASHING
        for propk, propv in properties.iteritems():
            if propk ==   'time':
                try:
                    etime=float(propv) / 1000.
                except ValueError:
                    
                    dt=propv.replace('T',' ')
                    dt2=dt.replace('Z',' ')
                    print dt2
                    etime=str2epoch(dt2)
            elif propk == 'mag':
                mag=float(propv)
            elif propk == 'magType':
                magtype=str(propv)
            elif propk == 'magtype':
                magtype=str(propv)
            elif propk == 'place':
                evname=str(propv)
            elif propk == 'flynn_region':
                evname=str(propv)
            elif propk == 'cdi':
                if propv is not None:
                    cdi=float(propv)
                    inull=float(propv)
            elif propk == 'felt':
                felt=propv
            elif propk == 'net':
                net=str(propv)
            elif propk == 'auth':
                net=str(propv)
            elif propk == 'code':
                code=str(propv)
            elif propk == 'source_id':
                code=str(propv)
            elif propk == 'updated':
                updated=propv / 1000.
            elif propk == 'lastupdate':
                dt=propv.replace('T',' ')
                dt2=dt.replace('Z',' ')
                print dt2
                updated=str2epoch(dt2)
            elif propk == 'place':
                place=str(propv)
    #        print propk
        
        #import pdb;pdb.set_trace()
        # force M to be mb, seems to bee good compromise....
        if magtype.lower() == 'm':
            magtype = 'mb'

        if magtype.lower()   == 'ml':
            ml=mag
        elif magtype.lower() == 'mb':
            mb=mag
        elif magtype.lower() == 'ms':
            ms=mag    
        # grn, srn seems to be unimplemenmted
        #gr=stock.grn(lat,lon)    
        #sr=stock.srn(lat,lon)    
        jdate=yearday(etime)
        
        fkey=str('%s%s' % (net, code))

        kmatch=dblookup(kdb,'','idmatch','','dbSCRATCH')
        try:
            dbputv(kmatch,'fkey', fkey)
        except Exception,e:
            print "Error :",e

        rec_list=dbmatches(kmatch,idmatch,'id-hook','fkey','fkey')        
        new_event=False
        evid=0
        updated_event=False
        if len(rec_list) > 1:
            print "found too many keys, sth strange goes on here" 
        if len(rec_list) > 0:
            for rec in rec_list:
                idmatch[3]=rec
                [ftime,kname,kval]=dbgetv(idmatch,'ftime','keyname', 'keyvalue')
                #print "found key %s %s" % (kname, kval) 
                if kname=='evid':
                    evid = kval
                    if updated > ftime:
                        new_event=False
                        updated_event=True
                    else:
                        updated_event=False


        else:        
            new_event=True
        
        
        if new_event:    
            if verbose:
                print "new event %s" % code
            evid=dbnextid(dborigin,'evid')
            orid=dbnextid(dborigin,'orid')
            orecno=dbaddv(dborigin,'time',etime,'lat',lat,'lon',lon,'depth',depth,
                'evid',evid,'orid',orid,
                'jdate',jdate,
                'mb',mb,'ml',ml,'ms',ms,
                'nass',0,'ndef',0,'auth','EMSC') 
            erecno=dbaddv(dbevent,'evid',evid,'prefor',orid,'evname',evname[:evname_width],'auth','EMSC')
            nmrecno=dbaddv(dbnetmag,'evid',evid,'orid',orid,'magnitude',mag,'magtype',magtype,'auth','EMSC-Webservice')
            dbaddv(idmatch,'fkey',fkey,'keyname','evid','keyvalue',evid,'ftime',updated)
        elif updated_event:
            if verbose:
                print "updated event %s" % code
            dbputv(idmatch,'ftime',updated)
            kmatch=dblookup(db,'','event','','dbSCRATCH')
            dbputv(kmatch,'evid',evid)
            evlist=dbmatches(kmatch,dbevent,'event-hook','evid','evid')
            if len(evlist) >1:
                print "strange, found a few matching events for evid %d " % evid
            if len(evlist) >0:
                dbevent[3]=evlist[0]
                [prefor]=dbgetv(dbevent,'prefor')

                kmatch=dblookup(db,'','origin','','dbSCRATCH')
                #if update, ansonsten adden...
                dbputv(kmatch,'orid',prefor)
                orlist=dbmatches(kmatch,dborigin,'orig-hook','orid')
                if len(orlist) >1:
                    print "strange, found a few origind for orid %d" % prefor
                if len(orlist)>0:
                    dborigin[3]=orlist[0]
                    dbputv(dborigin,'time',etime,'lat',lat,'lon',lon,'depth',depth,
                        'jdate',jdate) 
                    if magtype.lower()   == 'ml':
                        dbputv(dborigin,'ml',mag)
                    elif magtype.lower() == 'mb':
                        dbputv(dborigin,'mb',mag)
                    elif magtype.lower() == 'ms':
                        dbputv(dborigin,'ms',mag)
                    kmatch=dblookup(db,'','netmag','','dbSCRATCH')
                    dbputv(kmatch,'orid',prefor)
                    maglist=dbmatches(kmatch,dbnetmag,'mag-hook','orid')
                    if len(maglist)>1:
                        print "strange, found a few netmags for origin %d" % prefor
                    if len(maglist) > 0:
                        dbnetmag[3]=maglist[0]
                        dbputv(dbnetmag,'magnitude',mag,'magtype',magtype,'auth','EMSC-Webservice')
                
            
    return 0    

if __name__ == '__main__':
    status = main()
    sys.exit(status)
