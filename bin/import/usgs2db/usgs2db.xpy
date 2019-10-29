'''
@author      Nikolaus Horn <Nikolaus.Horn@zamg.ac.at>
@created     2013-11-25
@modified    2014-02-15
@version     1.0
@license     MIT-style license
'''


# Import Antelope modules

import antelope.datascope as ds
import antelope.stock as stock
import getopt
import codecs
import urllib.request, urllib.error, urllib.parse
import json
import pprint

def usage():
    print(sys.argv[0], "[-v] [-a auth] [-k keydb] [-u url] dbname") 

def main():    
    BASE_URL="http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson"
    verbose=0
    archive=0
    opts = []
    args = []
    keydbname='keydb'
    keyschema='idmatch1.0'
    auth='NEIC'
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'a:k:u:v', '')
    except getopt.GetoptError:
        print("illegal option")
        usage()
        sys.exit(2)

    for o,a in opts:
        if o == '-v':
            verbose = 1
        elif o == '-a':	
            auth = a
        elif o == '-u':	
            BASE_URL = a
        elif o == '-k':
            keydbname=a
            
    if len(args) > 1 or len(args) < 1:
        usage()
        sys.exit(1)

    if len(args) > 0:
        dbname=args[0]

    db= ds.dbopen( dbname, "r+" )
    dborigin=db.lookup(table='origin')
    dbevent=db.lookup(table='event')
    dbnetmag=db.lookup(table='netmag')

    dbq=db.lookup(table='origin',field='ml',record='dbNULL')
    [mlnull]=dbq.getv('ml')
    dbq=db.lookup(table='event',field='evname',record='dbNULL')
    evname_width=dbq.query('dbFIELD_SIZE')

    kdb=ds.dbopen(keydbname,'r+')
    descname=kdb.query('dbDATABASE_FILENAME')
    if os.path.exists(descname):
        schemaname=kdb.query('dbSCHEMA_NAME')
        if schemaname != keyschema:
            print("keydb %s has wrong schema %s, should be %s" % (keydbname,schemaname,keyschema)) 
            sys.exit(1)
    else:
        kdb.close()
        ds.dbcreate(keydbname,keyschema)

        kdb=ds.dbopen(keydbname,'r+')
    try:
        idmatch=kdb.lookup(table='idmatch')
    except Exception as e:
        print("Error :",e)
    

    #proxies={'http':'http://138.22.156.44:3128'}
    url=urllib.request.urlopen(BASE_URL)
    gj_string=url.read()
    obj=json.loads(gj_string)
    data=obj['features']    
    i=len(data)    
    for index in range(i):
        fdata=data[index]
        geom_type=fdata['type']    
        geometry=fdata['geometry']    
        coordinates=geometry['coordinates']    
        lon=  coordinates[0]    
        lat=  coordinates[1]    
        depth=coordinates[2]    
        properties=fdata['properties']    
        mb=ms=ml=mlnull    
        time=status=cdi=place=code=felt=mag=magtype=net=evtype=''    
        ml=mb=ms=mlnull
        # be sure to convert unicode objects to string objects by calling "str(xxx)", 
        # this prevents datascope  from CRASHING
        for propk, propv in properties.items():
            if propk ==   'time':
                etime=float(propv) / 1000.
            elif propk == 'mag':
                mag=float(propv)
            elif propk == 'magType':
                magtype=str(propv)
            elif propk == 'place':
                evname=str(propv)
            elif propk == 'cdi':
                if propv is not None:
                    cdi=float(propv)
                    inull=float(propv)
            elif propk == 'felt':
                felt=propv
            elif propk == 'net':
                net=str(propv)
            elif propk == 'code':
                code=str(propv)
            elif propk == 'updated':
                updated=propv / 1000.
            elif propk == 'place':
                place=str(propv)
        
        if magtype.lower()   == 'ml':
            ml=mag
        elif magtype.lower() == 'mb':
            mb=mag
        elif magtype.lower() == 'ms':
            ms=mag    
        # grn, srn seems to be unimplemenmted
        gr=stock.grnumber(lat,lon)    
        sr=stock.srnumber(lat,lon)    
        jdate=stock.epoch2str(etime,'%Y%j')
        
        fkey=str('%s%s' % (net, code))

        kmatch=idmatch.lookup(table='idmatch',record='dbSCRATCH')
        try:
            kmatch.putv(('fkey', fkey))
        except Exception as e:
            print("Error :",e)

        matcher=kmatch.matches(idmatch,'fkey')        
        rec_list=matcher()
        new_event=False
        evid=0
        updated_event=False
        if len(rec_list) > 1:
            print("found too many keys, sth strange goes on here") 
        if len(rec_list) > 0:
            for rec in rec_list:
                idmatch.record=rec
                [ftime,kname,kval]=idmatch.getv('ftime','keyname', 'keyvalue')
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
                print("new event %s" % code)
            evid=dborigin.nextid('evid')
            orid=dborigin.nextid('orid')
            orecno=dborigin.addv( ('time',etime),('lat',lat),('lon',lon),('depth',depth), 
                    ('evid',evid),('orid',orid), ('jdate',jdate), 
                    ('mb',mb),('ml',ml),('ms',ms), 
                    ('nass',0),('ndef',0),('auth',auth),('grn',gr),('srn',sr) ) 
            erecno=dbevent.addv(('evid',evid),('prefor',orid),('evname',evname[:evname_width]),('auth',auth) )
            nmrecno=dbnetmag.addv(('evid',evid),('orid',orid),('magnitude',mag),('magtype',magtype),('auth',auth) )
            idmatch.addv(('fkey',fkey),('keyname','evid'),('keyvalue',evid),('ftime',updated) )
        elif updated_event:
            if verbose:
                print("updated event %s" % code)
            idmatch.putv( ('ftime',updated) )
            kmatch=db.lookup(table='event', record='dbSCRATCH')
            kmatch.putv( ('evid',evid) )
            evmatcher=kmatch.matches(dbevent,'evid')
            evlist=evmatcher()
            if len(evlist) >1:
                print("strange, found a few matching events for evid %d " % evid)
            if len(evlist) >0:
                dbevent.record=evlist[0]
                [prefor]=dbevent.getv('prefor')

                kmatch=db.lookup(table='origin',record='dbSCRATCH')
                kmatch.putv( ('orid',prefor) )
                ormatcher=kmatch.matches(dborigin,'orid')
                orlist=ormatcher()
                if len(orlist) >1:
                    print("strange, found a few origind for orid %d" % prefor)
                if len(orlist)>0:
                    dborigin.record=orlist[0]
                    dborigin.putv( ('time',etime),('lat',lat),('lon',lon),('depth',depth),
                        ('jdate',jdate) ) 
                    if magtype.lower()   == 'ml':
                        dborigin.putv( ('ml',mag) )
                    elif magtype.lower() == 'mb':
                        dborigin.putv( ('mb',mag) )
                    elif magtype.lower() == 'ms':
                        dborigin.putv( ('ms',mag) )
                    kmatch=db.lookup(table='netmag',record='dbSCRATCH')
                    kmatch.putv( ('orid',prefor) )
                    magmatcher=kmatch.matches(dbnetmag,'orid')
                    maglist=magmatcher()
                    if len(maglist)>1:
                        print("strange, found a few netmags for origin %d" % prefor)
                    if len(maglist) > 0:
                        dbnetmag.record=maglist[0]
                        dbnetmag.putv( ('magnitude',mag) ,('magtype',magtype),('auth',auth) )
                
            
    return 0    

if __name__ == '__main__':
    status = main()
    sys.exit(status)
