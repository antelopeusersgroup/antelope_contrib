import re,os,sys
import json
import hashlib
#import inspect
import socket
import pprint
import resource as sysresource
from collections import defaultdict
from datetime import datetime, timedelta


class event2jsonException(Exception):
    """
    Local class to raise Exceptions to the
    rtwebserver framework.
    """
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return 'event2jsonException: %s' % (self.msg)
    def __str__(self):
        return repr(self)

if __name__ == '__main__':
    raise event2jsonException( 'DO NOT RUN DIRECTLY!!! Use rtwebserver framework' )

try:
    from twisted.web import server
    from twisted.web.resource import Resource
    from twisted.internet import reactor, defer
    from twisted.internet.threads import deferToThread
except Exception,e:
    raise event2jsonException( 'Problems loading Twisted libs: %s' % e )

try:
    import antelope.elog as elog
    import antelope.stock as stock
    import antelope.datascope as datascope
except Exception,e:
    raise event2jsonException( 'Problems loading Antelope libs: %s' % e )

try:
    import rtwebserver.config as config
except Exception,e:
    raise event2jsonException( 'Problems loading local libs: %s' % e )


def test_table(dbname,tbl,verbose=False):
    """
    Verify that we can work with table.
    Returns True if valid and we see data.
    """

    path = False

    try:
        with datascope.closing(datascope.dbopen( dbname , 'r' )) as db:
            db = db.lookup( table=tbl )

            if not db.query(datascope.dbTABLE_PRESENT):
                if verbose: elog.complain( 'No dbTABLE_PRESENT on %s' % dbname )
                return False

            if not db.record_count:
                if verbose: elog.complain( 'No %s.record_count' % dbname )
                return False

            path = db.query('dbTABLE_FILENAME')
    except Exception,e:
        elog.complain("Prolembs with db[%s]: %s" % (dbname,e) )
        return False

    return path

class Events(Resource):

    isLeaf = True
    allowedMethods = ("GET")

    def __init__(self):
        """
        Load class and get the data
        """

        self.dbs = {}
        self.verbose = stock.yesno( config.sitedict['siteconfig']['verbose'] )
        self.debug = stock.yesno( config.sitedict['siteconfig']['debug'] )
        self.timeformat = config.sitedict['siteconfig']['timeformat']
        self.timezone = config.sitedict['siteconfig']['timezone']
        self.time_limit = config.sitedict['siteconfig']['time_limit']
        self.refresh = int(config.sitedict['siteconfig']['refresh'])
        self.dbname = config.sitedict['siteconfig']['databases']
        try:
            self.readableJSON = int(config.sitedict['siteconfig']['readableJSON'])
        except:
            self.readableJSON = None

        self.event_cache = {}

        self.loading = True

        if self.debug:
            self.verbose = self.debug

        if not self.refresh:
            self.refresh = 60 * 60 # every hour default

        if self.verbose:
            elog.notify( "Events(): init()" )
            elog.notify( "\tdebug: %s" % self.debug )
            elog.notify( "\tverbose: %s" % self.verbose )
            elog.notify( "\ttimeformat: %s" % self.timeformat )
            elog.notify( "\ttimezone: %s" % self.timezone )
            elog.notify( "\ttime_limit: %s" % self.time_limit )
            elog.notify( "\tdbname: %s" % self.dbname )
            elog.notify( "\trefresh: %s" % self.refresh )
            elog.notify( "\treadableJSON: %s" % self.readableJSON )


        if self.verbose:
            elog.notify( '\t' + '#'*20 )
            elog.notify( '\tLoading Events!' )
            elog.notify( '\t' + '#'*20 )


        for name,path in self.dbname.iteritems():
            elog.notify( "Test %s db: %s" % (name,path) )

            for table in ['event','origin','netmag']:
                present = test_table(path,table,self.debug)
                if not present:
                    elog.complain('Empty or missing %s.%s' % (path,table) )

                if table is 'event': event = present
                if table is 'origin': origin = present
                if table is 'netmag': netmag = present

            if not origin:
                elog.complain( 'Cannot work without origin table.')
                continue

            db = datascope.dbopen( path , 'r' )
            self.dbs[name] = { 'db':db, 'path':path, 'mags':{},
                    'md5event':False, 'md5origin':False, 'md5netmag':False,
                    'origin': origin, 'event':event, 'netmag':netmag }


        deferToThread(self._init_in_thread)

    def _init_in_thread(self):

        elog.notify( 'Loading Events()' )

        self._get_event_cache()
        self.loading = False

        elog.notify( 'Done loading Events()' )
        elog.notify( '\nREADY!\n' )

    def _cache(self, db=False):
        """
        Return cached data.
        """
        temp = []

        try:
            if db in self.event_cache:
                return self.event_cache[db]
            else:
                return json.dumps({'valid_db': self.event_cache.keys(),
                    'error':'No ?db=*** spefied in URL.'})

        except Exception,e:
            elog.complain('Cannot find self.table(%s) => %s' % (db,e) )
            return False


    def render_GET(self, uri):

        try:
            (host,port) = uri.getHeader('host').split(':', 1)
        except:
            host = uri.getHeader('host')
            port = '-'

        hostname = socket.gethostname()

        if self.verbose:
            elog.notify("render_GET(): [%s] %s:%s%s" % (hostname,host,port,uri.uri))

        if self.debug:
            elog.debug('')
            elog.debug('render_GET() uri.uri:%s' % uri.uri)
            elog.debug('render_GET() uri.args:%s' % (uri.args) )
            elog.debug('render_GET() uri.prepath:%s' % (uri.prepath) )
            elog.debug('render_GET() uri.postpath:%s' % (uri.postpath) )
            elog.debug('render_GET() uri.path:%s' % (uri.path) )

            elog.debug('\tQUERY: %s ' % uri)
            elog.debug('\tHostname => [%s:%s]'% (host,port))
            elog.debug('\tHost=> [%s]'% uri.host)
            #elog.debug('\tsocket.gethostname() => [%s]'% socket.gethostname())
            elog.debug('')

        d = defer.Deferred()
        d.addCallback( self._render_uri )
        reactor.callInThread(d.callback, uri)

        if self.debug: elog.debug("render_GET() - return server.NOT_DONE_YET")

        return server.NOT_DONE_YET

    def _render_uri(self,uri):

        if self.loading:
            html =  "<html><body><h1>Server Loading!</h1></body></html>"
            return self._uri_results( uri, html ,error=True)

        if 'db' in uri.args:
            return self._uri_results(uri,self._cache(uri.args['db'][0]))

        return self._uri_results(uri,self._cache())


    def _uri_results(self, uri=None, results=False, error=False):

        if not uri:
            elog.complain('No URI to work with on _uri_results()')
            return

        if self.debug:
            elog.debug('_uri_results  uri: %s' % uri)

        if results:
            if error:
                uri.setHeader("content-type", "text/html")
            else:
                uri.setHeader("content-type", "application/json")
                expiry_time = datetime.utcnow() + timedelta(seconds=self.refresh)
                uri.setHeader("expires", expiry_time.strftime("%a, %d %b %Y %H:%M:%S GMT"))
            uri.write(results)

        else:
            elog.complain('No results from query.')
            uri.setHeader("content-type", "text/html")
            uri.setResponseCode( 500 )
            uri.write('Problem with server!')
            elog.complain( '_uri_results() Problem: No data for :%s' % uri )

        try:
            uri.finish()
        except Exception,e:
            elog.complain( '_uri_results() Problem: %s' % e )

        if self.debug: elog.debug( '_uri_results() DONE!' )

    def _get_magnitudes(self,db):

        mags = {}

        if self.debug: elog.debug('Get magnitudes ' )

        steps = ['dbopen netmag', 'dbsubset orid!=NULL']

        if self.time_limit:
            steps.extend(["dbsubset lddate > %d" % (stock.now() - float(self.time_limit))] )

        with datascope.freeing(db.process( steps )) as dbview:

            if self.debug: elog.debug('Got %s mags from file' % dbview.record_count )

            for record in dbview.iter_record():

                [orid, magid, magnitude, magtype,
                    auth, uncertainty, lddate ] = \
                    record.getv('orid', 'magid', 'magnitude',
                    'magtype', 'auth','uncertainty', 'lddate')

                try:
                    printmag = '%0.1f %s' % ( float(magnitude), magtype )
                except:
                    printmag = '-'

                if not orid in mags:
                    mags[orid] = {}

                mags[orid][magid] = {'magnitude':magnitude, 'printmag':printmag,
                        'lddate':lddate, 'magtype':magtype, 'auth':auth,
                        'uncertainty':uncertainty, 'magid':magid }

        return mags

    def _get_md5(self,file):
        """
        Get the checksum of a table
        """

        if os.path.isfile( file ):
            return hashlib.md5( open(file).read() ).hexdigest()

        return False


    def _memory_usage_resource(self):
        """
        Nice print of memory usage.
        """
        rusage_denom = 1024.
        if sys.platform == 'darwin':
            # ... it seems that in OSX the output is different units ...
            rusage_denom = rusage_denom * rusage_denom
        mem = sysresource.getrusage(sysresource.RUSAGE_SELF).ru_maxrss / rusage_denom
        return mem


    def _get_event_cache(self):
        """
        Private function to load the data from the tables
        """

        if self.debug:
            elog.debug( "Using approx. %0.1f MB of memory" % self._memory_usage_resource() )
        tempcache = {}

        for name in self.dbs:

            path = self.dbs[name]['path']
            mags = self.dbs[name]['mags']
            db = self.dbs[name]['db']

            origin = self.dbs[name]['origin']
            md5origin = self.dbs[name]['md5origin']

            event = self.dbs[name]['event']
            md5event = self.dbs[name]['md5event']

            netmag = self.dbs[name]['netmag']
            md5netmag = self.dbs[name]['md5netmag']

            if self.debug: elog.debug( "Events(%s): db: %s" % (name,path) )


            testorigin = self._get_md5(origin)
            testevent = self._get_md5(event) if event else False
            testnetmag = self._get_md5(netmag) if netmag else False


            if self.debug:
                elog.debug('event [old: %s new: %s]' %(md5event,testevent) )
                elog.debug('origin [old: %s new: %s]' %(md5origin,testorigin) )
                elog.debug('netmag [old: %s new: %s]' %(md5netmag,testnetmag) )

            if testorigin == md5origin and testevent == md5event and testnetmag == md5netmag:
                if self.debug: elog.debug('No update needed. Skipping.')
                continue

            tempcache[name] = []

            self.dbs[name]['md5event'] = testevent
            self.dbs[name]['md5origin'] = testorigin

            if testnetmag != netmag:
                mags = self._get_magnitudes(db)
                self.dbs[name]['mags'] = mags
                self.dbs[name]['md5netmag'] = testnetmag

            if event:
                steps = ['dbopen event']
                steps.extend(['dbjoin origin'])
                steps.extend(['dbsubset orid!=NULL'])
                steps.extend(['dbsubset orid==prefor'])
            else:
                steps = ['dbopen origin']

            if self.time_limit:
                steps.extend(["dbsubset time > %d" % (stock.now() - float(self.time_limit))] )


            steps.extend(['dbsort -r time'])

            if self.verbose:
                elog.notify( 'Events(%s): updating from %s' % (name,path) )

            if self.debug:
                elog.debug( ', '.join(steps) )


            with datascope.freeing(db.process( steps )) as dbview:

                if not dbview.record_count:
                    elog.complain( 'Events(%s): No records %s' % (name,path) )
                    continue

                for temp in dbview.iter_record():

                    (orid,time,lat,lon,depth,auth,nass,review) = \
                            temp.getv('orid','time','lat','lon','depth',
                                    'auth','nass','review')

                    evid = orid
                    if event:
                        evid = temp.getv('evid')[0]


                    if self.debug: elog.debug( "Events(%s): new evid #%s" % (name,evid) )

                    allmags = []
                    magnitude = '-'
                    maglddate = 0
                    strtime = stock.epoch2str(time, self.timeformat, self.timezone)
                    try:
                        srname = stock.srname(lat,lon)
                        grname = stock.grname(lat,lon)
                    except Exception,e:
                        error = 'Problems with (s/g)rname for orid %s: %s' % (orid,lat,lon,e) 
                        elog.complain(error)
                        srname = '-'
                        grname = '-'

                    if orid in mags:
                        for o in mags[orid]:
                            allmags.append(mags[orid][o])
                            if mags[orid][o]['lddate'] > maglddate:
                                magnitude = mags[orid][o]['printmag']
                                maglddate = mags[orid][o]['lddate']


                    tempcache[name].append({'time':time, 'lat':lat, 'srname':srname,
                            'evid':evid, 'orid':orid, 'lon':lon, 'magnitude':magnitude,
                            'grname': grname, 'review': review, 'strtime':strtime,
                            'allmags': allmags, 'depth':depth, 'auth':auth, 'nass':nass})

                    if self.debug: elog.debug( "Events(): %s add (%s,%s)" % (name,evid,orid) )

            self.event_cache[name] = json.dumps(tempcache[name],indent=self.readableJSON)

            if self.debug: elog.debug( "Completed updating db. (%s)" % name )


        if self.debug: elog.debug( "Schedule update in (%s) seconds" % self.refresh )
        reactor.callLater(self.refresh, self._get_event_cache )

resource = Events()
