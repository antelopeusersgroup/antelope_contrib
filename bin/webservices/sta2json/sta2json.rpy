import re,os,sys

if __name__ == '__main__':
    sys.exit( 'DO NOT RUN DIRECTLY!!! Use rtwebserver framework' )

import json
import hashlib
import socket
import pprint
import resource as sysresource
from collections import defaultdict
from datetime import datetime, timedelta

# safe to import * here
from rtwebserver.db2json_libs import *

try:
    from twisted.web import server
    from twisted.web.resource import Resource
    from twisted.internet import reactor, defer
    from twisted.internet.threads import deferToThread
except Exception,e:
    raise sta2jsonException( 'Problems loading Twisted libs: %s' % e )

try:
    import antelope.elog as elog
    import antelope.stock as stock
    import antelope.datascope as datascope
except Exception,e:
    raise sta2jsonException( 'Problems loading Antelope libs: %s' % e )

try:
    import rtwebserver.config as config
except Exception,e:
    raise sta2jsonException( 'Problems loading local libs: %s' % e )


class Stations(Resource):

    # isLeaf not working in rtwebserver for now...
    isLeaf = True
    allowedMethods = ("GET")

    def __init__(self):
        """
        Load class and get the data
        """

        self.loading = True

        self._read_pf()

        self.sta_cache = {}

        self.tables = ['deployment','site','snetsta','sitechan',
                    'comm','sensor','instrument']

        for name,path in self.dbname.iteritems():
            elog.notify( "Test %s db: %s" % (name,path) )

            self.dbs[name] = { 'tables':{} }
            for table in self.tables:
                present = test_table(path,table,self.debug)
                if not present:
                    raise sta2jsonException('Empty or missing %s.%s' % (path,table) )

                self.dbs[name]['tables'][table] = { 'path':present, 'md5':False }

            db = datascope.dbopen( path , 'r' )
            self.dbs[name]['db'] = db
            self.dbs[name]['path'] = path


        deferToThread(self._init_in_thread)


    def _read_pf(self):
        """
        Read configuration parameters from rtwebserver pf file.
        """
        self.dbs = {}
        try:
            self.verbose = stock.yesno( config.sitedict['sta2jsonconfig']['verbose'] )
            self.debug = stock.yesno( config.sitedict['sta2jsonconfig']['debug'] )
            self.timeformat = config.sitedict['sta2jsonconfig']['timeformat']
            self.timezone = config.sitedict['sta2jsonconfig']['timezone']
            self.sta_subset = config.sitedict['sta2jsonconfig']['sta_subset']
            self.refresh = int(config.sitedict['sta2jsonconfig']['refresh'])
            self.dbname = config.sitedict['sta2jsonconfig']['databases']
        except Exception,e:
            raise sta2jsonException( 'Problems reading pf values: %s' % e )

        try:
            self.readableJSON = int(config.sitedict['sta2jsonconfig']['readableJSON'])
        except:
            self.readableJSON = None

        if self.verbose:
            elog.notify( "Stations(): init()" )
            elog.notify( "\tdebug: %s" % self.debug )
            elog.notify( "\tverbose: %s" % self.verbose )
            elog.notify( "\ttimeformat: %s" % self.timeformat )
            elog.notify( "\ttimezone: %s" % self.timezone )
            elog.notify( "\tsubset: %s" % self.sta_subset )
            elog.notify( "\tdbname: %s" % self.dbname )
            elog.notify( "\trefresh: %s" % self.refresh )
            elog.notify( "\treadableJSON: %s" % self.readableJSON )

        if self.debug:
            self.verbose = self.debug

        if not self.refresh:
            self.refresh = 60 * 60 # every hour default


    def _init_in_thread(self):

        elog.notify( 'Loading Stations()' )

        self._get_sta_cache()
        self.loading = False

        elog.notify( 'Done loading Stations()' )
        elog.notify( '\nREADY!\n' )

    def _cache(self, flags):
        """
        Return cached data.
        """

        if not flags['db']:
            result = {'valid_db': self.sta_cache.keys(),
                'error':'No ?db=*** spefied in URL.'}

        if flags['db'] in self.sta_cache:
            result = self.sta_cache[ flags['db'] ]

            if flags['active']:
                result = result['active']
                if flags['snet'] in result:
                    result = result[ flags['snet'] ]
            elif flags['decom']:
                result = result['decom']
                if flags['snet'] in result:
                    result = result[ flags['snet'] ]
            elif flags['list']:
                result = result['list']
                if flags['snet'] in result:
                    result = result[ flags['snet'] ]
            else:
                result = result['full']
                if flags['snet'] in result:
                    result = result[ flags['snet'] ]
                    if flags['sta'] in result:
                        result = result[ flags['sta'] ]

        else:
            result = {'valid_db': self.sta_cache.keys(),
                'error':'No valid database in URL.'}

        return result


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


        db = False
        snet = False
        sta = False

        flags = {'db':False, 'snet':False,
                'sta':False, 'active':False,
                'decom':False, 'list':False}

        for var in flags.keys():
            if var in uri.args:
                flags[var] = uri.args[var][0]

        #if self.debug:
        #    for var in flags.keys():
        #        elog.debug('_render_uri() %s:%s' % (var,flags[var]) )

        return self._uri_results(uri,self._cache(flags))


    def _uri_results(self, uri=None, results=False, error=False):

        if not uri:
            elog.complain('No URI to work with on _uri_results()')
            return

        if self.debug:
            elog.debug('_uri_results  uri: %s' % uri)

        if results:
            if error:
                uri.setHeader("content-type", "text/html")
                uri.write(results)
            else:
                uri.setHeader("content-type", "application/json")
                expiry_time = datetime.utcnow() + timedelta(seconds=self.refresh)
                uri.setHeader("expires", expiry_time.strftime("%a, %d %b %Y %H:%M:%S GMT"))
                uri.write( json.dumps(results,indent=self.readableJSON) )

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

    #def _get_magnitudes(self,db):

    #    mags = {}

    #    if self.debug: elog.debug('Get magnitudes ' )

    #    steps = ['dbopen netmag', 'dbsubset orid!=NULL']

    #    if self.sta_subset:
    #        steps.extend(["dbsubset '%s'" % self.sta_subset])

    #    with datascope.freeing(db.process( steps )) as dbview:

    #        if self.debug: elog.debug('Got %s mags from file' % dbview.record_count )

    #        for record in dbview.iter_record():

    #            [orid, magid, magnitude, magtype,
    #                auth, uncertainty, lddate ] = \
    #                record.getv('orid', 'magid', 'magnitude',
    #                'magtype', 'auth','uncertainty', 'lddate')

    #            try:
    #                printmag = '%0.1f %s' % ( float(magnitude), magtype )
    #            except:
    #                printmag = '-'

    #            if not orid in mags:
    #                mags[orid] = {}

    #            mags[orid][magid] = {'magnitude':magnitude, 'printmag':printmag,
    #                    'lddate':lddate, 'magtype':magtype, 'auth':auth,
    #                    'uncertainty':uncertainty, 'magid':magid }

    #    return mags

    def _get_md5(self,file):
        """
        Get the checksum of a table
        """

        if self.debug:
            elog.debug('checksum [%s]' % file)

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


    def _get_sta_cache(self):
        """
        Private function to load the data from the tables
        """

        if self.debug:
            elog.debug( "Using approx. %0.1f MB of memory" % self._memory_usage_resource() )
        tempcache = {}

        for database in self.dbs:

            db = self.dbs[database]['db']
            dbpath = self.dbs[database]['path']
            need_update = False
            if self.debug:
                elog.debug( "(%s) path:%s" % (database,dbpath) )

            for name in self.tables:

                path = self.dbs[database]['tables'][name]['path']
                md5 = self.dbs[database]['tables'][name]['md5']

                if self.debug:
                    elog.debug( "(%s) table:%s path:%s md5:%s" % (database,name,path,md5) )


                test = self._get_md5(path)

                if self.debug:
                    elog.debug('[old: %s new: %s]' %(md5,test) )

                if test != md5:
                    if self.debug: elog.debug('Update needed.')
                    self.dbs[database]['tables'][name]['md5'] = test
                    need_update = True

            if not need_update:
                continue

            tempcache[database] = { 'full':{}, 'active':{}, 'decom':{}, 'list':{} }
            first = self.tables[0]
            steps = [ 'dbopen %s' % x if first == x else 'dbjoin -o %s' % x for x in self.tables ]


            if self.sta_subset:
                steps.extend(["dbsubset %s" % self.sta_subset])


            steps.extend(['dbsort snet sta time'])

            if self.verbose:
                elog.notify( 'Stations(%s): updating %s' % (name,path) )

            if self.debug:
                elog.debug( ', '.join(steps) )

            with datascope.freeing(db.process( steps )) as dbview:
                if not dbview.record_count:
                    elog.complain( 'Stations(%s): No records %s' % (name,path) )
                    continue

                for temp in dbview.iter_record():

                    (snet,vnet,sta,time,endtime,lat,lon) = \
                            temp.getv('snet','vnet','sta','time',
                                    'endtime','lat','lon')

                    strtime = stock.epoch2str(time, self.timeformat, self.timezone)
                    strendtime = stock.epoch2str(endtime, self.timeformat, self.timezone)


                    if not snet in tempcache[database]['full']:
                        tempcache[database]['full'][snet] = {}
                        tempcache[database]['decom'][snet] = {}
                        tempcache[database]['active'][snet] = {}
                        tempcache[database]['list'][snet] = {}

                    if not sta in tempcache[database]['full'][snet]:
                        tempcache[database]['full'][snet][sta] = {}

                    tempcache[database]['full'][snet][sta] = {
                        'time':time, 'strtime':strtime,
                        'endtime':endtime, 'strendtime':strendtime,
                        'vnet':vnet, 'lat':lat, 'lon':lon}

                    tempcache[database]['list'][snet][sta] = []

                    if int(endtime) < stock.now():
                        tempcache[database]['decom'][snet][sta] = []
                    else:
                        tempcache[database]['active'][snet][sta] = []



                    if self.debug: elog.debug( "Stations(): %s (%s_%s)" % (database,snet,sta) )

            # Flatten those dictionaries
            for cache in ['decom','active','list']:
                for snet in tempcache[database][cache].keys():
                    tempcache[database][cache][snet] = \
                            tempcache[database][cache][snet].keys()

            self.sta_cache[database] = tempcache[database]

            if self.debug: elog.debug( "Completed updating db. (%s)" % database )


        if self.debug: elog.debug( "Schedule update in (%s) seconds" % self.refresh )
        reactor.callLater(self.refresh, self._get_sta_cache )

resource = Stations()
