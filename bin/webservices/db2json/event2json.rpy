import re,os,sys
import json
#import inspect
import socket
import pprint
from collections import defaultdict
from datetime import datetime, timedelta

# safe to import * here
from rtwebserver.db2json_libs import *

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

class Events(Resource):

    # isLeaf not working in rtwebserver for now...
    isLeaf = True
    allowedMethods = ("GET")

    def __init__(self):
        """
        Load class and get the data
        """


        self.dbs = {}
        self.pf_keys = {
                'verbose':{'type':'bool','default':False},
                'timeformat':{'type':'str','default':'%d (%j) %h:%m:%s %z'},
                'timezone':{'type':'str','default':'utc'},
                'time_limit':{'type':'int','default':3600},
                'refresh':{'type':'int','default':60},
                'databases':{'type':'dict','default':{}},
                'readableJSON':{'type':'int','default':0}
                }

        self._read_pf()


        self.event_cache = {}

        self.loading = True

        self._log( "Events(): init()" )

        self._log( '\t' + '#'*20 )
        self._log( '\tLoading Events!' )
        self._log( '\t' + '#'*20 )


        for name,path in self.databases.iteritems():
            self._log( "Test %s db: %s" % (name,path) )

            for table in ['event','origin','netmag']:
                present = test_table(path,table)
                if not present:
                    self._complain('Empty or missing %s.%s' % (path,table) )

                if table is 'event': event = present
                if table is 'origin': origin = present
                if table is 'netmag': netmag = present

            if not origin:
                self._complain( 'Cannot work without origin table.')
                continue

            #db = datascope.dbopen( path , 'r' )
            self.dbs[name] = { 'db':path, 'path':path, 'mags':{},
                    'md5event':False, 'md5origin':False, 'md5netmag':False,
                    'origin': origin, 'event':event, 'netmag':netmag }


        deferToThread(self._init_in_thread)


    def _log(self,msg):
        if self.verbose:
            elog.notify( 'event2json: %s' % msg )

    def _complain(self,msg):
        elog.complain( 'evnet2json: PROBLEM: %s' % msg )


    def _read_pf(self):
        """
        Read configuration parameters from rtwebserver pf file.
        """

        elog.notify( 'Read parameters from pf file')

        module = 'event2jsonconfig'

        for attr in self.pf_keys:
            try:
                if self.pf_keys[attr]['type'] == 'int':
                    value = int(config.sitedict[module][attr])
                elif self.pf_keys[attr]['type'] == 'bool':
                    value = test_yesno(config.sitedict[module][attr])
                elif self.pf_keys[attr]['type'] == 'str':
                    value = str(config.sitedict[module][attr])
                else:
                    value = config.sitedict[module][attr]
            except Exception,e:
                value = self.pf_keys[attr]['default']

            setattr(self, attr, value )

            elog.notify( "%s: read_pf[%s]: %s" % (module, attr,getattr(self,attr) ) )


    def _init_in_thread(self):

        self._log( 'Loading Events()' )

        self._get_event_cache()
        self.loading = False

        self._log( 'Done loading Events()' )
        self._log( '\nREADY!\n' )

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
            self._complain('Cannot find self.table(%s) => %s' % (db,e) )
            return False


    def render_GET(self, uri):

        try:
            (host,port) = uri.getHeader('host').split(':', 1)
        except:
            host = uri.getHeader('host')
            port = '-'

        hostname = socket.gethostname()
        self._log("render_GET(): [%s] %s:%s%s" % (hostname,host,port,uri.uri))

        #self._log('')
        #self._log('render_GET() uri.uri:%s' % uri.uri)
        #self._log('render_GET() uri.args:%s' % (uri.args) )
        #self._log('render_GET() uri.prepath:%s' % (uri.prepath) )
        #self._log('render_GET() uri.postpath:%s' % (uri.postpath) )
        #self._log('render_GET() uri.path:%s' % (uri.path) )

        #self._log('\tQUERY: %s ' % uri)
        #self._log('\tHostname => [%s:%s]'% (host,port))
        #self._log('\tHost=> [%s]'% uri.host)
        #self._log('\tsocket.gethostname() => [%s]'% socket.gethostname())
        #self._log('')

        d = defer.Deferred()
        d.addCallback( self._render_uri )
        reactor.callInThread(d.callback, uri)

        self._log("render_GET() - return server.NOT_DONE_YET")

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
            self._complain('No URI to work with on _uri_results()')
            return

        self._log('_uri_results  uri: %s' % uri)

        if results:
            if error:
                uri.setHeader("content-type", "text/html")
            else:
                uri.setHeader("content-type", "application/json")
                expiry_time = datetime.utcnow() + timedelta(seconds=self.refresh)
                uri.setHeader("expires", expiry_time.strftime("%a, %d %b %Y %H:%M:%S GMT"))
            uri.write(results)

        else:
            self._complain('No results from query.')
            uri.setHeader("content-type", "text/html")
            uri.setResponseCode( 500 )
            uri.write('Problem with server!')
            self._complain( '_uri_results() Problem: No data for :%s' % uri )

        try:
            uri.finish()
        except Exception,e:
            self._complain( '_uri_results() Problem: %s' % e )

        self._log( '_uri_results() DONE!' )

    def _get_magnitudes(self,db):

        mags = {}

        self._log('Get magnitudes ' )

        steps = ['dbopen netmag', 'dbsubset orid!=NULL']

        if self.time_limit:
            steps.extend(["dbsubset lddate > %d" % (stock.now() - float(self.time_limit))] )

        with datascope.freeing(db.process( steps )) as dbview:

            self._log('Got %s mags from file' % dbview.record_count )

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



    def _get_event_cache(self):
        """
        Private function to load the data from the tables
        """

        tempcache = {}

        for name in self.dbs:

            path = self.dbs[name]['path']
            mags = self.dbs[name]['mags']
            path = self.dbs[name]['db']

            with datascope.closing(datascope.dbopen( path , 'r' )) as db:

                origin = self.dbs[name]['origin']
                md5origin = self.dbs[name]['md5origin']

                event = self.dbs[name]['event']
                md5event = self.dbs[name]['md5event']

                netmag = self.dbs[name]['netmag']
                md5netmag = self.dbs[name]['md5netmag']

                self._log( "Events(%s): db: %s" % (name,path) )


                testorigin =get_md5(origin)
                testevent = get_md5(event) if event else False
                testnetmag = get_md5(netmag) if netmag else False


                self._log('event [old: %s new: %s]' %(md5event,testevent) )
                self._log('origin [old: %s new: %s]' %(md5origin,testorigin) )
                self._log('netmag [old: %s new: %s]' %(md5netmag,testnetmag) )

                if testorigin == md5origin and testevent == md5event and testnetmag == md5netmag:
                    self._log('No update needed. Skipping.')
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
                    self._log( 'Events(%s): updating from %s' % (name,path) )

                self._log( ', '.join(steps) )


                with datascope.freeing(db.process( steps )) as dbview:

                    if not dbview.record_count:
                        self._complain( 'Events(%s): No records %s' % (name,path) )
                        continue

                    for temp in dbview.iter_record():

                        (orid,time,lat,lon,depth,auth,nass,ndef,review) = \
                                temp.getv('orid','time','lat','lon','depth',
                                        'auth','nass','ndef','review')

                        evid = orid
                        if event:
                            evid = temp.getv('evid')[0]


                        self._log( "Events(%s): new evid #%s" % (name,evid) )

                        allmags = []
                        magnitude = '-'
                        maglddate = 0
                        strtime = stock.epoch2str(time, self.timeformat, self.timezone)
                        try:
                            srname = stock.srname(lat,lon)
                            grname = stock.grname(lat,lon)
                        except Exception,e:
                            error = 'Problems with (s/g)rname for orid %s: %s' % (orid,lat,lon,e) 
                            self._complain(error)
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
                                'allmags': allmags, 'depth':depth, 'auth':auth,
                                'ndef': ndef, 'nass':nass})

                        self._log( "Events(): %s add (%s,%s)" % (name,evid,orid) )

                self.event_cache[name] = json.dumps(tempcache[name],indent=self.readableJSON)

                self._log( "Completed updating db. (%s)" % name )


        self._log( "Schedule update in (%s) seconds" % self.refresh )
        reactor.callLater(self.refresh, self._get_event_cache )

resource = Events()
