"""
rtwebserver module to deliver station metadata
to web clients in JSON format.

NO BRTT SUPPORT!!!!!

Juan Reyes
reyes@ucsd.edu
"""

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
    import antelope.Pkt as Pkt
    import antelope.elog as elog
    import antelope.stock as stock
    import antelope.datascope as datascope
    import antelope.orb as orb
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

        self.dbs = {}
        self.orbs = {}

        self.db_cache = {}

        self.tables = ['deployment','site','comm','sensor','dlsensor','dlsite']

        self.pf_keys = {
                'verbose':{'type':'bool','default':False},
                'debug':{'type':'bool','default':False},
                'timeformat':{'type':'str','default':'%D (%j) %H:%M:%S %z'},
                'timezone':{'type':'str','default':'UTC'},
                'sta_subset':{'type':'str','default':False},
                'refresh':{'type':'int','default':60},
                'databases':{'type':'dict','default':{}},
                'orbnames':{'type':'dict','default':{}},
                'readableJSON':{'type':'int','default':0}
                }

        self._read_pf()

        if self.debug:
            self.verbose = self.debug

        if not self.refresh:
            self.refresh = 60 # every minute default


        # Check DATABASES
        for name,path in self.databases.iteritems():
            try:
                if self.verbose: elog.notify( "init %s DB: %s" % (name,path) )

                self.dbs[name] = {}
                self.dbs[name] = { 'tables':{} }
                for table in self.tables:
                    present = test_table(path,table,self.debug)
                    if not present:
                        raise sta2jsonException('Empty or missing %s.%s' % (path,table) )

                    self.dbs[name]['tables'][table] = { 'path':present, 'md5':False }

                db = datascope.dbopen( path , 'r' )
                self.dbs[name]['db'] = db
                self.dbs[name]['path'] = path

                deferToThread(self._get_sta_cache,name)
            except Exception,e:
                raise sta2jsonException( 'Problems on configured dbs: %s' % e )

        # Check ORBS
        for name,orbname in self.orbnames.iteritems():
            if self.verbose: elog.notify( "init %s ORB: %s" % (name,orbname) )

            self.orbs[name] = {}
            self.orbs[name]['clients'] = {}
            self.orbs[name]['sources'] = {}
            self.orbs[name]['info'] = {
                    'status':'offline',
                    'last_check':0,
                    'name':orbname
                    }

            self.orbs[name]['orb'] = orb.Orb(orbname)

            deferToThread(self._get_orb_cache, name)

        self.loading = False


        if self.verbose: elog.notify( 'Done loading Stations()' )


    def _read_pf(self):
        """
        Read configuration parameters from rtwebserver pf file.
        """

        elog.notify( 'Read parameters from pf file')

        for attr in self.pf_keys:
            try:
                if self.pf_keys[attr]['type'] == 'int':
                    value = int(config.sitedict['sta2jsonconfig'][attr])
                elif self.pf_keys[attr]['type'] == 'bool':
                    value = stock.yesno(config.sitedict['sta2jsonconfig'][attr])
                elif self.pf_keys[attr]['type'] == 'str':
                    value = str(config.sitedict['sta2jsonconfig'][attr])
                else:
                    value = config.sitedict['sta2jsonconfig'][attr]
            except Exception,e:
                value = self.pf_keys[attr]['default']

            setattr(self, attr, value )

            elog.notify( "\tsta2jsonconfig %s: %s" % (attr,getattr(self,attr) ) )

    def _get_orb_cache(self, name):

        if self.verbose: elog.notify( 'Check ORB(%s) sources' % name)

        if self.debug:
            elog.debug( "Using approx. %0.1f MB of memory" % self._memory_usage_resource() )

        pkt = Pkt.Packet()

        try:
            if self.verbose: elog.notify("connect to orb(%s)" % name )
            self.orbs[name]['orb'].connect()
        except Exception,e:
            self.orbs[name]['info']['status'] = e
            elog.complain('Cannot connect ORB [%s]: %s' % (orbname,e) )
        else:
            self.orbs[name]['info']['status'] = 'online'
            self.orbs[name]['info']['last_check'] = stock.now()
            try:
                # get clients
                if self.verbose: elog.notify("get clients orb(%s)" % name )
                result = self.orbs[name]['orb'].clients()

                for r in result:
                    if isinstance(r,float):
                        self.orbs[name]['info']['clients_time'] = r
                        if self.verbose: elog.notify("orb(%s) client time %s" % (name, r) )
                    else:
                        self.orbs[name]['clients'] = r
            except Exception,e:
                elog.notify("Cannot query orb(%s) %s %s" % (name, Exception, e) )

            try:
                # get sources
                if self.verbose: elog.notify("get sources orb(%s)" % name )
                result = self.orbs[name]['orb'].sources()

                for r in result:
                    if isinstance(r,float):
                        self.orbs[name]['info']['sources_time'] = r
                        if self.verbose: elog.notify("orb(%s) sources time %s" % (name, r) )
                    else:
                        for stash in r:

                            pkt.srcname = Pkt.SrcName(stash['srcname'])
                            net = pkt.srcname.net
                            sta = pkt.srcname.sta

                            if self.debug:
                                elog.notify("orb(%s) update %s %s" % (name,net,sta) )

                            if not net in self.orbs[name]['sources']:
                                self.orbs[name]['sources'][net] = {}

                            self.orbs[name]['sources'][net][sta] = stash
            except Exception,e:
                elog.notify("Cannot query orb(%s) %s %s" % (name, Exception, e) )

        self.orbs[name]['orb'].close()

        reactor.callLater(self.refresh, self._get_orb_cache, name )


    def _cache(self, flags):
        """
        Return cached data for query response.
        """

        if self.debug: elog.notify( '_cache()')

        results = {}
        clients = {}
        sources = {}

        if flags['orb'] in self.orbs:
            orb = flags['orb']

            if stock.yesno( flags['clients'] ):
                clients = self.orbs[orb]['clients']

            if stock.yesno( flags['sources'] ):
                if flags['snet'] and flags['sta']:
                    try:
                        sources[flags['snet']] = \
                            { flags['sta']: self.orbs[orb]['sources'][flags['snet']][flags['sta']] }
                    except:
                        pass

                elif flags['snet']:
                    try:
                        sources[flags['snet']] = self.orbs[orb]['sources'][flags['snet']]
                    except:
                        pass

                else:
                        sources = self.orbs[orb]['sources']

            return {
                    'info': self.orbs[orb]['info'],
                    'clients': clients,
                    'sources': sources
                    }


        elif flags['db'] in self.db_cache:
            db = flags['db']

            if flags['active']:
                return self.db_cache[db]['active']

            elif flags['decom']:
                return self.db_cache[db]['decom']

            else:
                if flags['snet'] and flags['sta']:
                    snet = flags['snet']
                    sta = flags['sta']

                    try:
                        results = self.db_cache[db]['full'][snet][sta]
                    except:
                        return results

                    try:
                        results['orb'] = self.orbs[db]['sources'][snet][sta]
                    except:
                        results['orb'] = {}

                    return results

                else:
                    return self.db_cache[db]['list']

        else:
            return {
                    'valid_db': self.db_cache.keys(),
                    'valid_orb': self.orbs.keys(),
                    'error':'[%s] is not valid ' % flags
                    }

        return { 'error':'Problem with URL query'}


    def render_GET(self, uri):
        """
        Get query from client and parse arguments.
        """

        elog.debug('render_GET() uri.uri:%s' % uri.uri)

        try:
            (host,port) = uri.getHeader('host').split(':', 1)
        except:
            host = uri.getHeader('host')
            port = '-'

        if self.verbose:
            hostname = socket.gethostname()
            elog.notify("render_GET(): [%s] %s:%s%s" % (hostname,host,port,uri.uri))

        if self.debug:
            elog.debug('render_GET() uri.args:%s' % (uri.args) )
            elog.debug('render_GET() uri.prepath:%s' % (uri.prepath) )
            elog.debug('render_GET() uri.postpath:%s' % (uri.postpath) )
            elog.debug('render_GET() uri.path:%s' % (uri.path) )

            elog.debug('\tQUERY: %s ' % uri)
            elog.debug('\tHostname => [%s:%s]'% (host,port))
            elog.debug('\tHost=> [%s]'% uri.host)
            elog.debug('')

        d = defer.Deferred()
        d.addCallback( self._render_uri )
        reactor.callInThread(d.callback, uri)

        if self.debug: elog.debug("render_GET() - return server.NOT_DONE_YET")

        return server.NOT_DONE_YET

    def _render_uri(self,uri):
        """
        Get the uri arguments into a local structure.
        """

        if self.debug: elog.debug("_render_uri()")

        if self.loading:

            html =  { 'error':'Loading Server. Retry.'}
            return self._uri_results( uri, html ,error=True)


        db = False
        snet = False
        sta = False

        flags = {'db':False, 'snet':False,
                'sta':False, 'active':False,
                'decom':False, 'full':False,
                'clients':False, 'sources':True,
                'orb':False}

        for var in flags.keys():
            if var in uri.args:
                flags[var] = uri.args[var][0]

        return self._uri_results(uri,self._cache(flags))


    def _uri_results(self, uri=None, results=False, error=False):
        """
        Return data to client.
        """

        if not uri:
            elog.complain('No URI to work with on _uri_results()')
            return

        if self.debug:
            elog.debug('_uri_results  uri: %s results:%s' % (uri,results) )

        try:
            if error:
                uri.setHeader("content-type", "text/html")
                uri.setResponseCode( 500 )
                uri.write( pprint.pprint(results) )
            else:
                uri.setHeader("content-type", "application/json")
                expiry_time = datetime.utcnow() + timedelta(seconds=self.refresh)
                uri.setHeader("expires", expiry_time.strftime("%a, %d %b %Y %H:%M:%S GMT"))
                uri.write( json.dumps(results,indent=self.readableJSON) )

        except Exception,e:
            elog.complain('Exception: %s %s.' % (Exception,e) )
            uri.setHeader("content-type", "text/html")
            uri.setResponseCode( 500 )
            uri.write('Problem with server!')
            elog.complain( '_uri_results() Problem: Exception after :%s' % uri )

        try:
            uri.finish()
        except Exception,e:
            elog.complain('Exception: %s %s.' % (Exception,e) )
            elog.complain( '_uri_finish() Problem: Exception after :%s' % uri )

        if self.debug: elog.debug( '_uri_results() DONE!' )

    def _get_md5(self,file):
        """
        Get the checksum of a table
        """

        if self.debug: elog.debug('checksum [%s]' % file)

        if os.path.isfile( file ):
            return hashlib.md5( open(file).read() ).hexdigest()

        return False


    def _get_sensor(self, db, tempcache):

        if self.debug: elog.debug( "Stations(): dlsensor()")

        steps = [ 'dbopen dlsite', 'dbsort -u dlname ssident', 'dbjoin dlsensor ssident#dlident']

        if self.debug:
            elog.debug( ', '.join(steps) )

        steps.extend(['dbsort dlname snmodel dlsite.time'])

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                elog.complain( 'No records in dlsensor join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                (name,chident,snident,snmodel,time,endtime) = \
                    temp.getv('dlname','chident','snident','snmodel',
                            'dlsensor.time','dlsensor.endtime')

                snet,sta = name.split('_',1)
                time = int(time)
                endtime = int(endtime)

                try:
                    if not chident in tempcache[snet][sta]['sensor']:
                        tempcache[snet][sta]['sensor'][chident] = {}

                    if not snmodel in tempcache[snet][sta]['sensor'][chident]:
                        tempcache[snet][sta]['sensor'][chident][snmodel] = {}

                    if not snident in tempcache[snet][sta]['sensor'][chident][snmodel]:
                        tempcache[snet][sta]['sensor'][chident][snmodel][snident] = []

                    try:
                        if not len(tempcache[snet][sta]['sensor'][chident][snmodel][snident]): raise
                        original_list = []
                        for value in tempcache[snet][sta]['sensor'][chident][snmodel][snident]:

                            if value[0]-1 == endtime or value[0] == endtime:
                                value[0] = time
                                if self.debug: elog.debug( "update(%s) " % (value) )

                            if value[1]+1 == time or value[1] == time:
                                value[1] = endtime
                                if self.debug: elog.debug( "update(%s) " % (value) )

                            original_list.append(value)

                        tempcache[snet][sta]['sensor'][chident][snmodel][snident] = original_list

                    except Exception,e:
                        tempcache[snet][sta]['sensor'][chident][snmodel][snident].append( \
                                    [ time, endtime] )
                        if self.debug: elog.debug( "push(%s %s) " % (time,endtime) )

                except Exception,e:
                    pass

        return tempcache


    def _get_stabaler(self, db, tempcache):

        if self.debug: elog.debug( "_get_stabaler()")

        steps = [ 'dbopen stabaler']

        if self.debug:
            elog.debug( ', '.join(steps) )

        steps.extend(['dbsort dlsta time'])

        fields = ['nreg24','nreboot','firm','ssident']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                elog.complain( 'No records after stabler join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                snet = temp.getv('net')[0]
                sta = temp.getv('sta')[0]
                time = int(temp.getv('time')[0])
                touple = dict( zip(fields, temp.getv(*fields)) )

                try:
                    tempcache[snet][sta]['baler'][time] = touple
                    tempcache[snet][sta]['baler_ssident'] = touple['ssident']
                    tempcache[snet][sta]['baler_firm'] = touple['firm']

                    if self.debug: elog.debug( "baler(%s):%s" % (sta,time) )
                except:
                    pass

        return tempcache


    def _get_comm(self, db, tempcache):

        if self.debug: elog.debug( "_get_comm()")

        steps = [ 'dbopen comm']

        if self.debug:
            elog.debug( ', '.join(steps) )

        steps.extend(['dbsort sta time'])

        fields = ['time','endtime','commtype','provider']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                elog.complain( 'No records in %s after comm join' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                sta = temp.getv('sta')[0]
                results = dict( zip(fields, temp.getv(*fields)) )
                results['time'] = int(results['time'])
                results['endtime'] = int(results['endtime'])
                for snet in tempcache:
                    try:
                        tempcache[snet][sta]['comm'].append( results )
                        if self.debug: elog.debug( "comm(%s)" % sta )
                    except:
                        pass

        return tempcache



    def _get_dlsite(self, db, tempcache):

        if self.debug: elog.debug( "_get_dlsite()" )

        steps = [ 'dbopen dlsite']

        steps.extend(['dbsort ssident time'])

        if self.debug:
            elog.debug( ', '.join(steps) )

        fields = ['model','time','endtime','idtag']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                elog.complain( 'No records in after dlsite join %s' %
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():

                snet,sta = temp.getv('dlname')[0].split('_',1)
                ssident = temp.getv('ssident')[0]

                dl = dict( zip(fields, temp.getv(*fields)) )

                try:
                    if ssident in tempcache[snet][sta]['datalogger']:
                        tempcache[snet][sta]['datalogger'][ssident]['endtime'] = \
                                dl['endtime']
                    else:
                        tempcache[snet][sta]['datalogger'][ssident] = dl

                    if self.debug: elog.debug( "_get_dlsite(%s_%s)" % (snet,sta) )
                except Exception,e:
                    #elog.complain("#### No deployment entry for %s_%s %s %s" % \
                    #        (snet,sta,Exception,e) )
                    pass


        return tempcache


    def _get_deployment_list(self,db):

        tempcache = { 'full':{}, 'active':{}, 'decom':{}, 'list':{} }

        steps = [ 'dbopen deployment', 'dbjoin -o site']

        if self.sta_subset:
            steps.extend(["dbsubset %s" % self.sta_subset])

        steps.extend(['dbsort snet sta'])

        if self.debug:
            elog.debug( ', '.join(steps) )

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                elog.complain( 'No records after deployment-site join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():

                fields = ['vnet','snet','sta','time','endtime','equip_install',
                    'equip_remove', 'cert_time','decert_time','pdcc',
                    'lat','lon','elev','staname','statype']

                db_v = dict( zip(fields, temp.getv(*fields)) )

                for k in db_v:
                    try:
                        if abs(int(db_v[k])) == 9999999999:
                            db_v[k] = '-'
                    except:
                        pass

                try:
                    strtime = stock.epoch2str(db_v['time'],
                            self.timeformat, self.timezone)
                except:
                    strtime = '-'

                try:
                    strendtime = stock.epoch2str(db_v['endtime'],
                            self.timeformat, self.timezone)
                except:
                    strendtime = '-'

                sta = db_v['sta']
                snet = db_v['snet']
                endtime = db_v['endtime']

                if self.debug: elog.debug( "_get_deployment_list(%s_%s)" % (snet,sta) )

                if not snet in tempcache['full']:
                    tempcache['full'][snet] = {}
                    tempcache['decom'][snet] = {}
                    tempcache['active'][snet] = {}
                    tempcache['list'][snet] = {}

                if not sta in tempcache['full'][snet]:
                    tempcache['full'][snet][sta] = {} 
                tempcache['full'][snet][sta] = db_v

                tempcache['full'][snet][sta]['datalogger'] = {}
                tempcache['full'][snet][sta]['sensor'] = {}
                tempcache['full'][snet][sta]['baler'] = {}
                tempcache['full'][snet][sta]['comm'] = []

                tempcache['list'][snet][sta] = []

                try:
                    if int(endtime) < stock.now():
                        tempcache['decom'][snet][sta] = []
                    else:
                        raise
                except:
                    tempcache['active'][snet][sta] = []

        return tempcache


    def _get_sta_cache(self,database):
        """
        Private function to load the data from the tables
        """

        if self.debug:
            elog.debug( "Using approx. %0.1f MB of memory" % self._memory_usage_resource() )

        tempcache = {}

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

        if need_update:

            tempcache[database] = self._get_deployment_list(db)
            tempcache[database]['full'] = self._get_dlsite(db,
                    tempcache[database]['full'] )
            tempcache[database]['full'] = self._get_comm(db,
                    tempcache[database]['full'] )
            tempcache[database]['full'] = self._get_sensor(db,
                    tempcache[database]['full'] )
            tempcache[database]['full'] = self._get_stabaler(db,
                    tempcache[database]['full'] )

            # Flatten those dictionaries
            for cache in ['decom','active','list']:
                for snet in tempcache[database][cache].keys():
                    tempcache[database][cache][snet] = \
                            tempcache[database][cache][snet].keys()

            self.db_cache[database] = tempcache[database]

            if self.debug: elog.debug( "Completed updating db. (%s)" % database )


        if self.debug: elog.debug( "Schedule update in (%s) seconds" % self.refresh )
        reactor.callLater(self.refresh, self._get_sta_cache, database )

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


resource = Stations()
