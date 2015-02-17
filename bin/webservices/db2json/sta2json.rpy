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
import socket
import pprint
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
                'timeformat':{'type':'str','default':'%d (%j) %h:%m:%s %z'},
                'timezone':{'type':'str','default':'utc'},
                'sta_subset':{'type':'str','default':False},
                'refresh':{'type':'int','default':60},
                'databases':{'type':'dict','default':{}},
                'orbnames':{'type':'dict','default':{}},
                'readablejson':{'type':'int','default':0}
                }

        self._read_pf()

        if not self.refresh:
            self.refresh = 60 # every minute default


        # Check DATABASES
        for name,path in self.databases.iteritems():
            try:
                self.dbs[name] = {}
                self.dbs[name] = { 'tables':{} }
                for table in self.tables:
                    present = test_table(path,table)
                    if not present:
                        raise sta2jsonException('Empty or missing %s.%s' % (path,table) )

                    self.dbs[name]['tables'][table] = { 'path':present, 'md5':False }

                db = datascope.dbopen( path , 'r' )
                self.dbs[name]['db'] = db
                self.dbs[name]['path'] = path
                self._log( "init %s DB: %s" % (name,path) )

                deferToThread(self._get_sta_cache,name)
            except Exception,e:
                raise sta2jsonException( 'Problems on configured dbs: %s' % e )

        # Check ORBS
        for name,orbname in self.orbnames.iteritems():
            self._log( "init %s ORB: %s" % (name,orbname) )

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


        self._log( 'Done loading Stations()' )

    def _log(self,msg):
        if self.verbose:
            elog.notify( 'sta2json: %s' % msg )

    def _complain(self,msg):
        elog.complain( 'sta2json: PROBLEM: %s' % msg )


    def _read_pf(self):
        """
        Read configuration parameters from rtwebserver pf file.
        """

        elog.notify( 'Read parameters from pf file')

        module = 'sta2jsonconfig'

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

    def _get_orb_cache(self, name):

        self._log( 'Check ORB(%s) sources' % name)

        pkt = Pkt.Packet()

        try:
            self._log("connect to orb(%s)" % name )
            self.orbs[name]['orb'].connect()
        except Exception,e:
            self.orbs[name]['info']['status'] = e
            self._complain('Cannot connect ORB [%s]: %s' % (orbname,e) )
        else:
            self.orbs[name]['info']['status'] = 'online'
            self.orbs[name]['info']['last_check'] = stock.now()
            try:
                # get clients
                self._log("get clients orb(%s)" % name )
                result = self.orbs[name]['orb'].clients()

                for r in result:
                    if isinstance(r,float):
                        self.orbs[name]['info']['clients_time'] = r
                        self._log("orb(%s) client time %s" % (name, r) )
                    else:
                        self.orbs[name]['clients'] = r
            except Exception,e:
                self._complain("Cannot query orb(%s) %s %s" % (name, Exception, e) )

            try:
                # get sources
                self._log("get sources orb(%s)" % name )
                result = self.orbs[name]['orb'].sources()

                for r in result:
                    if isinstance(r,float):
                        self.orbs[name]['info']['sources_time'] = r
                        self._log("orb(%s) sources time %s" % (name, r) )
                    else:
                        for stash in r:

                            srcname = stash['srcname']
                            pkt.srcname = Pkt.SrcName(srcname)
                            net = pkt.srcname.net
                            sta = pkt.srcname.sta

                            del stash['srcname']

                            self._log("orb(%s) update %s %s" % (name,net,sta) )

                            if not net in self.orbs[name]['sources']:
                                self.orbs[name]['sources'][net] = {}

                            if not sta in self.orbs[name]['sources'][net]:
                                self.orbs[name]['sources'][net][sta] = {}

                            self.orbs[name]['sources'][net][sta][srcname] = stash

                            try:
                                if not 'orb' in self.db_cache[name]['active'][net][sta]:
                                    self.db_cache[name]['active'][net][sta]['orb'] = {}
                                self.db_cache[name]['active'][net][sta]['orb'][srcname] = \
                                        stash['slatest_time']
                            except:
                                pass
            except Exception,e:
                self._complain("Cannot query orb(%s) %s %s" % (name, Exception, e) )

        self.orbs[name]['orb'].close()

        reactor.callLater(self.refresh, self._get_orb_cache, name )


    def _cache(self, flags):
        """
        Return cached data for query response.
        """

        self._log('_cache()')

        if flags['orb'] in self.orbs:

            info = {}
            clients = {}
            sources = {}
            orb = flags['orb']

            try:
                return {
                        'info': self.orbs[orb]['info'],
                        'clients': self.orbs[orb]['clients'],
                        'sources': self.orbs[orb]['sources']
                        }
            except:
                return { 'info': [], 'clients':[], 'sources':[] }


        elif flags['db'] in self.db_cache:

            results = {}
            db = flags['db']
            snet = flags['snet']
            sta = flags['sta']

            if test_yesno( flags['active'] ):
                temp = self.db_cache[db]['active']

            elif test_yesno( flags['decom'] ):
                temp = self.db_cache[db]['decom']

            else:
                temp = dict_merge(self.db_cache[db]['decom'],
                        self.db_cache[db]['active'] )


            if snet and sta:

                try:
                    return { snet: { sta: temp[snet][sta] } }
                except:
                    pass

            elif snet:

                try:
                    return { snet: temp[snet] }
                except:
                    pass

            return temp

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

        self._log('render_GET() uri.uri:%s' % uri.uri)

        try:
            (host,port) = uri.getHeader('host').split(':', 1)
        except:
            host = uri.getHeader('host')
            port = '-'

        hostname = socket.gethostname()
        self._log("render_GET(): [%s] %s:%s%s" % (hostname,host,port,uri.uri))

        #elog.debug('render_GET() uri.args:%s' % (uri.args) )
        #elog.debug('render_GET() uri.prepath:%s' % (uri.prepath) )
        #elog.debug('render_GET() uri.postpath:%s' % (uri.postpath) )
        #elog.debug('render_GET() uri.path:%s' % (uri.path) )

        #elog.debug('\tQUERY: %s ' % uri)
        #elog.debug('\tHostname => [%s:%s]'% (host,port))
        #elog.debug('\tHost=> [%s]'% uri.host)
        #elog.debug('')

        d = defer.Deferred()
        d.addCallback( self._render_uri )
        reactor.callInThread(d.callback, uri)

        self._log("render_GET() - return server.NOT_DONE_YET")

        return server.NOT_DONE_YET

    def _render_uri(self,uri):
        """
        Get the uri arguments into a local structure.
        """

        self._log("_render_uri()")

        if self.loading:

            html =  { 'error':'Loading Server. Retry.'}
            return self._uri_results( uri, html ,error=True)


        db = False
        snet = False
        sta = False

        flags = {'db':False, 'snet':False,
                'sta':False, 'active':False,
                'decom':False, 'orb':False }

        for var in flags.keys():
            if var in uri.args:
                flags[var] = uri.args[var][0]

        return self._uri_results(uri,self._cache(flags))


    def _uri_results(self, uri=None, results=False, error=False):
        """
        Return data to client.
        """

        if not uri:
            self._complain('No URI to work with on _uri_results()')
            return

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
            self._complain('Exception: %s %s.' % (Exception,e) )
            uri.setHeader("content-type", "text/html")
            uri.setResponseCode( 500 )
            uri.write('Problem with server!')
            self._complain( '_uri_results() Problem: Exception after :%s' % uri )

        try:
            uri.finish()
        except Exception,e:
            self._complain('Exception: %s %s.' % (Exception,e) )
            self._complain( '_uri_finish() Problem: Exception after :%s' % uri )

        self._log( '_uri_results() DONE!' )


    def _get_sensor(self, db, tempcache):

        self._log( "Stations(): dlsensor()")

        steps = [ 'dbopen dlsite', 'dbsort -u dlname ssident', 'dbjoin dlsensor ssident#dlident']

        self._log( ', '.join(steps) )

        steps.extend(['dbsort dlname snmodel dlsite.time'])

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                self._complain( 'No records in dlsensor join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                (name,chident,snident,snmodel,time,endtime) = \
                    temp.getv('dlname','chident','snident','snmodel',
                            'dlsensor.time','dlsensor.endtime')

                snet,sta = name.split('_',1)
                time = int(time)
                endtime = int(endtime)

                status = find_status(tempcache,sta)
                if not status: continue

                try:
                    if not chident in tempcache[status][snet][sta]['sensor']:
                        tempcache[status][snet][sta]['sensor'][chident] = {}

                    if not snmodel in tempcache[status][snet][sta]['sensor'][chident]:
                        tempcache[status][snet][sta]['sensor'][chident][snmodel] = {}

                    if not snident in tempcache[status][snet][sta]['sensor'][chident][snmodel]:
                        tempcache[status][snet][sta]['sensor'][chident][snmodel][snident] = []

                    try:
                        if not len(tempcache[status][snet][sta]['sensor'][chident][snmodel][snident]): raise
                        original_list = []
                        for value in tempcache[status][snet][sta]['sensor'][chident][snmodel][snident]:

                            if value[0]-1 == endtime or value[0] == endtime:
                                value[0] = time
                                self._log( "update(%s) " % (value) )

                            if value[1]+1 == time or value[1] == time:
                                value[1] = endtime
                                self._log( "update(%s) " % (value) )

                            original_list.append(value)

                        tempcache[status][snet][sta]['sensor'][chident][snmodel][snident] = original_list

                    except Exception,e:
                        tempcache[status][snet][sta]['sensor'][chident][snmodel][snident].append( \
                                    [ time, endtime] )
                        self._log( "push(%s %s) " % (time,endtime) )

                except Exception,e:
                    pass

        return tempcache


    def _get_stabaler(self, db, tempcache):

        self._log( "_get_stabaler()")

        steps = [ 'dbopen stabaler']

        self._log( ', '.join(steps) )

        steps.extend(['dbsort dlsta time'])

        fields = ['nreg24','nreboot','firm','ssident']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                self._complain( 'No records after stabler join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                snet = temp.getv('net')[0]
                sta = temp.getv('sta')[0]
                time = int(temp.getv('time')[0])
                touple = dict( zip(fields, temp.getv(*fields)) )

                status = find_status(tempcache,sta)
                if not status: continue

                try:
                    tempcache[status][snet][sta]['baler'][time] = touple
                    tempcache[status][snet][sta]['baler_ssident'] = touple['ssident']
                    tempcache[status][snet][sta]['baler_firm'] = touple['firm']

                    self._log("baler(%s):%s" % (sta,time) )
                except:
                    pass

        return tempcache


    def _get_comm(self, db, tempcache):

        self._log( "_get_comm()")

        steps = [ 'dbopen comm']

        self._log( ', '.join(steps) )

        steps.extend(['dbsort sta time'])

        fields = ['time','endtime','commtype','provider']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                self._complain( 'No records in %s after comm join' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():
                sta = temp.getv('sta')[0]

                results = dict( zip(fields, temp.getv(*fields)) )
                results['time'] = int(results['time'])
                results['endtime'] = int(results['endtime'])

                status = find_status(tempcache,sta)
                if not status: continue
                snet = find_snet(tempcache,sta)
                if not snet: continue


                tempcache[status][snet][sta]['comm'].append( results )

        return tempcache



    def _get_dlsite(self, db, tempcache):

        self._log( "_get_dlsite()" )

        steps = [ 'dbopen dlsite']

        steps.extend(['dbsort ssident time'])

        self._log( ', '.join(steps) )

        fields = ['model','time','endtime','idtag']

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                self._log( 'No records in after dlsite join %s' %
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():

                snet,sta = temp.getv('dlname')[0].split('_',1)
                ssident = temp.getv('ssident')[0]

                dl = dict( zip(fields, temp.getv(*fields)) )

                status = find_status(tempcache,sta)
                if not status: continue

                try:
                    if ssident in tempcache[status][snet][sta]['datalogger']:
                        tempcache[status][snet][sta]['datalogger'][ssident]['endtime'] = \
                                dl['endtime']
                    else:
                        tempcache[status][snet][sta]['datalogger'][ssident] = dl

                    self._log( "_get_dlsite(%s_%s)" % (snet,sta) )
                except Exception,e:
                    #self._log("#### No deployment entry for %s_%s %s %s" % \
                    #        (snet,sta,Exception,e) )
                    pass


        return tempcache


    def _get_deployment_list(self,db):

        tempcache = { 'active':{}, 'decom':{}, 'list':{} }

        steps = [ 'dbopen deployment', 'dbjoin -o site']

        if self.sta_subset:
            steps.extend(["dbsubset %s" % self.sta_subset])

        steps.extend(['dbsort snet sta'])

        self._log( ', '.join(steps) )

        with datascope.freeing(db.process( steps )) as dbview:
            if not dbview.record_count:
                self._complain( 'No records after deployment-site join %s' % \
                        db.query(datascope.dbDATABASE_NAME) )
                return tempcache

            for temp in dbview.iter_record():

                fields = ['vnet','snet','sta','time','endtime','equip_install',
                    'equip_remove', 'cert_time','decert_time','pdcc',
                    'lat','lon','elev','staname','statype']

                db_v = dict( zip(fields, temp.getv(*fields)) )

                sta = db_v['sta']
                snet = db_v['snet']

                self._log( "_get_deployment_list(%s_%s)" % (snet,sta) )

                for k in db_v:
                    try:
                        if abs(int(db_v[k])) == 9999999999:
                            db_v[k] = '-'
                    except:
                        pass

                try:
                    db_v['strtime'] = stock.epoch2str(db_v['time'],
                            self.timeformat, self.timezone)
                except:
                    db_v['strtime'] = '-'

                try:
                    db_v['strendtime'] = stock.epoch2str(db_v['endtime'],
                            self.timeformat, self.timezone)
                except:
                    db_v['strendtime'] = '-'


                if not snet in tempcache['active']:
                    tempcache['decom'][snet] = {}
                    tempcache['active'][snet] = {}

                if not db_v['endtime'] is '-' and db_v['endtime'] < stock.now():
                    tempcache['decom'][snet][sta] = db_v
                    tempcache['decom'][snet][sta]['datalogger'] = {}
                    tempcache['decom'][snet][sta]['sensor'] = {}
                    tempcache['decom'][snet][sta]['baler'] = {}
                    tempcache['decom'][snet][sta]['comm'] = []
                else:
                    tempcache['active'][snet][sta] = db_v
                    tempcache['active'][snet][sta]['datalogger'] = {}
                    tempcache['active'][snet][sta]['sensor'] = {}
                    tempcache['active'][snet][sta]['baler'] = {}
                    tempcache['active'][snet][sta]['comm'] = []

        return tempcache


    def _get_sta_cache(self,database):
        """
        Private function to load the data from the tables
        """

        self._log( "deferToThread( sta_cache => %s)" % database )

        tempcache = {}

        db = self.dbs[database]['db']
        dbpath = self.dbs[database]['path']
        need_update = False
        self._log( "(%s) path:%s" % (database,dbpath) )

        for name in self.tables:

            path = self.dbs[database]['tables'][name]['path']
            md5 = self.dbs[database]['tables'][name]['md5']

            test = get_md5(path)

            self._log('(%s) table:%s path:%s md5:[old: %s new: %s]' % \
                        (database,name,path,md5,test) )

            if test != md5:
                self._log('Update needed.')
                self.dbs[database]['tables'][name]['md5'] = test
                need_update = True

        if need_update:

            tempcache[database] = self._get_deployment_list(db)
            tempcache[database] = self._get_dlsite(db, tempcache[database] )
            tempcache[database] = self._get_comm(db, tempcache[database] )
            tempcache[database] = self._get_sensor(db, tempcache[database] )
            tempcache[database] = self._get_stabaler(db, tempcache[database] )

            self.db_cache[database] = tempcache[database]

            self._log( "Completed updating db. (%s)" % database )


        self._log( "Schedule update in (%s) seconds" % self.refresh )
        reactor.callLater(self.refresh, self._get_sta_cache, database )


resource = Stations()
