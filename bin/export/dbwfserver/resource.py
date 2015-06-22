import os, sys
import socket
import twisted.web.resource
import twisted.internet.defer
import twisted.internet.reactor
import twisted.web.static
from twisted.internet.threads import deferToThread
from dbcentral import Dbcentral
import antelope.stock as stock
import twisted.web.server
import json
from string import Template
import logging
from dbwfserver.util import isNumber, Events, Stations, load_template
from textwrap import dedent

"""
Twisted.web resources for use by the dbwfserver application

FaviconResource is a thin wrapper around a static File resource.

QueryParserResource is the main resource that runs at the root of the
dbwfserver application
"""

class FaviconResource(twisted.web.static.File):
    """
    Serve up a favicon from the static content directory
    """

    def __init__(self,config):
        twisted.web.static.File.__init__(
            self,
            os.path.join(config.static_dir, 'images/favicon.ico'),
            defaultType='image/vnd.microsoft.icon')

class QueryParserResource(twisted.web.resource.Resource):
    """
    Serve http queries. Functions as the root resource of the dbwfserver Site
    """

    isLeaf = False

    allowedMethods = ("GET")

    def __init__(self,config,dbname):

        self.logger=logging.getLogger(__name__)

        self.logger.info('########################')
        self.logger.info('        Loading!        ')
        self.logger.info('########################')

        self.init_finished=False
        self.init_failure=False

        self.config = config
        self.dbname = dbname
        self.loading_stations = True
        self.loading_events = True

        self.root_template = load_template(self.config.template)
        self.plot_template = load_template(self.config.plot_template)

        #
        # Initialize Classes
        #
        self.logger.debug('QueryParser(): Init DB: Load class twisted.web.resource.Resource.__init__(self)')
        twisted.web.resource.Resource.__init__(self)


        #
        # Open db using Dbcentral CLASS
        #
        self.logger.debug("QueryParser(): Init DB: Create Dbcentral object with database(%s)." % self.dbname)
        self.dbcentral = Dbcentral(self.dbname,
                            self.config.nickname,
                            self.config.debug,
                            ['wfdisc','sitechan']
                           )
        if self.config.debug: self.dbcentral.info()

        if not self.dbcentral.list():
            self.logger.critical('Init DB: No databases to use! (%s)' %
                                 self.dbname)
            sys.exit(twisted.internet.reactor.stop())


        self.tvals = {
            "filters": '<option value="None">None</option>',
            "dbname": self.dbname,
            "display_arrivals": '',
            "display_points": '',
            "proxy_url": self.config.proxy_url,
            "dbname": self.dbname,
            "application_title": self.config.application_title,
        }

        for filter in self.config.filters:
            self.tvals['filters'] += '<option value='+filter.replace(' ','_')+'>'
            self.tvals['filters'] += filter
            self.tvals['filters'] += '</option>'

        if self.config.event == 'true' and self.config.display_arrivals:
            self.tvals['display_arrivals'] = 'checked="checked"'

        if self.config.display_points:
            self.tvals['display_points'] = 'checked="checked"'

        if not self.dbcentral.list():
            self.logger.critical(
                'No valid databases to work with! -v or -V for more info')
            return False

        d=deferToThread(self._init_in_thread)
        d.addCallback(self._init_finished)
        d.addErrback(self._init_failed)

    def _init_finished(self,d):
        self.init_finished=True

    def _init_failed(self,failure):
        self.init_failure=True
        self.logger.critical('An error occurred during initialization: ' +
                             str(failure))
        sys.exit(twisted.internet.reactor.stop())

    def _init_in_thread(self):

        self.logger.info('Loading Stations()')
        self.stations = Stations(self.config,self.dbcentral)
        self.loading_stations = False
        self.logger.info('Done loading Stations()')

        if self.config.event == 'true':
            self.logger.info('Loading Events()')
            self.events = Events(self.dbcentral)
            self.loading_events = False
            self.logger.info('Done loading Events()')
        else:
            self.loading_events = False

        self.logger.info('READY!')


    def getChild(self, name, request):

        #self.logger.debug("getChild(): name:%s request:%s" % (name,request))
        return self

    def _render_loading(self, request):
        """Output an error page while loading Stations and Events"""

        responseCode = 500
        #html =  "<html><head><title>%s</title></head><body><h1>DBWFSERVER:</h1></br><h3>Server Loading!</h3></br>" % self.config.application_title
        #html +=  "<p>Waiting for Stations: %s</p></br>" % self.loading_stations
        #html +=  "<p>Waiting for Events: %s</p></br>" % self.loading_events
        #html +=  "</body></html>"

        html_template=Template(dedent(
            """\
            <html>
                <head><title>ERROR: $responseCode - $appname</title></head>
                <body>
                    <h1>ERROR: $responseCode - $appname</h1>
                    <p>The DBWFSERVER $appname is still starting up.</p>
                    <p>Waiting for Stations: $loading_stations</p>
                    <p>Waiting for Events: $loading_events</p>
                </body>
            </html>
            """))

        html = html_template.substitute(
            appname          = self.config.application_title,
            responseCode     = responseCode,
            loading_stations = self.loading_stations,
            loading_events   = self.loading_events,
        )


        request.setHeader("content-type", "text/html")
        request.setResponseCode( responseCode )
        self.logger.debug("_render_loading returning: \n" + html)
        return html

    def render_GET(self, request):

        self.logger.debug('QueryParser(): render_GET(%s)' % request)
        self.logger.debug('QueryParser(): render_GET() request.uri:%s' % request.uri)
        self.logger.debug('QueryParser(): render_GET() request.args:%s' % (request.args) )
        self.logger.debug('QueryParser(): render_GET() request.prepath:%s' % (request.prepath) )
        self.logger.debug('QueryParser(): render_GET() request.postpath:%s' % (request.postpath) )
        self.logger.debug('QueryParser(): render_GET() request.path:%s' % (request.path) )

        if self.loading_stations or self.loading_events:
            return self._render_loading(request)


        (host,port) = request.getHeader('host').split(':', 1)
        self.logger.debug('QueryParser():\tQUERY: %s ' % request)
        self.logger.debug('QueryParser():\tHostname => [%s:%s]'% (host,port))
        self.logger.debug('QueryParser():\tHost=> [%s]'% request.host)
        self.logger.debug('QueryParser():\tsocket.gethostname() => [%s]'% socket.gethostname())
        self.logger.debug('')
        #self.logger.debug('QueryParser():\tsocket.getsockname() => [%s]'% request.host.getsockname())
        #request.setHost(host,self.config.port)


        d = twisted.internet.defer.Deferred()
        d.addCallback( self.render_uri )
        twisted.internet.reactor.callInThread(d.callback, request)

        self.logger.debug("QueryParser(): render_GET() - return server.NOT_DONE_YET")

        return twisted.web.server.NOT_DONE_YET



    def render_uri(self,request):

        #
        # Clean and prep vars
        #
        response_data = {}
        response_meta = {}

        response_meta.update( {
            "error":      'false',
            "setupEvents": self.config.event,
            "setupUI":    'false',
            "realtime":   self.config.realtime,
            #"proxy_url":  self.config.proxy_url,
            "style":      self.config.style,
            "meta_query": "false"
        } )

        #if self.config.proxy_url: response_meta['proxy'] = "'" + self.config.proxy_url + "'"

        #
        # remove all empty  elements
        # This (localhost:8008/stations/) is the same as # (localhost:8008/stations)
        #
        path = request.prepath
        while True:
            try:
                path.remove('')
            except:
                break


        # Parse all elements on the list
        query = self._parse_request(path)

        if 'precision' in request.args:
            query.update( { "precision":int(request.args['precision'][0]) })
        else:
            query.update( { "precision":1} )

        if 'period' in request.args:
            query.update( { "period":int(request.args['period'][0]) })
        else:
            query.update( { "period":0} )

        if 'median' in request.args:
            test = request.args['median'][0]
            if test.lower() in ("yes", "true", "t", "1"):
                query.update( { "median":1 } )
            else:
                query.update( { "median":0 } )
        else:
            query.update( { "median":0 } )

        if 'realtime' in request.args:
            test = request.args['realtime'][0]
            if test.lower() in ("yes", "true", "t", "1"):
                query.update( { "realtime":1 } )
            else:
                query.update( { "realtime":0 } )
        else:
            query.update( { "realtime":0 } )


        if 'filter' in request.args:
            filter = request.args['filter'][0]
            query.update( { "filter":filter.replace('_',' ') } )
        else:
            query.update( { "filter":'None' } )


        if 'calibrate' in request.args:
            test = request.args['calibrate'][0]
            if test.lower() in ("yes", "true", "t", "1"):
                query.update( { "calibrate":1 } )
            else:
                query.update( { "calibrate":0 } )
        else:
            request.args.update( { "calibrate":[self.config.apply_calib] } )
            query.update( { "calibrate":self.config.apply_calib } )


        self.logger.debug('QueryParser(): render_uri() request.prepath => path(%s)[%s]' % (len(path),path) )
        self.logger.debug('QueryParser(): render_uri() query => [%s]' % query)

        if query['data']:


                self.logger.debug('QueryParser(): render_uri() "data" query')

                if len(path) == 0:
                    #ERROR: we need one option
                    self.logger.error('QueryParser(): render_uri() ERROR: Empty "data" query!')
                    return self.uri_results(request,'Invalid data query.')


                elif path[0] == 'events':

                    """
                    Return events dictionary as JSON objects. For client ajax calls.
                    Called with or without argument.
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => events')
                    if self.config.event != 'true':
                        return self.uri_results(request,{})

                    elif len(path) == 2:
                        return self.uri_results( request, self.events(path[1]) )

                    elif len(path) == 3:
                        return self.uri_results( request, self.events.phases(path[1],path[2]) )

                    else:
                        return self.uri_results(request,self.events.table() )


                elif path[0] == 'dates':

                    """
                    Return list of yearday values for time in db
                    for all wfs in the cluster of dbs.
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => dates')

                    return self.uri_results( request, self.stations.dates() )



                elif path[0] == 'stadates':

                    """
                    Return list of yearday values for time in db
                    for all stations in the cluster of dbs.
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => dates')

                    if len(path) == 2:
                        return self.uri_results( request, self.stations.stadates(path[1]) )

                    if len(path) == 3:
                        return self.uri_results( request, self.stations.stadates(path[1],path[2]) )

                    return self.uri_results( request, self.stations.stadates() )



                elif path[0] == 'stations':

                    """
                    Return station list as JSON objects. For client ajax calls.
                    Called with argument return dictionary
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => stations')

                    if len(path) == 2:
                        return self.uri_results( request, self.stations(path[1]) )

                    return self.uri_results( request, self.stations.list() )


                elif path[0] == 'channels':

                    """
                    Return channels list as JSON objects. For client ajax calls.
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => channels')

                    if len(path) == 2:
                        stas = self.stations.convert_sta(path[1].split('|'))
                        return self.uri_results( request, self.stations.channels( stas ) )

                    return self.uri_results( request, self.stations.channels() )


                elif path[0] == 'now':

                    """
                    Return JSON object for epoch(now).
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => now')

                    return self.uri_results( request, [stock.now()] )


                elif path[0] == 'filters':

                    """
                    Return list of filters as JSON objects. For client ajax calls.
                    """

                    self.logger.debug('QueryParser(): render_uri() query => data => filters %s' % self.config.filters)

                    return self.uri_results( request, self.config.filters )


                elif path[0] == 'wf':

                    """
                    Return JSON object of data. For client ajax calls.
                    """

                    self.logger.debug(
                        "QueryParser(): render_uri(): get_data(%s))" % query)

                    return self.uri_results( request, self.get_data(query) )



                elif path[0] == 'coverage':

                    """
                    Return coverage tuples as JSON objects. For client ajax calls.
                    """
                    self.logger.debug("QueryParser(): render_uri(): Get coverage")

                    query.update( { "coverage": 1 } )

                    return self.uri_results( request, self.get_data(query) )



                else:
                    #ERROR: Unknown query type.
                    return self.uri_results( request, "Unknown query type:(%s)" % path )




        response_meta.update(self.tvals)

        if not path:
            return  self.uri_results(
                request,
                self.root_template.safe_substitute(response_meta)
            )

        response_meta['meta_query'] = {}
        response_meta['meta_query']['sta'] = query['sta']
        response_meta['meta_query']['chan'] = query['chan']
        response_meta['meta_query']['time_start'] = query['start']
        response_meta['meta_query']['time_end'] = query['end']
        response_meta['meta_query']['page'] = query['page']

        if request.args:
            response_meta['setupUI'] = json.dumps(request.args)

        response_meta['meta_query'] = json.dumps( response_meta['meta_query'] )

        if path[0] == 'wf':
            return  self.uri_results(
                request,
                self.root_template.safe_substitute(response_meta)
            )

        elif path[0] == 'plot':
            return  self.uri_results(
                request,
                self.plot_template.safe_substitute(response_meta)
            )

        return self.uri_results( request, "Invalid query."  )


    def _parse_request(self,args):

        """
        Strict format for uri:
            localhost/wf/sta/chan/time/time/page

            Data-only calls:
            localhost/data/wf
            localhost/data/times
            localhost/data/events
            localhost/data/filters
            localhost/data/stations
            localhost/data/coverage
            localhost/data/channels
            localhost/data/wf/sta/chan/time/time/page
        """
        self.logger.debug("QueryParser(): _parse_request(): URI: %s" % str(args) )

        uri = {}
        time_window = float(self.config.default_time_window)

        uri.update( {
            "sta":[],
            "chan":[],
            "end":0,
            "data":False,
            "start":0,
            "page":1,
            "coverage":0,
            "time_window":False
        } )

        if 'data' in args:
            self.logger.info("QueryParser() _parse_request(): data query!")
            uri['data'] = True
            args.remove('data')

        # localhost/sta
        if len(args) > 1:
            uri['sta'] = args[1]

        # localhost/sta/chan
        if len(args) > 2:
            uri['chan'] = args[2]

        # localhost/sta/chan/time
        if len(args) > 3:
            uri['start'] = args[3]

        # localhost/sta/chan/time/time
        if len(args) > 4:
            uri['end'] = args[4]

        # localhost/sta/chan/time/time/page
        if len(args) > 5:
            uri['page'] = args[5]

        #
        # Fix start
        #
        if uri['start']:
            if isNumber(uri['start']):
                uri['start'] = isNumber(uri['start'])
            elif uri['start'] == 'hour':
                uri['start'] = 0
                time_window = 3600
            elif uri['start'] == 'day':
                uri['start'] = 0
                time_window = 86400
            elif uri['start'] == 'week':
                uri['start'] = 0
                time_window = 604800
            elif uri['start'] == 'month':
                uri['start'] = 0
                time_window = 2629743
            else:
                uri['start'] = 0

        #
        # Fix end
        #
        if uri['end']:
            if isNumber(uri['end']):
                uri['end'] = isNumber(uri['end'])
            elif uri['end'] == 'hour':
                uri['end'] = 0
                time_window = 3600
            elif uri['end'] == 'day':
                uri['end'] = 0
                time_window = 86400
            elif uri['end'] == 'week':
                uri['end'] = 0
                time_window = 604800
            elif uri['end'] == 'month':
                uri['end'] = 0
                time_window = 2629743
            else:
                uri['end'] = 0

        #
        # Build missing times if needed
        #
        if uri['sta'] and uri['chan']:
            if not uri['start'] :
                uri['end'] = self.stations.max_time()
                uri['start'] = uri['end'] - time_window

                uri['end']   = isNumber(uri['end'])
                uri['start'] = isNumber(uri['start'])

            if not uri['end']:
                uri['end'] = uri['start'] + time_window

        self.logger.debug("QueryParser(): _parse_request(): [sta:%s chan:%s start:%s end:%s]" % (uri['sta'], uri['chan'], uri['start'], uri['end']) )

        return uri

    def uri_results(self, uri=None, results=False):
        self.logger.info('QueryParser(): uri_results(%s,%s)' % (uri,type(results)))

        if uri:

            if results != False:
                if type(results).__name__ == 'list' or type(results).__name__ == 'dict':
                    uri.setHeader("content-type", "application/json")
                    uri.write(json.dumps(results))
                else:
                    uri.setHeader("content-type", "text/html")
                    uri.write(results)
            else:
                uri.setHeader("content-type", "text/html")
                uri.setResponseCode( 500 )
                uri.write('Problem with server!')

            try:
                uri.finish()
            except:
                pass

        self.logger.info('QueryParser(): uri_results() DONE!')

    def get_data(self,query):
        #
        # Return points or bins of data for query
        #

        response_data = ""

        self.logger.debug("QueryParser(): get_data(): Build COMMAND")

        self.logger.debug("QueryParser(): get_data(): Get data for uri:%s.%s" % (
            query['sta'],query['chan']))

        if not query['sta']:
            response_data = "Not valid station value"
            self.logger.error(response_data)
            return { "ERROR": response_data }

        if not query['chan']:
            response_data = "Not valid channel value "
            self.logger.error(response_data)
            return { "ERROR": response_data }

        start = isNumber(query['start'])
        end   = isNumber(query['end'])

        if not start:
            temp_dic = self.stations(query['sta'][0])
            if temp_dic:
                start = temp_dic[query['chan'][0]]['end'] - \
                            self.config.default_time_window

        #if not start: start = stock.now()

        if not end: end = start + self.config.default_time_window

        tempdb = self.dbcentral(start)
        if not tempdb:
            response_data = "Not valid database for this time [%s]" % start
            self.logger.error(response_data)
            return { "ERROR": response_data }

        regex = "-s 'sta=~/%s/ && chan=~/%s/' " % (query['sta'],query['chan'])

        if query['filter'] != 'None':
            filter = "-f '%s'" % query['filter']
        else:
            filter = ""

        if query['coverage']:
            coverage = "-b "
        else:
            coverage = ""

        if query['realtime']:
            realtime = "-r "
        else:
            realtime = ""

        if query['precision']:
            precision = "-q %s" % query['precision']
        else:
            precision = ""

        if query['median']:
            median = "-a "
        else:
            median = ""

        if query['calibrate']:
            calibrate = "-c "
        else:
            calibrate = ""

        if query['period']:
            period = "-t %s" % query['period']
        else:
            period = ""

        if query['page']:
            page = "-p %s" % query['page']
        else:
            page = ""

        run = "dbwfserver_extract %s %s %s %s %s %s %s %s %s -n %s -m %s %s %s %s 2>&1" % (
            regex, coverage, filter, page, calibrate, precision, realtime,
            median, period, self.config.max_traces, self.config.max_points,
            tempdb, start, end
        )

        self.logger.info("QueryParser(): get_data(): Extraction command: [%s]" % run)

        # Method 1
        #master, slave = pty.openpty()
        #pipe = Popen(run, stdin=PIPE, stdout=slave, stderr=slave, close_fds=True, shell=True)
        #stdout = os.fdopen(master)
        #return stdout.readline()

        # Method 2
        return os.popen(run).read().replace('\n', '')

