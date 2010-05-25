from twisted.python import log 
import sys
import os
import re
from time import gmtime, strftime

from string import Template
from twisted.web import resource

import antelope.stock as stock
from antelope.datascope import *

import dbwfserver.eventdata 
import dbwfserver.config as config

from collections import defaultdict 

"""
Import Python module JSON or SimpleJSON to 
parse returned results from queries
bsed on Python version test
"""

if(float(sys.version_info[0])+float(sys.version_info[1])/10 >= 2.6):

    import json

else:

    import simplejson as json



def isNumber(test):
#{{{
    """
    Test if the string is a valid number 
    and return the converted number. 
    """
    try:
        try:
            return int(test)
        except:
            return float(test)
    except:
        return False
#}}}

class QueryParser(resource.Resource):
    """
    Serve Datascope query requests.
    """
    def __init__(self):
#{{{
        resource.Resource.__init__(self)

        self.dbname = config.dbname
        self.db = dbopen(self.dbname)

        self.stations = dbwfserver.eventdata.Stations(self.dbname)
        self.events = dbwfserver.eventdata.Events(self.dbname)

        self.eventdata = dbwfserver.eventdata.EventData(self.dbname)
#}}}

    def _jquery_includes(self):
        # {{{
        jquery_includes = ''

        for jqf in config.jquery_files:

            if(re.match(r'^IE\s+', jqf)):

                re.sub(r'^IE\s+', '', jqf)
                jquery_includes += '<!--[if IE]>\n'
                jquery_includes += '<script language="javascript" '
                jquery_includes += 'type="text/javascript" src="'
                jquery_includes += jqf
                jquery_includes += '"></script>\n'
                jquery_includes += '<![endif]-->\n'

            else:

                jquery_includes += '<script type="text/javascript" '
                jquery_includes += 'src="'
                jquery_includes += jqf
                jquery_includes += '"></script>\n'

        return jquery_includes
        # }}}

    def getChild(self, name, request):
#{{{
        return self
#}}}

    def render(self, request):

#{{{ Setup template, init variables
        template = config.html_template

        url_params = {}

        response_data = {}

        tvals = defaultdict(dict)

        tvals = { 
            "dir":               '&mdash;',
            "key":               '&mdash;',
            "error":             'false',
            "wf_data":           'false',
            "cv_data":           'false',
            "event_list":        'false',
            "event_data":        'false',
            "event_selc":        'false',
            "station_selc":      'false',
            "station_data":      'false',
            "station_list":      'false',
            "dbname":            config.dbname,
            "application_title": config.application_title,
            "jquery_includes":   self._jquery_includes(),
            "filters":           '<option value="None">None</option>'
        }

        for filter in config.filters:
            tvals['filters'] += '<option value="'+config.filters[filter].replace(' ','_')+'">'
            tvals['filters'] += filter
            tvals['filters'] += '"</option>'

        if config.display_arrivals:
            tvals['display_arrivals'] = 'checked="checked"'
        else:
            tvals['display_arrivals'] = ''

        args = request.uri.split("/")[1:]

        #
        # remove last element if it's empty... 
        # This (localhost:8008/stations/) is the same as 
        # (localhost:8008/stations) 
        #
        if args[len(args)-1] == '':
            args.pop()
#}}}

        if args:

            tvals['dir'] = args[0]

            log.msg("ARGS: %s " % args)

            if args[0] == 'data':
                #{{{
                if config.verbose:
                    log.msg("Data query of type: %s " % args[0])

                request.setHeader("content-type", "application/json")

                if 'wf' in args:

                    """
                    TEST:

                        http://localhost:8008/data/wf/USP/645
                        http://localhost:8008/data/wf/AAK+BBC/BHZ/645
                        http://localhost:8008/data/wf/USP/706139610/706139810
                        http://localhost:8008/data/wf/USP+AAK/BHZ/706139710/706139810
                        http://localhost:8008/data/wf/USP+AAK/BHZ/706139710/706139810/0.3-1_BP

                    """
                    if len(args) == 2:
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 4:
                        url_params['sta'] = args[2].split('+')
                        url_params['orid'] = args[3]
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 5:
                        url_params['sta'] = args[2].split('+')
                        if isNumber( args[3] ):
                            url_params['time_start'] = args[3]
                            url_params['time_end'] = args[4]
                        else:
                            url_params['chans'] = args[3].split('+')
                            url_params['orid'] = args[4]

                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 6:
                        url_params['sta'] = args[2].split('+')
                        url_params['chans'] = args[3].split('+')
                        url_params['time_start'] = args[4]
                        url_params['time_end'] = args[5]
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 7:
                        url_params['sta'] = args[2].split('+')
                        url_params['chans'] = args[3].split('+')
                        url_params['time_start'] = args[4]
                        url_params['time_end'] = args[5]
                        url_params['filter'] = args[6]
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                elif 'coverage' in args:
                    """
                    You can test this with:
                        http://localhost:8008/data/coverage/
                        http://localhost:8008/data/coverage/AAK+USP
                        http://localhost:8008/data/coverage/AAK+USP/BHZ+BHN
                        http://localhost:8008/data/coverage/AAK+USP/706139700
                        http://localhost:8008/data/coverage/AAK+USP/BHZ/706139700
                        http://localhost:8008/data/coverage/AAK+USP/BHZ/706139700/706139820

                    """

                    if len(args) == 3:
                        url_params['sta'] = args[2].split('+')

                    elif len(args) == 4:
                        url_params['sta'] = args[2].split('+')
                        if isNumber( args[3] ):
                            url_params['time_start'] = args[3]
                        else:
                            url_params['chans'] = args[3].split('+')

                    elif len(args) == 5:
                        url_params['sta'] = args[2].split('+')
                        if isNumber( args[3] ):
                            url_params['time_start'] = args[3]
                            url_params['time_end'] = args[4]
                        else:
                            url_params['chans'] = args[3].split('+')
                            url_params['time_start'] = args[4]

                    elif len(args) == 6:
                        url_params['sta'] = args[2].split('+')
                        url_params['chans'] = args[3].split('+')
                        url_params['time_start'] = args[4]
                        url_params['time_end'] = args[5]

                    if config.verbose:
                        log.msg("Query coverage. %s" % str(args) ) 

                    response_data.update(self.eventdata.coverage(url_params))

                    return json.dumps(response_data)

                elif 'events' in args:
                    """
                    You can test this with:

                        http://localhost:8008/data/events - list of events
                        http://localhost:8008/data/events/66484 - list of stations that recorded event 66484
                        http://localhost:8008/data/events/645+655
                    """

                    if len(args) == 3:
                        args[2] = args[2].split('+')

                        for event in args[2]:
                            log.msg("\n\n\tcalling self.events(%s)\n\n" % event)
                            response_data[event] = self.events(event)
                        return json.dumps(response_data)

                    else:
                        return json.dumps(self.events.table())

                elif 'stations' in args:
                    """
                    You can test this with:

                        http://localhost:8008/data/stations
                        http://localhost:8008/data/stations/Y12C
                        http://localhost:8008/data/stations/LZH+OBN+USP
                    """

                    if len(args) == 3:
                        args[2] = args[2].split('+')

                        for sta in args[2]:
                            log.msg("\n\n\tcalling self.stations(%s)\n\n" % sta)
                            response_data[sta] = self.stations(sta)
                        return json.dumps(response_data)

                    else:
                        return json.dumps(self.stations.list())

                elif 'filters' in args:

                    """
                    You can test this with:

                        http://localhost:8008/data?type=filters
                        OR
                        http://localhost:8008/data/filters
                    """

                    return json.dumps(config.filters, sort_keys=True)

                else:
                    request.setHeader("content-type", "text/html")
                    request.setHeader("response-code", 500)
                    dbwfserver.eventdata._error("Unknown type of query: %s" % args)
                    tvals['error'] =  json.dumps( "Unknown query type:(%s)" % args )

                #}}}

            elif args[0] == 'events':
                if len(args) > 1:#{{{

                    tvals['key'] = args[1]
                    tvals['event_data'] = {}

                    log.msg("\n\n\tcalling self.events(%s)\n\n" % args[1])
                    tvals['event_data'] = self.events(args[1])

                    if not tvals['event_data']:
                        request.setResponseCode(404)
                        return "No origin %s in database (404 error)" % (args[1])

                    tvals['event_selc'] = args[1]
                    tvals['station_list'] = self.stations.list()

                else:

                    tvals['event_list'] = json.dumps(self.events.table())

#}}}

            elif args[0] == 'stations':
                if len(args) > 1:#{{{

                    tvals['key'] = args[1]
                    args[1] = args[1].split('+')
                    tvals['station_data'] = {}

                    for sta in args[1]:
                        log.msg("\n\n\tcalling self.stations(%s)\n\n" % sta)
                        tvals['station_data'][sta] = json.dumps( self.stations(sta) )

                    if not tvals['station_data']:
                        request.setResponseCode(404)
                        return "No station %s in database (404 error)" % (args[1])

                    tvals['station_selc'] = json.dumps( args[1] )
                    tvals['event_list'] = json.dumps(self.events.table())

                else:

                    tvals['station_list'] = json.dumps( self.stations.list() )

                #}}}

            elif args[0] == 'wf':
#{{{
                """
                TEST:

                    http://localhost:8008/wf/USP/645
                    http://localhost:8008/wf/AAK+BBC/BHZ/645
                    http://localhost:8008/wf/USP/706139610/706139810
                    http://localhost:8008/wf/USP+AAK/BHZ/706139710/706139810

                """
                if len(args) == 3:
                    url_params['sta'] = args[1].split('+')
                    url_params['orid'] = args[2]
                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))
                    tvals['key'] = args[1] + ' / ' + args[2]

                elif len(args) == 4:
                    url_params['sta'] = args[1].split('+')
                    if isNumber( args[2] ):
                        url_params['time_start'] = args[2]
                        url_params['time_end'] = args[3]
                    else:
                        url_params['chans'] = args[2].split('+')
                        url_params['orid'] = args[3]
                    tvals['key'] = args[1] + ' / ' + args[2] + ' / ' + args[3]
                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                elif len(args) == 5:
                    url_params['sta'] = args[1].split('+')
                    url_params['chans'] = args[2].split('+')
                    url_params['time_start'] = args[3]
                    url_params['time_end'] = args[4]
                    tvals['key'] = args[1] + ' / ' + args[2] + ' / ' + args[3] + ' / ' + args[4]
                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                elif len(args) == 6:
                    url_params['sta'] = args[1].split('+')
                    url_params['chans'] = args[2].split('+')
                    url_params['time_start'] = args[3]
                    url_params['time_end'] = args[4]
                    url_params['filter'] = args[5]
                    tvals['key'] = args[1] + ' / ' + args[2] + ' / ' + args[3] + ' / ' + args[4] + ' / ' + args[5]
                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                else:
                    request.setResponseCode(404)
                    return "If you request the waveforms resource (/wfs) you must provide a station code and epoch time (404 error)"

#}}}

            elif args[0] == 'coverage':
#{{{
                """
                You can test this with:
                    http://localhost:8008/coverage/
                    http://localhost:8008/coverage/AAK+USP
                    http://localhost:8008/coverage/AAK+USP/BHZ+BHN
                    http://localhost:8008/coverage/AAK+USP/706139700
                    http://localhost:8008/coverage/AAK+USP/BHZ/706139700
                    http://localhost:8008/coverage/AAK+USP/BHZ/706139700/706139820

                """

                if len(args) == 2:
                    url_params['sta'] = args[1].split('+')

                elif len(args) == 3:
                    url_params['sta'] = args[1].split('+')
                    if isNumber( args[2] ):
                        url_params['time_start'] = args[2]
                    else:
                        url_params['chans'] = args[2].split('+')

                elif len(args) == 4:
                    url_params['sta'] = args[1].split('+')
                    if isNumber( args[2] ):
                        url_params['time_start'] = args[2]
                        url_params['time_end'] = args[3]
                    else:
                        url_params['chans'] = args[2].split('+')
                        url_params['time_start'] = args[3]

                elif len(args) == 5:
                    url_params['sta'] = args[1].split('+')
                    url_params['chans'] = args[2].split('+')
                    url_params['time_start'] = args[3]
                    url_params['time_end'] = args[4]

                if config.verbose:
                    log.msg("Query coverage. %s" % str(args) ) 


                tvals['cv_data'] =  json.dumps(self.eventdata.coverage(url_params))
#}}}

            elif args[0] != '':
                request.setHeader("response-code", 500)
                dbwfserver.eventdata._error("Unknown type of query: %s" % args)
                tvals['error'] =  json.dumps( "Unknown query type:(%s)" % args )

        html_stations = Template(open(template).read()).substitute(tvals)

        request.write( html_stations )
        request.finish()
