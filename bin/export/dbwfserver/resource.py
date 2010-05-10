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

    Support following query arguments:
        * type      - Data Type - e.g. 'wf','events','stations'
        * net       - Network Code - e.g. 'TA'
        * sta       - Station Code - e.g. '109C'
        * orid      - Origin id - e.g. '66484'
        * chan      - Channel Code - 'BHZ'
        * ts        - Time Start in Epoch Seconds
        * te        - Time End in Epoch Seconds
        * tw        - Time Window in Seconds
        * availability  - 'Lines' (True) or 'Waveforms' (False). 'Lines' only indicate Waveform availability. 
        * canvas_size   - The number of pixels of the plotting Canvas, to be passed to 'get_segment'
        * amount    - Amount of data to be retrieved: can be 'all', 'slice'
        * filter    - Waveform filter
        * phases    - Plot phase arrivals or not
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
        template_queryparser = config.queryparser_html_template

        template = config.index_html_template

        url_params = {}

        response_data = {}

        tvals = defaultdict(dict)

        tvals = { 
            "dbname":            config.dbname,
            "application_title": config.application_title,
            "jquery_includes":   self._jquery_includes(),
            "dir":               'false',
            "key":               'false',
            "my_list":           'false',
            "error":             'false',
            "wf_data":           'false',
            "event_list":        'false',
            "event_data":        'false',
            "event_selc":        'false',
            "station_selc":      'false',
            "station_data":      'false',
            "station_list":      'false'
        }

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
            my_list = '<ul class="ui-helper-reset ui-helper-clearfix">'

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
                        http://localhost:8008/data/wf/USP/706139610/706139810
                        http://localhost:8008/data/wf/USP+AAK/BHZ/706139710/706139810

                    """
                    if len(args) == 2:
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 4:
                        args[2] = args[2].split('+')
                        url_params['sta'] = args[2]
                        url_params['orid'] = args[3]
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 5:
                        args[2] = args[2].split('+')
                        url_params['sta'] = args[2]
                        if isNumber( args[3] ):
                            url_params['time_start'] = args[3]
                            url_params['time_end'] = args[4]
                        else:
                            args[3] = args[3].split('+')
                            url_params['chans'] = args[3]
                            url_params['orid'] = args[4]

                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                    elif len(args) == 6:
                        args[2] = args[2].split('+')
                        args[3] = args[3].split('+')
                        url_params['sta'] = args[2]
                        url_params['chans'] = args[3]
                        url_params['time_start'] = args[4]
                        url_params['time_end'] = args[5]
                        return json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                elif 'coverage' in args:
                    """
                    You can test this with:

                    """

                    if len(args) == 5:
                        args[2] = args[2].split('+')
                        url_params['sta'] = args[2]
                        if isNumber( args[3] ):
                            url_params['time_start'] = args[3]
                            url_params['time_end'] = args[4]
                        else:
                            args[3] = args[3].split('+')
                            url_params['chans'] = args[3]
                            url_params['time_start'] = args[4]

                    elif len(args) == 6:
                        args[2] = args[2].split('+')
                        args[3] = args[3].split('+')
                        url_params['sta'] = args[2]
                        url_params['chans'] = args[3]
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

                request.setHeader("response-code", 500)
                log.msg("ERROR in query. Unknown type:%s" % args)
                return "Unknown query type:(%s)" % args

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

                    #tvals['event_list'] = json.dumps(self.events.list())
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
                if len(args) == 3:
                    url_params['sta'] = [args[1]]
                    if self.events(args[2]):
                        url_params['orid'] = args[2]
                    else:
                        url_params['orid_time'] = args[2]

                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

                elif len(args) == 4:
                    url_params['sta'] = [args[1]]
                    url_params['time_start'] = args[2]
                    url_params['time_end'] = args[3]
                    tvals['wf_data'] = json.dumps(self.eventdata.get_segment(url_params, self.stations, self.events))

#}}}

            elif args[0] == 'coverage':
#{{{
                if len(args) >= 2:
                    url_params['sta']        = [args[1]]
                if len(args) >= 3:
                    url_params['chan']       = [args[2]]
                if len(args) >= 4:
                    url_params['time_start'] = args[3]
                if len(args) >= 5:
                    url_params['time_end']   = args[4]

                if config.verbose:
                    log.msg("Query coverage. %s" % str(args) ) 

                response_data = {'type':'coverage'}

                response_data.update({'format':'bars'})

                response_data.update(self.eventdata.coverage(url_params))

                tvals['wf_data'] =  json.dumps(response_data)
#}}}

            elif args[0] != '':
                log.msg("ERROR in query. Unknown type:%s" % args)
                tvals['error'] =  json.dumps( "Unknown query type:(%s)" % args )

        html_stations = Template(open(template_queryparser).read()).substitute(tvals)

        request.write( html_stations )

        return ""

        # {{{ Output based on URI query args


            #if isinstance(mydata,dict):

            #    for my_key,my_vals in mydata.iteritems():

            #        #my_vals.sort()

            #        for val in my_vals:
            #            if metadatatype == 'stations':
            #                val_readable = strftime("%Y-%m-%d %H:%M:%S",gmtime(float(val)))
            #                my_list += "<li class='ui-state-active ui-corner-all'><a href='/wfs/%s/%s'>%s</a></li>\n" % (my_key,val,val_readable)
            #            else:
            #                val_readable = val
            #                my_list += "<li class='ui-state-active ui-corner-all'><a href='/wfs/%s/%s'>%s</a></li>\n" % (val,my_key,val_readable)

            #else:

            #    #mydata.sort()
            #    for mys in mydata:

            #        if metadatatype == 'events':
            #            mys_readable = strftime("%Y-%m-%d %H:%M:%S",gmtime(float(mys)))
            #        else:
            #            mys_readable = mys

            #        my_list += "<li class='ui-state-active ui-corner-all'><a href='/%s/%s'>%s</a></li>\n" % (metadatatype,mys,mys_readable)


#        else:
#
#            request.setResponseCode(404)
#            return "Resource not found (404 error)"

        # }}} Output based on URI query args


#class Waveform(resource.Resource):
#
#    def _jquery_includes(self):
#
#        # {{{
#        jquery_includes = ''
#
#        for jqf in config.jquery_files:
#
#            if(re.match(r'^IE\s+', jqf)):
#
#                re.sub(r'^IE\s+', '', jqf)
#                jquery_includes += '<!--[if IE]>\n'
#                jquery_includes += '<script language="javascript" '
#                jquery_includes += 'type="text/javascript" src="'
#                jquery_includes += jqf
#                jquery_includes += '"></script>\n'
#                jquery_includes += '<![endif]-->\n'
#
#            else:
#
#                jquery_includes += '<script type="text/javascript" '
#                jquery_includes += 'src="'
#                jquery_includes += jqf
#                jquery_includes += '"></script>\n'
#
#        return jquery_includes
#        # }}}
#
#    def getChild(self, name, request):
#
#        return self
#
#    def render(self, request):
#
#        tvals = { 
#            "dbname":               config.dbname,
#            "application_title":    config.application_title,
#            "jquery_includes":      self._jquery_includes(),
#            "title":                '',
#            "jscript_vars":         '',
#            "filters":              ''
#        }
#
#        template_waveform = config.waveform_html_template
#
#        args = request.uri.split("/")[1:]
#
#        if len(args) >= 3:
#
#            sta_list = args[1].split('+')
#
#            if args[2]:
#
#                if isNumber(args[2]):
#                    chan = False
#                    orid_time = isNumber(args[2])
#                else:
#                    chan = args[2]
#                    chan_split = args[2].split('+')
#                    orid_time = isNumber(args[3])
#
#            # Build javascript variables
#            jscript_vars = "dataObj['sta'] = " + '["' + '","'.join(sta_list) + '"]\n'
#            jscript_vars += "dataObj['orid_time'] = " + str(orid_time) + '\n'
#
#            # Build title
#            title = tvals["application_title"] + " / waveforms / " + args[1] + " / "
#
#            # Build filter list
#            filterList = "<option value='None' selected>None</option>\n"
#            for filterKey in sorted(config.filters.iterkeys()):
#                filterList += "\t<option value='%s'>%s</option>\n" % (config.filters[filterKey],filterKey)
#            tvals['filters'] = filterList
#
#            # Override channels
#            if chan:
#                title += chan + " / "
#                jscript_vars += "dataObj['chan'] = " + '["' + '","'.join(chan_split) + '"]'
#
#            if orid_time: title += strftime("%Y-%m-%d %H:%M:%S",gmtime(orid_time))
#
#            tvals['title']        = title
#            tvals['jscript_vars'] = jscript_vars
#
#            html_stations = Template(open(template_waveform).read()).substitute(tvals)
#
#            request.write( html_stations )
#
#            return ""
#
#        else:
#        
#            request.setResponseCode(404)
#            return "If you request the waveforms resource (/wfs) you must provide a station code and epoch time (404 error)"
