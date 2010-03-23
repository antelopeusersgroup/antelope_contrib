from twisted.python import log 
import sys
import os
import re
from time import gmtime, strftime

from string import Template
from twisted.web import resource

import antelope.stock as stock

import dbwfserver.eventdata 
import dbwfserver.config as config

"""
Import Python module JSON or SimpleJSON to 
parse returned results from queries
bsed on Python version test
"""

if(float(sys.version_info[0])+float(sys.version_info[1])/10 >= 2.6):

    import json

else:

    import simplejson as json

eventdata = dbwfserver.eventdata.EventData(config.dbname)



def isNumber(test):
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




class Data(resource.Resource):

    """
    Serve Datascope query requests.

    Support following query arguments:
        * type - Data Type - e.g. 'wf','events','stations'
        * net - Network Code - e.g. 'TA'
        * sta - Station Code - e.g. '109C'
        * orid - Origin id - e.g. '66484'
        * chan - Channel Code - 'BHZ'
        * ts - Time Start in Epoch Seconds
        * te - Time End in Epoch Seconds
        * tw - Time Window in Seconds
        * availability - 'Lines' (True) or 'Waveforms' (False). 'Lines' only indicate Waveform availability. 
        * canvas_size - The number of pixels of the plotting Canvas, to be passed to 'get_segment'
        * filter - Waveform filter
    """

    def __init__(self):

        resource.Resource.__init__(self)

    def _extract_request(self, request):
        #{{{
        type        = request.args.get('type',        [None])[0]
        net_args    = request.args.get('net',         None)
        sta_args    = request.args.get('sta',         None)
        orid        = request.args.get('orid',        [None])[0]
        availability= request.args.get('availability',[None])[0]
        orid_time   = request.args.get('orid_time',   [None])[0]
        time_start  = request.args.get('ts',          [None])[0]
        time_end    = request.args.get('te',          [None])[0]
        chan_args   = request.args.get('chan',        config.default_chans) 
        time_window = request.args.get('tw',          [config.default_time_window])[0]
        canvas_size = request.args.get('canvas_size', [config.canvas_size_default])[0] 
        filter      = request.args.get('filter',      [None])[0] 

        if net_args:
            net = list(set([n.upper() for n in net_args]))
            net.sort()
        else:
            net =  None

        if sta_args:
            sta = list(set([s.upper() for s in sta_args]))
            sta.sort()
        else:
            sta =  None

        chan = list(set([c.upper() for c in chan_args]))
        chan.sort()

        if time_start: 
            time_start = float(time_start)
        
        if time_end: 
            time_end = float(time_end)

        if time_window: 
            time_window = float(time_window)

        if orid: 
            try:
                orid = int(orid)
            except:
                orid = False

        if canvas_size: 
            canvas_size = int(canvas_size)

        if config.verbose:
            log.msg("Received request for:")
            log.msg("\ttype:\t%s" % type)
            log.msg("\tnet:\t%s" % str(net))
            log.msg("\tsta:\t%s" % str(sta))
            log.msg("\torid:\t%s" % orid)
            log.msg("\toridtime:\t%s" % orid_time)
            log.msg("\tchan:\t%s" % str(chan))
            log.msg("\ttime_window:\t%s" % stock.strtdelta(time_window))
            log.msg("\ttime_start:\t%s" % time_start)
            log.msg("\ttime_end:\t%s" % time_end)
            log.msg("\tavailability:\t%s" % str(availability))
            log.msg("\tcanvas_size:\t%d" % canvas_size)
            log.msg("\tfilter:\t%s" % filter)

        return type, net, sta, orid, chan, orid_time, time_window, time_start, time_end, availability, canvas_size, filter
#}}}
    def getChild(self, name, request):
#{{{
        if name == '':

            return self

        return resource.Resource.getChild(self, name, request)
#}}}
    def render_GET(self, request):
#{{{
        type, net, sta, orid, chan, orid_time, time_window, time_start, time_end, availability, canvas_size, filter = self._extract_request(request)

        """
        Handle different type of data request
        such as metadata (stations, events)
        and waveform data
        """

        request.setHeader("content-type", "application/json")

        response_data = {}

        if config.verbose:
            log.msg("Type of query:%s " % type)

        if type == 'wf':

            """
            TEST:
                http://localhost:8008/data?type=wf&sta=113A&orid=66554

                http://localhost:8008/data?type=wf&sta=113A&orid=66554&ts=1230900154&te=1230900254&chan=BHZ

                http://localhost:8008/data?type=wf&sta=113A&ts=1230900154&te=1230900254

            #DEBUG TOOL:
            #This line will output all vars as a json object:

            return json.dumps({"net": net, "sta": sta, "chan":chan, "orid":orid, "orid_time":orid_time, "time_window":time_window, "time_start":time_start, "time_end":time_end, "availability":availability, "canvas_size":canvas_size, "filter":filter })
            """

            function = "Function: get_segment(%s,%s,%s,%s,%s,%s,%s,%s,%s)" % (str(sta),str(chan),canvas_size,orid,orid_time,time_window,time_start,time_end,filter,config.apply_calib)
            if config.debug: log.msg(function)

            try:
                return json.dumps(eventdata.get_segment(sta, chan, canvas_size, orid, None, time_window, time_start, time_end, filter))
            except:
                request.setHeader("response-code", 500)
                log.msg("\n")
                log.msg("Problems on... " + function)
                log.msg("\n")
                return function


        elif type == 'coverage':
            """
            You can test this with:
            http://localhost:8008/data?type=coverage 
                        - list coverage tuples of (time,end_time) for all stations and default channels
            or
            http://localhost:8008/data?type=coverage&sta=X18A&chan=BHZ
                        - list coverage tuples of (time,end_time) for station X18A chan BHZ
            or
            http://localhost:8008/data?type=coverage&te=1230940700
                        - list coverage tuples of (time,end_time) until time_end
            or
            http://localhost:8008/data?type=coverage&chan=BHZ&ts=1230768001&te=1230940700
                        - list coverage tuples of (time,end_time) between start and end times for all BHZ chans
            or 
            http://localhost:8008/data?type=coverage&sta=X18A&chan=BHZ&ts=1230768001&te=1230940700
                    
            Multiple stations/channels query...
                http://localhost:8008/data?type=events&sta=113A&sta=123A&chan=BHZ&chan=BHE&chan=BHN
            """

            if config.verbose:
                log.msg("Query coverage. STA:%s CHAN:%s START:%s END:%s" % (str(sta),str(chan),str(time_start),str(time_end)) ) 

            response_data = {'type':'coverage'}

            response_data.update({'format':'bars'})

            response_data.update(eventdata.coverage(sta,chan,time_start,time_end))

            return json.dumps(response_data)

        elif type == 'events':
            """
            You can test this with:
            http://localhost:8008/data?type=events - list of events
            or 
            http://localhost:8008/data?type=events&sta=127A - dict. of events recorded by station 127A
            or 
            http://localhost:8008/data?type=events&orid=66484 - list of stations that recorded event 66484
            or 
            http://localhost:8008/data?type=events&sta=127A&orid=66484 - returns a floating point that is the arrival time

            UPDATE:
                Multiple stations query...
                http://localhost:8008/data?type=events&sta=113A&sta=123A
            """

            if config.verbose:
                log.msg("Query events. STA:%s ORID:%s" % (str(sta),orid) ) 

            return json.dumps(eventdata.event_list(sta,orid))

        elif type == 'stations':

            """
            You can test this with:
            http://localhost:8008/data?type=stations
            http://localhost:8008/data?type=stations&sta=Y12C
            """

            return json.dumps(eventdata.available_stations(sta))

        elif type == 'filters':

            """
            You can test this with:
            http://localhost:8008/data?type=filters
            """

            return json.dumps(config.filters, sort_keys=True)
        
        else:

            request.setHeader("response-code", 500)
            log.msg("ERROR in query. Unknown type:%s" % type)
            return "Unknown query type:(%s)" % type

        return 0
#}}}

class Root(resource.Resource):

    isLeaf = True

    def _jquery_includes(self):
#{{{
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
#}}}
    def getChild(self, name, request):
#{{{
        if name == '':

            return self

        return resource.Resource.getChild(self, name, request)
#}}}
    def render_GET(self, request):

        # {{{ Load template and substiude values. 

        tvals = {
            "dbname":config.dbname,
            "application_title":config.application_title,
            "jquery_includes":self._jquery_includes(),
        }

        template = config.index_html_template
        log.msg( template )
        # }}} Load template and substiude values. 

        html = Template(open(template).read()).substitute(tvals)

        request.write( html )

        return ""


class QueryParser(resource.Resource):

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

        return self

    def render(self, request):

        # {{{ Output based on URI query args

        tvals = { 
            "dbname":            config.dbname,
            "application_title": config.application_title,
            "jquery_includes":   self._jquery_includes(),
            "dir":               '', 
            "key":               '', 
            "my_list":           ''
        }

        args = request.uri.split("/")[1:]

        template_queryparser = config.queryparser_html_template

        my_list = '<ul class="ui-helper-reset ui-helper-clearfix">'

        if args:

            metadatatype = args[0]
            tvals['dir'] = args[0]

            if metadatatype == 'stations':

                if len(args) > 1:

                    tvals['key'] = args[1]
                    args_list = args[1].split('+')

                    mydata = eventdata.event_list(args_list)
                    if not mydata:
                        request.setResponseCode(404)
                        return "Station code %s did not record any events (404 error)" % (args[1])
                else:

                    mydata = eventdata.available_stations()

            elif metadatatype == 'events':

                if len(args) > 1:

                    mydata = eventdata.event_list(None,args[1])
                    metadatatype = 'wfs'
                    tvals['key'] = strftime("%Y-%m-%d %H:%M:%S",gmtime(float(args[1])))

                    if not mydata:
                        request.setResponseCode(404)
                        return "Origin time %s does not have any stations recording arrivals (404 error)" % (args[1])

                else:
                    mydata = eventdata.event_list()

            else:
    
                request.setResponseCode(404)
                return "Data type %s not found (404 error)" % (args[1])



            if isinstance(mydata,dict):

                for my_key,my_vals in mydata.iteritems():

                    my_vals.sort()

                    for val in my_vals:
                        if metadatatype == 'stations':
                            val_readable = strftime("%Y-%m-%d %H:%M:%S",gmtime(float(val)))
                            my_list += "<li class='ui-state-active ui-corner-all'><a href='/wfs/%s/%s'>%s</a></li>\n" % (my_key,val,val_readable)
                        else:
                            val_readable = val
                            my_list += "<li class='ui-state-active ui-corner-all'><a href='/wfs/%s/%s'>%s</a></li>\n" % (val,my_key,val_readable)

            else:

                mydata.sort()
                for mys in mydata:

                    if metadatatype == 'events':
                        mys_readable = strftime("%Y-%m-%d %H:%M:%S",gmtime(float(mys)))
                    else:
                        mys_readable = mys

                    my_list += "<li class='ui-state-active ui-corner-all'><a href='/%s/%s'>%s</a></li>\n" % (metadatatype,mys,mys_readable)

            tvals['my_list'] = my_list

        else:

            request.setResponseCode(404)
            return "Resource not found (404 error)"

        # }}} Output based on URI query args

        html_stations = Template(open(template_queryparser).read()).substitute(tvals)

        request.write( html_stations )

        return ""

class Waveform(resource.Resource):

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

        return self

    def render(self, request):

        tvals = { 
            "dbname":            config.dbname,
            "application_title": config.application_title,
            "jquery_includes":   self._jquery_includes(),
            "dir":               'waveforms', 
            "sta":               '', 
            "time":              '',
            "event_json":        ''
        }

        template_waveform = config.waveform_html_template

        args = request.uri.split("/")[1:]

        if len(args) >= 3:

                tvals['type'] = args[0]

                sta_list = args[1].split('+')

                orid_time = isNumber(args[2])

                tvals['sta'] = args[1]
                tvals['time'] = strftime("%Y-%m-%d %H:%M:%S",gmtime(orid_time))

                chan_args   = config.default_chans
                time_window = config.default_time_window
                canvas_size = config.canvas_size_default
                apply_calib = config.apply_calib

                chan = list(set([c.upper() for c in chan_args]))
                chan.sort()

                try:

                    tvals['event_json'] = json.dumps(eventdata.get_segment(sta_list,chan,canvas_size,None,orid_time,time_window,orid_time,None,None))

                except:

                    tvals['event_json'] = "{ 'error': 'Not a valid query' }"

        else:
        
            request.setResponseCode(404)
            return "If you request the waveforms resource (/wfs) you must provide a station code and epoch time (404 error)"

        html_stations = Template(open(template_waveform).read()).substitute(tvals)

        request.write( html_stations )

        return ""
