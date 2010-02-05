from twisted.python import log 
import sys
import os
import re

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
        
        type        = request.args.get('type',        [None])[0]
        net_args    = request.args.get('net',         None)
        sta_args    = request.args.get('sta',         None)
        orid        = request.args.get('orid',        [None])[0]
        availability= request.args.get('availability',[None])[0]
        orid_time   = request.args.get('orid_time',   [None])[0]
        time_start  = request.args.get('ts',          [None])[0]
        time_end    = request.args.get('te',          [None])[0]
        chan_args   = request.args.get('chans',       config.default_chans) 
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

    def getChild(self, name, request):

        if name == '':

            return self

        return resource.Resource.getChild(self, name, request)

    def render_GET(self, request):

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

                http://localhost:8008/data?type=wf&sta=113A&orid=66554&ts=1230900154&te=1230900254&chans=BHZ

                http://localhost:8008/data?type=wf&sta=113A&ts=1230900154&te=1230900254

            #DEBUG TOOL:
            #This line will output all vars as a json object:

            return json.dumps({"net": net, "sta": sta, "chan":chan, "orid":orid, "orid_time":orid_time, "time_window":time_window, "time_start":time_start, "time_end":time_end, "availability":availability, "canvas_size":canvas_size, "filter":filter })
            """

            for my_sta in sta:

                for my_chan in chan:

                    my_stachan = my_sta+'_'+my_chan # Define the STA_CHAN combination

                    if config.apply_calib is True:

                        if config.verbose:

                            log.msg("Function: get_segment(%s,%s,%s,%s,%s,%s,%s,%s,%s)" % (my_sta,my_chan,canvas_size,orid,time_window,time_start,time_end,filter,config.apply_calib) )

                        try:
                            [format,segment] = eventdata.get_segment(my_sta, my_chan, canvas_size, orid, time_window, time_start, time_end, filter, config.apply_calib)
                        except:
                            request.setHeader("response-code", 500)
                            log.msg("\n")
                            log.msg("Problems on query:get_segment(sta=%s,chan=%s,orid=%s,st=%s,et=%s)" % (my_sta,my_chan,orid,time_start,time_end) )
                            log.msg("\n")
                            return "Problems on query:get_segment(sta=%s,chan=%s,orid=%s,st=%s,et=%s)" % (my_sta,my_chan,orid,time_start,time_end)

                    else:

                        if config.verbose:

                            log.msg("Function: get_segment(%s,%s,%s,%s,%s,%s,%s,%s)" % (my_sta,my_chan,canvas_size,orid,time_window,time_start,time_end,filter) )

                        try:
                            [format,segment] = eventdata.get_segment(my_sta, my_chan, canvas_size, orid, time_window, time_start, time_end, filter)
                        except:
                            request.setHeader("response-code", 500)
                            log.msg("\n")
                            log.msg("Problems on query:get_segment(sta=%s,chan=%s,orid=%s,st=%s,et=%s)" % (my_sta,my_chan,orid,time_start,time_end) )
                            log.msg("\n")
                            return "Problems on query:get_segment(sta=%s,chan=%s,orid=%s,st=%s,et=%s)" % (my_sta,my_chan,orid,time_start,time_end)

                    response_data.update({my_stachan:segment})

            if orid:

                this_metadata = eventdata.event_list(my_sta,orid)
                response_data.update({"metadata":this_metadata})

            response_data.update({"net": net, "sta": sta, "chan":chan, "orid":orid, "orid_time":orid_time, "time_window":time_window, "time_start":time_start, "time_end":time_end, "availability":availability, "canvas_size":canvas_size, "format":format, "filter":filter})

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

            if not sta:

                return json.dumps(eventdata.event_list(None,orid))

            for my_sta in sta:

                if len(sta) == 1:

                    response_data = eventdata.event_list(my_sta,orid)

                else:

                    response_data.update({my_sta:eventdata.event_list(my_sta,orid)})

            return json.dumps(response_data)


        elif type == 'stations':

            """
            You can test this with:
            http://localhost:8008/data?type=stations
            """

            return json.dumps(eventdata.available_stations())

        elif type == 'filters':

            """
            You can test this with:
            http://localhost:8008/data?type=filters
            """

            my_filters = eventdata.available_filters()

            return json.dumps(my_filters, sort_keys=True)
        
        else:

            request.setHeader("response-code", 500)
            log.msg("ERROR in query. Unknown type:%s" % type)
            return "Unknown query type:(%s)" % type

        return 0


class Root(resource.Resource):

    isLeaf = True

    def _jquery_includes(self):

        jquery_includes = ''

        for jqf in config.jquery_files:

            if(re.match(r'^IE\s+', jqf)):

                re.sub(r'^IE\s+', '', jqf)
                jquery_includes += '<!--[if IE]>\n'
                jquery_includes += '<script language="javascript" '
                jquery_includes += 'type="text/javascript" src="jquery/'
                jquery_includes += jqf
                jquery_includes += '"></script>\n'
                jquery_includes += '<![endif]-->\n'

            else:

                jquery_includes += '<script type="text/javascript" '
                jquery_includes += 'src="jquery/'
                jquery_includes += jqf
                jquery_includes += '"></script>\n'

        return jquery_includes

    def getChild(self, name, request):

        if name == '':

            return self

        return resource.Resource.getChild(self, name, request)

    def render_GET(self, request):

        """
        Load template and substiude values. 
        """

        tvals = {
            "dbname":config.dbname,
            "application_title":config.application_title,
            "jquery_includes":self._jquery_includes(),
        }

        template = config.index_html_template

        html = Template(open(template).read()).substitute(tvals)

        request.write( html )

        """
        If request.args defined...

        request.path The path only (arguments not included).
        request.args All of the arguments, including URL and POST arguments.
        request.uri  The full URI that was requested (includes arguments). 
            For request.args:
                ?foo=bar&foo=baz&quux=spam 
            results in: 
                {'foo': ['bar', 'baz'], 'quux': ['spam']}. )
        """

        if request.args:
            rqst =  '<script type="text/javascript">$(document).ready('

            if len(request.args) > 1:
                text = '{'
                for arg in request.args:
                    if arg == 'chans' or arg == 'sta':
                        text = text + arg + ':' + str(request.args[arg]) + ','
                    else:
                        text = text + arg + ':"' + str(request.args[arg]) + '",'
                text = text + 'url:"yes"}'
                rqst =  rqst + 'PlotSelect.getData('+text+')'

            elif 'orid' in request.args:
                rqst =  rqst + 'PlotSelect.doQueryAjax("data","events","-","'+request.args['orid']+'")'

            elif 'sta' in request.args:
                rqst =  rqst + 'PlotSelect.doQueryAjax("data","events",'+str(request.args['sta'])+',-1)'

            else:
                reqst = rqst + 'alert("Problem on request for data: %s")' % request.args

            rqst =  rqst + ');</script>'

            request.write(rqst)

        return ""
