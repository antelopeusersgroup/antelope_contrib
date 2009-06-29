import sys
import os
import re

from string import Template
from twisted.web import resource

import antelope.stock as stock

import dbwfserver.eventdata 
import dbwfserver.config as config

if(float(sys.version_info[0])+float(sys.version_info[1])/10 >= 2.6):
    import json
else:
    import simplejson as json

eventdata = dbwfserver.eventdata.EventData(config.dbname)

class DataSlice(resource.Resource):
    """Serve Datascope query requests.

    Support following query arguments:
        * net - Network Code - e.g. 'TA'
        * sta - Station Code - e.g. '109C'
        * chan - Channel Code - 'BHZ'
        * ts - Time Start in Epoch Seconds
        * te - Time End in Epoch Seconds
        * tw - Time Window in Seconds
        * availability - 'Lines' (True) or 'Waveforms' (False). 'Lines' only indicate Waveform availability. 
        * canvas_size - The number of pixels of the plotting Canvas, to be passed to 'get_segment'
    """

    def __init__(self):
        resource.Resource.__init__(self)

    def _extract_request(self, request):
        
        net = request.args.get('net', [None])
        sta = request.args.get('sta', [None])[0]
        chan_args = request.args.get('chan', []) 
        time_window = float(request.args.get('tw', [0])[0])
        time_start = float(request.args.get('ts', [-1])[0])
        time_end = float(request.args.get('te', [-1])[0])
        availability = request.args.get('availability', [False])[0]
        canvas_size = request.args.get('canvas_size', [config.canvas_size_default])[0] 

        chans = list(set([chan.upper() for chan in chan_args]))
        if not chans:
            chans = list(config.default_chans)
        chans.sort()

	if config.verbose:
	    print "Received request for:"
	    print "\tnet:\t%s" % net
	    print "\tsta:\t%s" % sta
	    print "\tchans:\t%s" % str(chans)
	    print "\ttime_window:\t%s" % stock.strtdelta(time_window)
	    print "\ttime_start:\t%s" % stock.strtime(time_start)
	    print "\ttime_end:\t%s" % stock.strtime(time_end)
	    print "\tavailability:\t%s" % str(availability)
	    print "\tcanvas_size:\t%d" % canvas_size

	return net, sta, chans, time_window, time_start, time_end, availability, canvas_size

    def getChild(self, name, request):

        if name == '':
            return self

        return resource.Resource.getChild(self, name, request)

    def render_GET(self, request):

	net, sta, chans, time_window, time_start, time_end, availability, canvas_size = self._extract_request(request)

        try:
	    # SCAFFOLD not quite right
            max_time_start, max_time_end = eventdata.available_range_for_stachan(sta, chans[0])
        except KeyError:
            request.setHeader("response-code", 500)
            return "No data for sta %s" % sta

        #default time slice is the min and max:
        if time_start == 0 and time_end == 0:
            time_start, time_end = max_time_start, max_time_end

        response_data = {}

        for chan in chans:
            if availability:
                pass #get Line data
            else:
                segment = eventdata.get_segment(time_start, time_end, sta, chan, canvas_size)
                response_data.update({chan:segment})

        request.setHeader("content-type", "application/json")

        response_data.update({"sta": sta, "chans":chans, "max_ts":max_time_start, "max_te":max_time_end})
	
        response_data = json.dumps(response_data)

        return response_data


class Root(resource.Resource):
    isLeaf = False

    def _format_stations(self):

        fmt = "<option value='%s'>%s</option>"
	stas = eventdata.available_stations()
        return "\n".join([fmt % (sta, sta) for sta in stas])

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

        tvals = {
            "dbname":config.dbname,
	    "application_title":config.application_title,
            "stations":self._format_stations(),
	    "jquery_includes":self._jquery_includes(),
        }

        template = config.index_html_template

        html = Template(open(template).read()).substitute(tvals)

        return html
