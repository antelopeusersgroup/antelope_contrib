import sys
import os
import re

from twisted.python import log 
from twisted.web import server, static, rewrite
from twisted.application import service, internet

import antelope.stock as stock

import dbwfserver.config as config
import dbwfserver.resource as resource


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

def pathToArgs(request):
    """
    Assume segments of path are values for queries:
        Variabes:
            - sta
            - orid
            - start_time
            - end_time
        Opions:
            localhost:8008/STA                         => Will bring list of events
            localhost:8008/ORID                        => Will bring list of stations
            localhost:8008/STA/ORID                    => Look for waveform for this sta/orid
            localhost:8008/STA/CH01+CH03/ORID          => Look for waveform for this sta/chan/orid 
            localhost:8008/STA+STA/START/END           => Look for waveform from start to end
            localhost:8008/STA+STA/CH01+CH02/START/END => Look for waveform from start to end for chans

        
        TEST:
            http://localhost:8008/BEL
            http://localhost:8008/66529
            http://localhost:8008/BEL/66556
            http://localhost:8008/BEL/BHZ+BHN/66556
            http://localhost:8008/BEL+CIA/BHZ+BHN/1230886037.81/1230886241
    """
    parts = request.uri.split('/')

    if config.verbose:
        log.msg("\nReceived request:")
        log.msg("\tURL:\t%s" % request.uri)
        log.msg("\tPOSTPATH:\t%s" % request.postpath)
        log.msg("\tARGS:\t%s\n" % request.args)

    if request.uri == '/':
        if config.verbose: log.msg('Resource: /(Root)')

    elif 'jquery' in request.postpath:
        if config.verbose: log.msg('Folder: /jquery')
        position = [i for i,x in enumerate(parts) if x == 'jquery']
        parts = parts[position[0]:]
        request.path = '/jquery'
        request.postpath = parts

    elif 'static' in request.postpath:
        if config.verbose: log.msg('Folder: /static')
        position = [i for i,x in enumerate(parts) if x == 'static']
        parts = parts[position[0]:]
        request.path = '/static'
        request.postpath = parts

    elif 'data' in request.postpath:
        if config.verbose: log.msg('Resource: /data')
        request.path = '/data'
        request.postpath = ['data']

    elif len(request.postpath) == 1:
        """
        In this case we have two options:
            localhost:8008/BEL
            or
            localhost:8008/66556

            Note:
                It is possible to call this with 
                two stations, like:
                localhost:8008/BEL+CIA
                Not sure if this will be useful.
        """
        if isNumber(request.postpath[0]):
            vars = { 'orid':request.postpath[0]}
        else:
            stations = request.postpath[0].split('+')
            vars = {'sta':stations}

        if config.verbose: log.msg('Resource: /(Root) ? %s' % vars)
        request.args = vars
        request.postpath = ['']
        request.path = '/'

    elif len(request.postpath) == 2:
        """
        In this case we have two options:
            localhost:8008/BEL/66556
            or
            localhost:8008/66556/BEL

            Note:
                It is possible to call this with 
                two stations, like:
                localhost:8008/BEL+CIA/66556
                Not sure if this will be useful.
        """
        if isNumber(request.postpath[0]):
            stations = request.postpath[1].split('+')
            vars = { 'orid':request.postpath[0], 'sta':stations}
        else:
            stations = request.postpath[0].split('+')
            vars = { 'orid':request.postpath[1], 'sta':stations}

        if config.verbose: log.msg('Resource: /(Root) ? %s' % vars)
        request.args = vars
        request.postpath = ['']
        request.path = '/'

    elif len(request.postpath) == 3:
        """
        In this case we have two options:
            localhost:8008/BEL/BHZ+BHN/66556
            localhost:8008/BEL/1230886037.81/1230886241.25
            localhost:8008/BEL+CIA/1230886037.81/1230886241.25

            Note:
                It is possible to call this with 
                two stations, like:
                localhost:8008/BEL/1230886037.81/1230886241.25
                Only if you have the times, not orid. 
                The stations might not have the same event. 
        """

        positions = [i for i,x in enumerate(parts) if isNumber(x) ]

        if len(positions) == 1:
            stations = request.postpath[0].split('+')
            channels = request.postpath[1].split('+')
            vars = {'sta':stations, 'chans':channels,'orid':request.postpath[2]}

        elif len(positions) == 2:
            stations = request.postpath[0].split('+')
            vars = {'sta':stations, 'time_start':request.postpath[1],'time_end':request.postpath[2]}
        else:
            if config.verbose: log.msg('\n')
            if config.verbose: log.msg('ERROR: On 3 element query: %s' % parts)
            if config.verbose: log.msg('Options:\n\t[sta],[chans],[orid]\n\tor\n\t[sta],[start],[end]')
            if config.verbose: log.msg('\n')

        if config.verbose: log.msg('Resource: /(Root) ? %s' % vars)
        request.args = vars
        request.postpath = ['']
        request.path = '/'

    elif len(request.postpath) == 4:
        """
        In this case we have one option:
            localhost:8008/BEL/BHZ+BHN/1230886037.81/1230886241.25
            localhost:8008/BEL+G13A/BHZ+BHN/1230886037.81/1230886241.25
        """

        positions = [i for i,x in enumerate(parts) if isNumber(x) ]

        if len(positions) == 2:
            stations = request.postpath[0].split('+')
            channels = request.postpath[1].split('+')
            vars = {'sta':stations, 'chans':channels,'time_start':request.postpath[2],'time_end':request.postpath[3]}
        else:
            if config.verbose: log.msg('\n')
            if config.verbose: log.msg('ERROR: On 4 element query: %s' % parts)
            if config.verbose: log.msg('Options:\n\t[sta],[chans],[start],[end]')
            if config.verbose: log.msg('\n')

        if config.verbose: log.msg('Resource: /(Root) ? %s' % vars)
        request.args = vars
        request.postpath = ['']
        request.path = '/'

    else: 
        log.msg('\n')
        log.msg('ERROR: Query: %s' % parts)
        log.msg('Options:')
        log.msg('\t[sta]')
        log.msg('\t[orid]')
        log.msg('\t[sta],[orid]')
        log.msg('\t[orid],[sta]')
        log.msg('\t[sta],[chans],[orid]')
        log.msg('\t[sta],[start],[end]')
        log.msg('\t[sta],[chans],[start],[end]')
        log.msg('\n')

        request.redirect(request.prePathURL())
        request.finish()

    return
 
root = resource.Root()

root.putChild('data', resource.Data())

#root.putChild('wf', resource.Waveform())

root.putChild('jquery', static.File(config.jquery_dir))
root.putChild('static', static.File(config.static_dir))

rewrite_root = rewrite.RewriterResource(root, pathToArgs)

site = server.Site(rewrite_root)

site.displayTracebacks = config.display_tracebacks

application = service.Application(config.application_name)

internet.TCPServer(config.port, site).setServiceParent(application)
