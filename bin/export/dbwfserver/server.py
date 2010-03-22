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

    else: 
        log.msg('\n')
        log.msg('ERROR: Query: %s' % parts)
        log.msg('Options:')
        log.msg('\t[sta]')
        log.msg('\t[orid]')
        log.msg('\t[sta],[orid]')
        log.msg('\t[orid],[sta]')
        log.msg('\t[sta],[chan],[orid]')
        log.msg('\t[sta],[start],[end]')
        log.msg('\t[sta],[chan],[start],[end]')
        log.msg('\n')

        request.redirect(request.prePathURL())
        request.finish()

    return
 
root = resource.Root()

root.putChild('data',     resource.Data())
root.putChild('stations', resource.QueryParser())
root.putChild('events',   resource.QueryParser())
root.putChild('wfs',      resource.QueryParser())

# root.putChild('wf', resource.Waveform())

root.putChild('jquery',   static.File(config.jquery_dir))
root.putChild('static',   static.File(config.static_dir))

# rewrite_root = rewrite.RewriterResource(root, pathToArgs)
rewrite_root = rewrite.RewriterResource(root)

site = server.Site(rewrite_root)

site.displayTracebacks = config.display_tracebacks

application = service.Application(config.application_name)

internet.TCPServer(config.port, site).setServiceParent(application)
