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

root = resource.Root()

root.putChild('data',     resource.Data()        )
root.putChild('stations', resource.QueryParser() )
root.putChild('events',   resource.QueryParser() )
root.putChild('wfs',      resource.Waveform()    )

root.putChild('static',   static.File(config.static_dir))

rewrite_root = rewrite.RewriterResource(root)

site = server.Site(rewrite_root)

site.displayTracebacks = config.display_tracebacks

application = service.Application(config.application_name)

internet.TCPServer(config.port, site).setServiceParent(application)
