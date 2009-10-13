import sys
import os

from twisted.web import server, static
from twisted.application import service, internet

import antelope.stock as stock

import dbwfserver.config as config
import dbwfserver.resource as resource

root = resource.Root()

root.putChild('data', resource.Data())

root.putChild('jquery', static.File(config.jquery_dir))
root.putChild('static', static.File(config.static_dir))

site = server.Site(root)

site.displayTracebacks = config.display_tracebacks

application = service.Application(config.application_name)

internet.TCPServer(config.port, site).setServiceParent(application)
