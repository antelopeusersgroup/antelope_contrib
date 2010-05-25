import sys
import os
import re

from twisted.web import server, static, rewrite
from twisted.application import service, internet

import dbwfserver.config as config
import dbwfserver.resource as resource


root = resource.QueryParser()

root.putChild('static',   static.File(config.static_dir))

rewrite_root = rewrite.RewriterResource(root)

site = server.Site(rewrite_root)

site.displayTracebacks = config.display_tracebacks

application = service.Application(config.application_name)

internet.TCPServer(config.port, site).setServiceParent(application)
