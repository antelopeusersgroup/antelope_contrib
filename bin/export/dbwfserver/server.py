"""main server resource for dbwfserver."""

from __main__ import config
import dbwfserver.resource as resource
from twisted.application import internet, service
from twisted.python.log import ILogObserver, PythonLoggingObserver
from twisted.web import server, static

for port, db in config.run_server.items():

    root = resource.QueryParserResource(config, db)

    root.putChild("static", static.File(config.static_dir))

    root.putChild("favicon.ico", resource.FaviconResource(config))

    site = server.Site(root)

    site.displayTracebacks = config.display_tracebacks

    application = service.Application("dbwfserver")

    observer = PythonLoggingObserver("dbwfserver.twisted.port" + str(port))

    application.setComponent(ILogObserver, observer.emit)

    sc = service.IServiceCollection(application)

    sc.addService(internet.TCPServer(int(port), site))
