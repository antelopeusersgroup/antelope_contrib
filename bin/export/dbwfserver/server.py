from __main__ import config
import os
import dbwfserver.resource as resource
from twisted.application import internet, service
from twisted.web import server, static
from twisted.python.log import ILogObserver,PythonLoggingObserver

for port,db  in config.run_server.items():

    root = resource.QueryParser(config, db)

    root.putChild('static', static.File(config.static_dir))

    #
    # favicon.ico
    #
    favicon = static.File(
        os.path.join( config.static_dir, 'images/favicon.ico'),
        defaultType='image/vnd.microsoft.icon')

    root.putChild('favicon.ico', favicon)

    site = server.Site(root)

    site.displayTracebacks = config.display_tracebacks

    application = service.Application('dbwfserver')

    observer = PythonLoggingObserver('dbwfserver.twisted.port'+str(port))

    application.setComponent(ILogObserver, observer.emit)

    sc = service.IServiceCollection(application)

    sc.addService(internet.TCPServer(int(port), site))
