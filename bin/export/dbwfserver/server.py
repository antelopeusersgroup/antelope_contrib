from __main__ import config
import os
import resource
import twisted.web
import twisted.application

for port,db  in config.run_server.items():

    root = resource.QueryParser(config, db)

    root.putChild('static', twisted.web.static.File(config.static_dir))

    #
    # favicon.ico
    #
    favicon = twisted.web.static.File(os.path.join(config.static_dir, 'images/favicon.ico'),
                                    defaultType='image/vnd.microsoft.icon')
    root.putChild('favicon.ico', favicon)

    site = twisted.web.server.Site(root)

    site.displayTracebacks = config.display_tracebacks

    application = twisted.application.service.Application('dbwfserver')

    sc = twisted.application.service.IServiceCollection(application)

    sc.addService(twisted.application.internet.TCPServer(int(port), site))
