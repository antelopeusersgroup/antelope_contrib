from __main__ import *

class Config_Server():
    def __init__(self):
#{{{
        self.event               = 'false'
        self.pfname              = 'dbwfserver'
        self.style               = 'cupertino'
        self.nickname            = ''
        self.application_title   = ''
        self.static_dir          = ''
        self.jquery_dir          = ''
        self.template            = ''
        self.plot_template       = ''
        self.local_data          = ''
        self.antelope            = ''
        self.dbname              = ''
        self.proxy_url           = ''
        self.port                = -1
        self.max_traces          = -1
        self.max_points          = -1
        self.realtime            = 'false'
        self.apply_calib         = False
        self.display_tracebacks  = False
        self.display_arrivals    = True
        self.display_points      = False
        self.verbose             = False
        self.debug               = False
        self.daemonize           = False
        self.import_paths        = ()
        self.default_time_window = -1
        self.filters             = []
        self.run_server          = {}

        try:
            opts, pargs = getopt.getopt(sys.argv[1:], 'dp:P:vVern:')
        except getopt.GetoptError:
            self.usage()
            sys.exit(-1)

        if( len(pargs) == 1):

            self.dbname = pargs[0]

        for option, value in opts:

            if '-e' in option:
                self.event = 'true'

            if '-p' in option:
                self.pfname = str(value)

            if '-r' in option:
                self.realtime = 'true'

            if '-d' in option:
                self.daemonize = True

            if '-V' in option:
                self.debug = True
                self.verbose = True

            if '-v' in option:
                self.verbose = True

            if '-P' in option:
                self.port = int(value)

            if '-n' in option:
                self.nickname = str(value)

        #
        # Get values from pf file
        #
        if self.port == -1:
            self.port = stock.pfget_int( self.pfname, "port" )

        try:
            self.max_points = stock.pfget_int( self.pfname, "max_points" )
        except:
            pass
        try:
            self.max_traces = stock.pfget_int( self.pfname, "max_traces" )
        except:
            pass
        try:
            self.jquery_dir = stock.pfget_string( self.pfname, "jquery_dir" )
        except:
            pass
        try:
            self.static_dir = stock.pfget_string( self.pfname, "static_dir" )
        except:
            pass
        try:
            self.template = stock.pfget_string( self.pfname, "template" )
        except:
            pass
        try:
            self.plot_template = stock.pfget_string( self.pfname, "plot_template" )
        except:
            pass
        try:
            self.local_data = stock.pfget_string( self.pfname, "local_data" )
        except:
            pass
        try:
            self.style = stock.pfget_string( self.pfname, "jquery_ui_style" )
        except:
            pass
        try:
            self.antelope = stock.pfget_string( self.pfname, "antelope" )
        except:
            pass
        try:
            self.application_title = stock.pfget_string( self.pfname, "application_title" )
        except:
            pass
        try:
            self.proxy_url = stock.pfget_string( self.pfname, "proxy_url" )
        except:
            pass
        try:
            self.apply_calib = stock.pfget_boolean( self.pfname, "apply_calib" )
        except:
            pass
        try:
            self.display_tracebacks = stock.pfget_boolean( self.pfname, "display_tracebacks" )
        except:
            pass
        try:
            self.display_arrivals = stock.pfget_boolean( self.pfname, "display_arrivals" )
        except:
            pass
        try:
            self.display_points = stock.pfget_boolean( self.pfname, "display_points" )
        except:
            pass
        try:
            self.default_time_window = stock.pfget_tbl( self.pfname, "default_time_window" )
        except:
            pass
        try:
            self.filters = list(stock.pfget_tbl( self.pfname, "filters" ) )
        except:
            pass
        try:
            self.import_paths = stock.pfget_tbl( self.pfname, "import_paths" )
        except:
            pass

#}}}

    def configure(self):
#{{{
        # 
        # Expand paths
        #
        for p in self.import_paths:
            log.msg('Expnding path: %s' % p)
            sys.path.insert(0, p)

        # 
        # Fix paths
        #
        self.template = ("%s/%s/%s" % (self.antelope,self.local_data,self.template))
        self.plot_template = ("%s/%s/%s" % (self.antelope,self.local_data,self.plot_template))
        self.jquery_dir = ("%s/%s/%s" % (self.antelope,self.local_data,self.jquery_dir))
        self.static_dir = ("%s/%s/%s" % (self.antelope,self.local_data,self.static_dir))

        # 
        # Sanity check
        #
        if not os.path.isfile(self.template):
            sys.exit('\n\nERROR: No template found (%s)\n'% self.template)


        # Build dictionary of servers we want to run 
        if self.dbname and self.port:

            # only one for now
            self.run_server = { int(self.port):str(self.dbname) }

        else:

            self.usage()
            sys.exit('\n\nERROR: Not a valid db:port setup. (%s,%s)\n' % (self.dbname,self.port))


        argv_remap = list()
        argv_remap.append(sys.argv[0])

        if(not self.daemonize):
            argv_remap.append("-n")

        argv_remap.append("-y")
        argv_remap.append(os.path.join(os.environ['ANTELOPE'], 'local/data/python/dbwfserver/server.py'))

        if os.path.isdir('./state'):
            pid_path = './state'
        else:
            pid_path = '/tmp'
        argv_remap.append("--pidfile")
        argv_remap.append( pid_path+'/dbwfserver_'+str(os.getpid())+'.pid' )

        return argv_remap
#}}}
    def usage(self):

        print "\n\tUsage: dbwfserver [-drevV] [-n nickname] [-p pfname] [-P port] dbname\n"

    def __getattr__(self,attrname):
#{{{
        if attrname == "event": return self.event
        if attrname == "pfname": return self.pfname
        if attrname == "nickname": return self.nickname
        if attrname == "style": return self.style
        if attrname == "application_title": return self.application_title
        if attrname == "static_dir": return self.static_dir
        if attrname == "jquery_dir": return self.jquery_dir
        if attrname == "template": return self.template
        if attrname == "plot_template": return self.plot_template
        if attrname == "local_data": return self.local_data
        if attrname == "antelope": return self.antelope
        if attrname == "proxy_url": return self.proxy_url
        if attrname == "port": return self.port
        if attrname == "max_points": return self.max_points
        if attrname == "max_traces": return self.max_traces
        if attrname == "apply_calib": return self.apply_calib
        if attrname == "display_tracebacks": return self.display_tracebacks
        if attrname == "display_arrivals": return self.display_arrivals
        if attrname == "display_points": return self.display_points
        if attrname == "verbose": return self.verbose
        if attrname == "debug": return self.debug
        if attrname == "daemonize": return self.daemonize
        if attrname == "import_paths": return self.import_paths
        if attrname == "default_time_window": return self.time_window
        if attrname == "filters": return self.filters
        if attrname == "run_server": return self.run_server
        if attrname == "realtime": return self.realtime

        raise AttributeError, attrname
#}}}

    def __setattr__(self,attr,value):

        try:
            self.__dict__[attr] = value
        except:
            raise AttributeError, attr + ' not allowed'



