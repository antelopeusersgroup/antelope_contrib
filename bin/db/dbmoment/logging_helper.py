#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.



# Try to make a generic logging setup for dbmoment.
# This function will return an object
# of the logging class. If none available with
# requested name then it will configure one for
# you.

# Import like this...

#       from dbmoment.logging import getLogger

# Then create a new object like this...

#       option 1) logging = getLogger()
#       option 2) logging = getLogger(self.__class__.__name__)

# You can then log strings to console using any of the
# provided methods.
#   -------------------------- allways prints
#   logging.critical(obj)
#   logging.critical('test')
#   logging.error(obj)
#   logging.error('test')
#   logging.warning(obj)
#   logging.warning('test')
#   logging.notify(obj)
#   logging.notify('test')
#   -------------------------- verbose mode or greater
#   logging.info(obj)
#   logging.info('test')
#   -------------------------- debug mode or greater
#   logging.debug(obj)
#   logging.debug('test')

def getLogger(name=None, loglevel=False, parent=False, log_filename=None, log_max_count=10):

    import os, sys, inspect
    import logging as log_object
    import logging.handlers as handlers

    # Define some name for this instance.
    main = os.path.basename( sys.argv[0] )
    inspectmain = os.path.basename( inspect.stack()[1][1] )

    if not name:
        # If none provided then use the name of the file
        # with script calling the function.
        name = inspectmain


    # If there is some main (parent) function using the
    # getLogger then prepend the name of main script.
    if not main == inspectmain:
        name = '%s.%s' % (main,name)
        return log_object.getLogger(name)


    newlogger = log_object.getLogger(name)

    if not len(newlogger.handlers):
        # We need new logger


        newlogger.propagate = False

        if loglevel:
            newlogger.setLevel( log_object.getLevelName( loglevel ) )
        else:
            newlogger.setLevel( log_object.getLevelName( 'WARNING' ) )

        formatter = log_object.Formatter( '%(asctime)s %(name)s[%(levelname)s]: %(message)s')


        # Adding new logging level
        log_object.addLevelName(35, "NOTIFY")


        def niceprint(msg):
            try:
                if isinstance(msg, str): raise
                return "\n%s" % json.dumps( msg, indent=4, separators=(',', ': ') )
            except:
                return msg

        def newcritical(self, message, *args, **kws):
            self.log(50, niceprint(message), *args, **kws)

        def newerror(self, message, *args, **kws):
            self.log(40, niceprint(message))

            if len(args):
                sys.exit( args[0] )

            sys.exit( 33 )

        def newnotify(self, message, *args, **kws):
            self.log(35, niceprint(message), *args, **kws)

        def newwarning(self, message, *args, **kws):
            self.log(30, niceprint(message), *args, **kws)

        def newinfo(self, message, *args, **kws):
            self.log(20, niceprint(message), *args, **kws)

        def newdebug(self, message, *args, **kws):
            self.log(10, niceprint(message), *args, **kws)

        def newkill(self, message, *args, **kws):
            self.log(50, niceprint(message), *args, **kws)
            sys.exit( 3 )

        log_object.Logger.critical = newcritical
        log_object.Logger.error = newerror
        log_object.Logger.notify = newnotify
        log_object.Logger.info = newinfo
        log_object.Logger.debug = newdebug
        log_object.Logger.kill = newkill

        # Print to screen
        stream_handler = log_object.StreamHandler()
        stream_handler.setFormatter(formatter)

        # Maybe we want to log to a file
        if log_filename:

            if int(log_max_count):
                count = int(log_max_count)
            else:
                count = 5
            file_handler = handlers.RotatingFileHandler( log_filename, backupCount=count)

            # Check if log exists and should therefore be rolled
            if os.path.isfile(log_filename): file_handler.doRollover()

            file_handler.setFormatter(formatter)

        if log_filename: newlogger.addHandler(file_handler)
        newlogger.addHandler(stream_handler)


    return newlogger


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
