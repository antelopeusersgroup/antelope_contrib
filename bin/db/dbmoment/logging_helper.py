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

from __main__ import *      # Get all the libraries from parent

#import os
#import sys
#import json
#import logging
#import inspect
#

def getLogger(name='', loglevel=False):

    # Define some name for this instance.
    main = os.path.basename( sys.argv[0] )
    inspectmain = os.path.basename( inspect.stack()[1][1] )

    # If none provided then use the name of the file
    # with script calling the function.
    if not name:
        name = inspectmain

    # If there is some main (parent) function using the
    # getLogger then prepend the name of main script.
    if not main == inspectmain:
        name = '%s.%s' % (main,name)
        return logging.getLogger(name)

    newlogger = logging.getLogger(name)

    if not len(newlogger.handlers):
        # We need new logger
        handler = logging.StreamHandler()
        formatter = logging.Formatter( '%(asctime)s %(name)s[%(levelname)s]: %(message)s')

        handler.setFormatter(formatter)
        newlogger.addHandler(handler)

        # Adding new logging level
        logging.addLevelName(35, "NOTIFY")

        if not loglevel:
            newlogger.setLevel( logging.getLogger(main).getEffectiveLevel() )
        else:
            newlogger.setLevel( logging.getLevelName( loglevel ) )

        def niceprint(msg):
            try:
                if isinstance(msg, str): raise
                return "\n%s" % json.dumps( msg, indent=4, separators=(',', ': ') )
            except:
                return msg

        def newcritical(self, message, *args, **kws):
            self.log(50, niceprint(message), *args, **kws)

        def newerror(self, message, *args, **kws):
            self.log(40, '***')
            self.log(40, '***')
            self.log(40, niceprint(message), *args, **kws)
            self.log(40, '***')
            self.log(40, '***')
            sys.exit( '\nExit from dbmoment with errors.\n' )

        def newnotify(self, message, *args, **kws):
            self.log(35, niceprint(message), *args, **kws)

        def newwarning(self, message, *args, **kws):
            self.log(30, niceprint(message), *args, **kws)

        def newinfo(self, message, *args, **kws):
            self.log(20, niceprint(message), *args, **kws)

        def newdebug(self, message, *args, **kws):
            self.log(10, niceprint(message), *args, **kws)

        def newkill(self, message, *args, **kws):
            self.log(50, '***')
            self.log(50, '***')
            self.log(50, niceprint(message), *args, **kws)
            self.log(50, '***')
            self.log(50, '***')
            sys.exit( '\nExit from dbmoment with errors.\n' )

        logging.Logger.critical = newcritical
        logging.Logger.error = newerror
        logging.Logger.notify = newnotify
        logging.Logger.info = newinfo
        logging.Logger.debug = newdebug
        logging.Logger.kill = newkill


    return newlogger


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
