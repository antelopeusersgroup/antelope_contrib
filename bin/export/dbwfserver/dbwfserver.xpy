import sys
import platform
import logging
logging.basicConfig()

from twisted.scripts.twistd import run
import dbwfserver.config as configuration

def system_print():
    """
    Print system info in case of errors

    """

    print
    print 'uname:', platform.uname()
    print
    print 'Version      :', sys.version_info
    print 'Version      :', platform.python_version()
    print 'Compiler     :', platform.python_compiler()
    print 'Build        :', platform.python_build()
    print
    print 'system   :', platform.system()
    print 'node     :', platform.node()
    print 'release  :', platform.release()
    print 'version  :', platform.version()
    print 'machine  :', platform.machine()
    print 'processor:', platform.processor()

def main():
    """
    Run the twisted reactor, overriding the twistd class with new args

    """

    # Refer to global config var, necessary for twistd override
    global config

    logger=logging.getLogger('dbwfserver')

    #Configure system with command-line flags and pf file values.
    config = configuration.Config_Server()
    sys.argv = config.configure()

    if config.verbose:
        logger.setLevel(logging.INFO)

    if config.debug:
        logger.setLevel(logging.DEBUG)

    logger.info('Start Server!')

    run()

if __name__ == '__main__':
    main()
