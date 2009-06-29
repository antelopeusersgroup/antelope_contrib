from twisted.scripts.twistd import run

import dbwfserver.config as config

sys.argv = config.configure(sys.argv)

run()
