"""
nagios_check_dbmaster

@author   Rob Newman <robertlnewman@gmail.com>
@package  Antelope (Datascope, Orb)
@version  1.0
@notes    1. Follow Nagios plugin guidelines:
             http://nagiosplug.sourceforge.net/developer-guidelines.html
          2. Python elog routines still do not work as advertised - add
             caveat to the man page and redirect output to /dev/null
"""
from optparse import OptionParser
import antelope.datascope as antdb
import antelope.orb as antorb
import antelope.stock as antstock
import antelope.elog as antelog
import antelope.Pkt as antPkt

# Until the elog routines can 
# be captured in a try/except 
# clause, redirect elog messages to /dev/null
os.environ['ELOG_DELIVER'] = '/dev/null'

class NagiosCheckDbmaster():
    """Class definition
    for Nagion plugin for
    dbmaster checks
    """
    # {{{

    def __init__(self):
        # {{{
        antelog.elog_init(sys.argv)
        self.invalid = []
        self.errors = []
        self.warnings = []
        # }}}

    def parse_command_line(self):
        """Parse the command line
        options.
        """
        # {{{
        usage = "Usage: %prog [-v] [-V] host:orb database"
        parser = OptionParser(usage=usage, version="%prog 1.0")
        parser.add_option("-v", "--verbose", dest="verbose", help="verbose output", action="store_true", default=False)
        parser.add_option("-V", "--veryverbose", dest="debug", help="very verbose output (debug)", action="store_true", default=False)
        (options, args) = parser.parse_args()
        if len(args) != 2:
            parser.error("Must provide a valid orbname (host:orb) and dbmaster path")
        if not (options.verbose or options.debug):
            verbose = 0
        else:
            if options.verbose:
                verbose = 1
            elif options.debug:
                verbose = 2
        self.verbose = verbose
        self.orb = args[0]
        self.db = args[1]
        # }}}

    def check_orb(self):
        """Check we can
        open the orb"""
        # {{{
        try:
            orb = antorb.orbopen(self.orb, 'r')
        except Exception, e:
            (self.invalid).append("check_orb() [Exception]: %s" % e)
        else:
            if orb._orbfd == None:
                (self.invalid).append("check_orb() [TypeError]: Orb name '%s' invalid" % self.orb)
            else:
                return orb 
        # }}}

    def get_orb_sources_stations(self, orbptr):
        """Return a list of
        all orb sources
        """
        # {{{
        station_source_list = []
        try:
            when, sources = antorb.orbsources(orbptr)
        except ValueError, e:
            (self.invalid).append("get_orb_sources_stations() [ValueError]: %s" % e)
        except Exception, e:
            (self.invalid).append("get_orb_sources_stations() [Exception]: %s" % e)
            return False
        else:
            for s in sources:
                (net, sta, chan, loc, suffix, subcode) = antPkt.split_srcname(s['srcname'])
                if sta != '' and sta not in station_source_list:
                    station_source_list.append(sta)
            station_source_list.sort()
            return station_source_list
        # }}}

    def check_db(self):
        """Check we can 
        view the db"""
        # {{{
        if not os.path.exists(self.db):
            (self.invalid).append("check_db() [Exception]: Database '%s' does not exist" % self.db)
            return False
        else:
            try:
                db = antdb.dbopen(self.db, 'r')
            except Exception, e:
                (self.invalid).append("check_db() [Exception]: %s" % e)
                return False
            else:
                return db
        # }}}

    def get_db_active_stations(self, dbptr):
        """Return a list of 
        all the active stations 
        in the database"""
        # {{{
        sta_list = []
        try:
            active_db = antdb.dblookup(dbptr, '', 'site', '', '');
        except Exception, e:
            (self.invalid).append("get_db_active_stations() [Exception]: %s" % e)
            return False
        else:
            active_db.join('snetsta')
            active_db.join('deployment')
            active_db.subset('offdate == NULL || offdate >= now()')
            active_db.subset('endtime >= now()')
            active_db.subset('time <= now()')
            active_db.sort('sta')
            if self.verbose > 1:
                (self.warnings).append("Number of records: %s" % active_db.query('dbRECORD_COUNT'))
            for i in range(active_db.query('dbRECORD_COUNT')):
                active_db[3] = i
                sta_list.append(active_db.getv('sta')[0])
        return sta_list
        # }}}

    def membership_test(self, orblist, dblist):
        """Test the membership
        of elements in each list
        """
        # {{{
        set_orb = set(orblist)
        set_db = set(dblist)
        if set_orb != set_db:
            if len(set_orb.difference(set_db)) > 0:
                err_msg = 'Missing dbmaster records error: ' + ', '.join(set_orb.difference(set_db))
                (self.errors).append(err_msg)
            if len(set_db.difference(set_orb)) > 0:
                err_msg = 'Missing orb sources error: ' + ', '.join(set_db.difference(set_orb))
                (self.warnings).append(err_msg)
        # }}}

    def output_status(self):
        """Parse the errors
        and output a Nagios
        friendly service status
        """
        # {{{
        if len(self.invalid) > 0:
            service_status = "Unknown"
            if self.verbose > 0:
                service_status += "\nList of invalid command line arguments or low-level failures internal to the plugin:\n"
                for i in self.invalid:
                    service_status += "\t - %s\n" % i
        else:
            if len(self.errors) > 0:
                service_status = "Critical"
                if self.verbose > 0:
                    service_status += "\nList of errors:\n"
                    for e in self.errors:
                        service_status += "\t - %s\n" % e
            else:
                if len(self.warnings) > 0:
                    service_status = "Warning"
                    if self.verbose > 0:
                        service_status += "\nList of warnings:\n"
                        for w in self.warnings:
                            service_status += "\t - %s\n" % w
                else:
                    service_status = "OK"
        return service_status
        # }}}

    # }}}

def main():
    """Main processing
    """
    sources = False
    active_stas = False

    test = NagiosCheckDbmaster()
    test.parse_command_line()

    orbptr = test.check_orb()
    if orbptr:
        sources = test.get_orb_sources_stations(orbptr)

    dbptr = test.check_db()
    if dbptr:
        active_stas = test.get_db_active_stations(dbptr)

    if sources and active_stas:
        test.membership_test(sources, active_stas)

    # Insert other tests here

    print test.output_status()
    return 0

if __name__ == '__main__':
   status = main()
   sys.exit(status)
