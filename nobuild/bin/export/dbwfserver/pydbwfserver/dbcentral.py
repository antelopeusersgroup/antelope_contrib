"""
Utilities to work with dbcentral.

Dbcentral will load the database and test for
the existence of the 'clusters' table first. If missing
the class will assume that the intention of the user
is to load a regular database and use the class
functionality to store the pointer. This will trigger
the self.type value to be "masquerade". Regular
dbcentral tables will load normally and set
self.type to "dbcentral".


@author: Juan Reyes <support@anf.ucsd.edu>
@author: Geoff Davis <gadavis@ucsd.edu>

@usage:

  Create:
     element = Dbcentral(path)
     element = Dbcentral(path,nickname)

     # Enable Debug mode. Verbose output
     element = Dbcentral(path,nickname,True)

     # verify each db contains specifed tables with data in them
     element = Dbcentral(path,nickname,False,['wfdisc','sitechan')

  Access:
     # Nice print of values
     print(element)

     # return string value for mode [dbcentral,masquerade]
     element.type

     # return string value for path
     element.path

     # return string value for nickname
     element.nickname
     j
     # return station_patterns of databases
     element.station_patterns()

     # return database matching the epoch time
     element(epoch)

     # remove database from class.
     element.purge(db)
"""

import glob
import logging
import os
import signal
import sys

from six import string_types

from antelope import datascope, stock

if __name__ == "__main__":
    # Conditionally add in paths for finding antelope modules
    """
    Test the class
    """
    signal.signal(signal.SIGINT, signal.SIG_DFL)

    sys.path.append(os.environ["ANTELOPE"] + "/data/python")
    sys.path.append(os.environ["ANTELOPE"] + "contrib/data/python")


class DbcentralException(Exception):
    """Base exception type for Dbcentral."""

    def __init__(self, msg):
        """Initialize DbcentralException."""
        self.msg = msg

    def __repr__(self):
        """Make a copy of self."""
        return "DbcentralException(msg=%r)" % self.msg


class UnknownVolumeTypeException(DbcentralException):
    """The Dbcentral cluster database contains an unknown Volume type."""

    def __init__(self, volume_type):
        """Initialize class."""
        self.volumetype = volume_type
        msg = 'Volume type "%s" in cluster database not understood' % volume_type
        DbcentralException.__init__(self, msg=msg)

    def __repr__(self):
        """Make a copy of self."""
        return "UnknownVolumeTypeException(volume_type=%r)" % self.volumetype


class NoDatabaseException(DbcentralException):
    """The Dbcentral database contains no valid databases."""

    def __init__(self):
        """Initialize class."""
        msg = "No valid databases in dbcentral"
        DbcentralException.__init__(self, msg)


class Dbcentral:
    """
    Represent the contents of an Antelope DBCentral database.

    DBCentral databases are a meta-station_patterns of other Antelope databases.
    They allow a large database to be split up into a collection of smaller
    databases.

    See also: :manpage:`dbcentral(1)`
    """

    def __init__(self, path, nickname=None, debug=False, required_tables=None):
        """Initialize the Dbcentral object.

        @param path is the path on disk to the dbcentral database descriptor
        file. If the database specified is not a dbcentral database, this class
        goes into "masquerade" mode where it will contain only a single
        database, but still behave like a Dbcentral object.

        @param nickname is the task nickname, useful to filter databases by type.
        REQUIRED when path is a dbcentral database instead of a waveform or
        other type of database

        @param debug sets the logging level

        @param required_tables is a station_patterns of table names to validate in each database.
        A database is automatically dropped from the station_patterns of available
        databases if it does not contain these tables, or if those tables do
        not have any records.
        """
        self.type = False
        self.path = os.path.abspath(path)
        self.nickname = nickname
        self.debug = debug

        if required_tables is None:
            self.required_tables = []
        else:
            if isinstance(required_tables, string_types):
                if len(required_tables) > 0:
                    self.required_tables = [required_tables]
                else:
                    self.required_tables = []
            else:
                self.required_tables = required_tables
        assert not isinstance(self.required_tables, string_types)

        self.glob = glob.glob

        self.logger = logging.getLogger(__name__)

        if self.debug:
            self.logger.setLevel(logging.DEBUG)

        # Create dictionary to hold all the values
        self.logger.debug(
            "init(): path:%s nickname:%s debug:%s"
            % (self.path, self.nickname, self.debug)
        )
        self.dbs = {}

        # Load the dbs
        self.logger.debug("_get_list(): ")
        self._get_list()

    def __repr__(self):
        """Duplicate self."""
        format_string = (
            "{classname}(dbname={dbname}, nickname={nickname}, debug={debug},"
        )
        " required_tables={required_tables}, type={type}, dbs={dbs})"

        return format_string.format(
            classname=self.__class__.__name__,
            dbname=self.path,
            nickname=self.nickname,
            debug=self.debug,
            required_tables=",".join(self.required_tables.join),
            type=self.type,
            dbs=",".join(self.dbs),
        )

    def __str__(self):
        """Generate string for end-user/application display of content."""
        start = "An instance of the %s class at %r containing %d databases: " % (
            self.__class__.__name__,
            self.path,
            len(self.dbs),
        )
        middle = " ,".join(self.dbs.keys())
        return start + middle

    def info(self):
        """Print log messages with information about this class."""
        self.logger.info("Dbcentral.nickname => %s" % self.nickname)
        self.logger.info("Dbcentral.type => %s" % self.type)
        self.logger.info("Dbcentral.path => %s" % self.path)
        self.logger.info("Dbcentral.station_patterns => %s" % self.list())
        for element in sorted(self.dbs):
            self.logger.info("%s => %s" % (element, self.dbs[element]["times"]))

    def __call__(self, time=stock.now()):
        """
        Intercept data requests.

        Default value of time is "now"
        """

        try:
            time = float(time)
        except ValueError:
            print(
                "\n*Dbcentral*: Dbcentral() => error in time=>[%s] %s"
                % (time, time.__class__,)
            )
        else:
            for element in sorted(self.dbs):
                start = self.dbs[element]["times"][0]
                end = self.dbs[element]["times"][1]
                if start < time < end:
                    return element

        raise DbcentralException("No db match for time=>[%s]" % time)

    def _get_list(self):
        try:
            db = datascope.dbopen(self.path, "r")
        except Exception as e:
            raise DbcentralException("Cannot open database %s (%s)" % (self.path, e))

        try:
            db = db.lookup("", "clusters", "", "")
        except datascope.DblookupFieldError:
            self.type = "masquerade"
            self.nickname = None
            self.dbs[self.path] = {"times": [-10000000000.0, 10000000000.0]}
            self.logger.info("Not a dbcentral database. Set single database.")
            return

        else:
            self.type = "dbcentral"
            if self.nickname is None:
                raise ValueError("Need nickname for Dbcentral clustername regex.")

        try:
            db = db.lookup("", "clusters", "", "dbNULL")
            null_time, null_endtime = db.getv("time", "endtime")
        except Exception as e:
            raise DbcentralException(
                "Cannot look up null values in clusters table. (%s)" % e
            )

        expr = "clustername =='%s'" % self.nickname

        try:
            db = db.subset(expr)
        except Exception as e:
            raise DbcentralException("Cannot subset on clustername. %s" % e)

        try:
            db = db.sort("time")
            nclusters = db.record_count
        except Exception as e:
            raise DbcentralException("Cannot sort on 'time' . %s" % e)

        if nclusters < 1:
            raise DbcentralException('No matches for nickname "%s".' % self.nickname)

        self.logger.debug("Records=%s" % nclusters)

        for i in range(nclusters):
            self.logger.debug("db.record=%s" % i)
            db.record = i

            try:
                dbname_template = db.extfile()[-1]
            except Exception as e:
                raise DbcentralException("Cannot run db.extfile(). %s" % e)

            self.logger.debug("dbname_template=%s" % dbname_template)

            try:
                volumes, net, time, endtime = db.getv(
                    "volumes", "net", "time", "endtime"
                )
            except Exception as e:
                raise DbcentralException(
                    "Problems with db.getv('volumes','net',"
                    + "'time','endtime'). (%s)\n" % e
                )

            self.logger.debug("volumes=%s" % volumes)
            self.logger.debug("net=%s" % net)
            self.logger.debug("time=%s" % time)
            self.logger.debug("endtime=%s" % endtime)

            if endtime == null_endtime:
                # This will be problematic with realtime systems
                endtime = stock.now()

            self.logger.debug("endtime=%s" % endtime)

            start_year = int(stock.epoch2str(time, "%Y"))
            end_year = int(stock.epoch2str(endtime, "%Y"))
            start_month = int(stock.epoch2str(time, "%L"))
            end_month = int(stock.epoch2str(endtime, "%L"))

            if volumes == "single":

                dbname = stock.epoch2str(time, dbname_template)
                self._test_db(time, dbname)

            elif volumes == "year":

                for y in range(start_year, end_year + 1):
                    voltime = stock.str2epoch("1/1/%s 00:00:00" % y)
                    dbname = stock.epoch2str(voltime, dbname_template)

                    self._test_db(voltime, dbname)

            elif volumes == "month":

                vol_month = start_month
                vol_year = start_year

                while vol_year < end_year or (
                    vol_year == end_year and vol_month <= end_month
                ):

                    voltime = stock.str2epoch("%d/1/%d" % (vol_month, vol_year))

                    if vol_month < 12:
                        vol_month = vol_month + 1
                    else:
                        vol_year = vol_year + 1
                        vol_month = 1

                    dbname = stock.epoch2str(int(voltime), dbname_template)

                    self._test_db(voltime, dbname)

            elif volumes == "day":

                start_day = int(stock.yearday(time))
                end_day = int(stock.yearday(endtime))

                vol_day = start_day

                while vol_day <= end_day:

                    voltime = stock.epoch(vol_day)
                    dbname = stock.epoch2str(voltime, dbname_template)

                    if self._test_db(voltime, dbname):
                        self.dbs[dbname] = {"times": [time, endtime]}

                    vol_day = stock.yearday((stock.epoch(vol_day) + 86400))

            else:
                raise UnknownVolumeTypeException(volumes)

        self.logger.debug("DBS=%s" % self.dbs.keys())

    def _test_db(self, time, dbname):
        """
        Verify that the db is valid before saving the value.

        Skips databases that don't match the criteria specified. If
        self.verifytables is set, extra tests are performed to ensure that the
        database contains the requested tables, and that the tables contain
        data.
        """

        self.logger.debug("Test for time=%s =>> %s" % (time, dbname))

        # if os.path.isfile(dbname):
        #    self.dbs[dbname] = {'times': [time,endtime]}
        #    return

        # if self.glob("%s.*" % dbname):
        #    self.dbs[dbname] = {'times': [time,endtime]}
        #    self.logger_instance.warning( "No descriptor file for (%s)." % dbname )
        #    return
        db = None
        dbtbl = None
        try:
            db = datascope.dbopen(dbname, "r")
        except datascope.DatascopeError:
            self.logger.error("Cannot dbopen %s, skipping." % dbname)
            return False
        else:
            for table in self.required_tables:
                try:
                    dbtbl = db.lookup(table=table)
                    try:
                        dbtbl.query(datascope.dbTABLE_PRESENT)
                        records = dbtbl.query(datascope.dbRECORD_COUNT)
                        if not records:
                            self.logger.error(
                                "%s.%s is an empty table. Skipping db."
                                % (dbname, table)
                            )
                            return False
                    except datascope.DatascopeError:
                        self.logger.error(
                            "The table %s.%s is not present. Skipping db."
                            % (dbname, table)
                        )
                        return False
                finally:
                    if dbtbl is not None:
                        datascope.dbfree(dbtbl)
        finally:
            if db is not None:
                datascope.dbclose(db)

        # If we get here, the database passes our tests. Add it to the station_patterns
        return True

    def after(self, time):
        """Get the rest of the databases after the designated timestamp."""

        dbs_after = []

        try:
            time = float(time)
        except ValueError:
            self.logger.error("error in time=>[%s] %s" % (time, time.__class__))
        else:
            for element in sorted(self.dbs):
                start = self.dbs[element]["times"][0]
                end = self.dbs[element]["times"][1]
                if time < start and time < end:
                    dbs_after.extend([element])

        return dbs_after

    def list(self):
        """Get the databases contained within the dbcentral class."""

        try:
            return sorted(self.dbs.keys())
        except Exception:
            raise NoDatabaseException()

    def purge(self, tbl):
        """Cean Dbcentral object by removing a database."""
        self.logger.debug("Dbcentral.purge() => %s" % tbl)

        try:
            del self.dbs[tbl]
        except ValueError:
            pass


def main():
    """Run tests on the Antelope demo database.

    This test function will run if the file is called directly

    Opens the Antelope demo database and runs some tests on it
    """
    import logging
    from pprint import pprint

    logging.basicConfig()
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)

    time = 1262404000.00000

    dbcntl = Dbcentral(
        "/opt/antelope/data/db/demo/demo",
        debug=True,
        required_tables=["wfdisc", "stachan"],
    )

    print("dbcntl.__repr__() = %r" % dbcntl)
    pprint(dbcntl, indent=10, width=1, depth=1)
    pprint(dbcntl.dbs, indent=10, width=1, depth=None)
    print("dbcntl.str__() is %s" % dbcntl)
    print("dbcntl(%s) == %s" % (time, dbcntl(time)))
    try:
        dbcntl.purge("test")
    except Exception as e:
        logger.info("dbcntl.purge(%s) => %s" % ("test", e))

    print("Done with Dbcentral demo.")


if __name__ == "__main__":
    main()
