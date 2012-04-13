"""
dbcentral.py

@author:   Rob Newman <robertlnewman@gmail.com> 858.822.1333
@created:  2010-08-11
@modified: 2011-01-25
@notes:    1. Open up clusters table, create dictionary or list, return
           2. Add unit testing
"""
import os
import sys
import time
import datascope as antdb

if '5.2' in os.environ['ANTELOPE']:
    import stock as antstock
    import elog as antelog
    antelog.elog_init(sys.argv)
    legacy = False
else:
    import stock as antstock
    antstock.elog_init(sys.argv)
    legacy = True

class DbCentral:

    def __init__(self, path, clustername, include_times=False):
        """Initialize class
        """
        self.path = path
        self.clustername = clustername
        self.include_times = include_times
        self.errstr = antstock.epoch2str(antstock.now(), "%g %H:%M:%S")
        self.base = os.path.basename(__file__)

    def year_resolver(self, start_yr, end_yr, dbname_template):
        """Year list
        """
        if self.include_times:
            years = {}
        else:
            years = []
        for y in range(start_yr, end_yr + 1):
            voltime = antstock.str2epoch("1/1/%s 00:00:00" % y)
            volendtime = antstock.str2epoch("12/31/%s 23:59:59" % y)
            dbname = antstock.epoch2str(voltime, dbname_template)
            if os.path.exists(dbname) and os.path.isfile(dbname):
                if self.include_times:
                    years[dbname] = (voltime,volendtime)
                else:
                    years.append(dbname)
            else:
                if not legacy:
                    antelog.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
                else:
                    antstock.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
        return years

    def month_resolver(self, start_mth, start_yr, end_mth, end_yr, dbname_template):
        """Month list
        """
        if self.include_times:
            months = {}
        else:
            months = []
        vol_month = start_mth
        vol_year = start_yr
        while vol_year <= end_yr or (vol_year == end_yr and vol_month <= end_mth):
            voltime = antstock.str2epoch("%d/1/%d" % (vol_month, vol_year))
            if vol_month < 12:
                vol_month += 1
            else:
                vol_month = 1
                vol_year += 1
            volendtime = antstock.str2epoch("%d/1/%d" % (vol_month,vol_year)) - 1
            dbname = antstock.epoch2str(int(voltime), dbname_template)
            if os.path.exists(dbname) and os.path.isfile(dbname):
                if self.include_times:
                    months[dbname] = (voltime,volendtime)
                else:
                    months.append(dbname)
            else:
                if not legacy:
                    antelog.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
                else:
                    antstock.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
        return months

    def day_resolver(self, start_dy, end_dy, dbname_template):
        """Day list
        """
        if self.include_times:
            days = {}
        else:
            days = []
        while start_dy <= end_dy:
            voltime = antstock.epoch(start_day)
            volendtime = voltime + 86399 # one second less than a full day
            dbname = antstock.epoch2str(voltime, dbname_template)
            if os.path.exists(dbname) and os.path.isfile(dbname):
                if self.include_times:
                    days[dbname] = (voltime, volendtime)
                else:
                    days.append(dbname)
            else:
                if not legacy:
                    antelog.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
                else:
                    antstock.elog_notify("%s %s *notify*: Dbpath '%s' does not exist." % (self.errstr, self.base, dbname))
            start_dy = yearday((antstock.epoch(start_dy) + 86400))
        return days

    def namelist(self):
        """Get the list of resolved paths
        """
        if not os.path.exists(self.path) and not os.path.isfile(self.path):
            if not legacy:
                antelog.elog_die("%s %s *die*: Dbpath '%s' does not exist." % (self.errstr, self.base, self.path))
            else:
                antstock.elog_die("%s %s *die*: Dbpath '%s' does not exist." % (self.errstr, self.base, self.path))

        db = antdb.dbopen(self.path, 'r')
        db.lookup(table='clusters')
        db.lookup(record='dbNULL')

        try:
            null_time, null_endtime = db.getv('time', 'endtime')
        except Exception, e:
            if not legacy:
                antelog.elog_die("%s %s *die*: %s" % (self.errstr, self.base, e))
            else:
                antstock.elog_die("%s %s *die*: %s" % (self.errstr, self.base, e))

        db.lookup(record='dbALL')

        try:
            db.subset("clustername =~ /%s/" % self.clustername)
        except Exception, e:
            if not legacy:
                antelog.elog_die("%s %s *notify*: %s" % (self.errstr, self.base, e))
            else:
                antstock.elog_die("%s %s *notify*: %s" % (self.errstr, self.base, e))

        if db.query('dbRECORD_COUNT') == 0:
            if not legacy:
                antelog.elog_die("%s %s *die*: Zero records for the clustername '%s'." % (self.errstr, self.base, self.clustername))
            else:
                antstock.elog_die("%s %s *die*: Zero records for the clustername '%s'." % (self.errstr, self.base, self.clustername))

        db.sort('time')

        for i in range(db.query('dbRECORD_COUNT')):
            db[3] = i
            dbname_template = db.extfile()
            volumes, time, endtime = db.getv('volumes', 'time', 'endtime')

            if endtime == null_endtime:
                endtime = antstock.now()

            start_day = int(antstock.yearday(time))
            start_month = int(antstock.epoch2str(time, '%L'))
            start_year = int(antstock.epoch2str(time, '%Y'))
            end_day = int(antstock.yearday(endtime))
            end_month = int(antstock.epoch2str(endtime, '%L'))
            end_year = int(antstock.epoch2str(endtime, '%Y'))

            if volumes == 'single':
                if self.include_times:
                    namelist[dbname_template] = (time, endtime)
                else:
                    namelist = dbname_template ;

            elif volumes == 'year':
                namelist = self.year_resolver(start_year, end_year, dbname_template)

            elif volumes == 'month':
                namelist = self.month_resolver(start_month, start_year, end_month, end_year, dbname_template)

            elif volumes == 'day':
                namelist = self.day_resolver(start_month, start_year, end_month, end_year, dbname_template)

            else:
                if not legacy:
                    antelog.elog_die("%s %s *notify*: Volumes type '%s' in cluster database not understood" % (self.errstr, self.base, volumes))
                else:
                    antstock.elog_die("%s %s *notify*: Volumes type '%s' in cluster database not understood" % (self.errstr, self.base, volumes))

        return namelist

if __name__ == '__main__':
    import unittest

    class TestDbCentral(unittest.TestCase):

        def setUp(self):
            """If you want to test on your
            local system, you need to modify
            self.testdbname and self.testclustername
            to a valid local dbcentral database and
            clustername.
            """
            self.testdbname = '/anf/shared/dbcentral/dbcentral'
            self.testclustername = 'usarray'

        def tearDown(self):
            pass

        def test_method_open(self):
            db = antdb.dbopen(self.testdbname, 'r')
            self.assertTrue(db[0] >= 0)
            self.assertEqual(db[1], -501)
            self.assertEqual(db[2], -501)
            self.assertEqual(db[3], -501)
            db.close()

        def test_method_lookup_clusters(self):
            db = antdb.dbopen(self.testdbname, 'r')
            db.lookup(table='clusters')
            self.assertTrue(db[0] >= 0)
            self.assertEqual(db[1], 0)
            self.assertEqual(db[2], -501)
            self.assertEqual(db[3], -501)
            db.close()

        def test_method_lookup_clustername(self):
            db = antdb.dbopen(self.testdbname, 'r')
            db.lookup(table='clusters')
            db.subset('clustername =~ /%s/' % self.testclustername)
            self.assertTrue(db.query('dbRECORD_COUNT') >= 0)
            self.assertTrue(db.table >= 0)
            self.assertEqual(db[2], -501)
            self.assertEqual(db[3], -501)
            db.close()

        def test_method_paths(self):
            db = antdb.dbopen(self.testdbname, 'r')
            db.lookup(table='clusters')
            db.subset('clustername =~ /%s/' % self.testclustername)
            db[3]=0
            dbcentral = DbCentral(self.testdbname, self.testclustername, False)
            paths = dbcentral.namelist()
            self.assertTrue(len(paths) > 0)
            db.close()

    suite = unittest.TestLoader().loadTestsFromTestCase(TestDbCentral)
    unittest.TextTestRunner(verbosity=2).run(suite)
