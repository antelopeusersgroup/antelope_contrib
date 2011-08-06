# {{{ HEADERS
#
# @author:   Rob Newman <rlnewman@ucsd.edu>, (+1) 858 822 1333
# @expanded: Juan Reyes <reyes@ucsd.edu>, (+1) 858 822 2989
# @created:  2010-08-11
# @updated:  2010-11-21  Juan Reyes
# @updated:  2011-01-13  Juan Reyes
#
# @usage:
#   Create:
#      element = dbcentral(path,nickname)      # Default mode
#      element = dbcentral(path,nickname,True) # Enable Debug mode. Verbose output
#   Access:
#      print element                           # Nice print of values
#      element.type                            # return string value for mode [dbcentral,masquerade]
#      element.path                            # return string value for path
#      element.nickname                        # return string value for nickname
#      element.list()                          # return list of databases
#      element(epoch)                          # return database matching the epoch time
#      element.purge(db)                       # remove database from class.
#
#   Note:
#       dbcentral will load the database and test for the existence of the 'clusters' table first. If missing 
#       the class will assume that the intention of the user is to load a regular database and use the class
#       functionality to store the pointer. This will trigger the self.type value to be "masquerade". Regular
#       dbcentral tables will load normally and set self.type to "dbcentral". 

# }}} HEADERS

from __main__ import *

class dbcentralException(Exception):
    pass

class dbcentral:

    def __init__(self, path, nickname=None, debug=False):
        # {{{ __init__
        self.type = False
        self.path = os.path.abspath(path)
        self.nickname = nickname
        self.debug = debug

        # Create dictionary to hold all the values
        self.dbs = {}

        # Load the dbs
        self._get_list()


        # }}} __init__

    def __str__(self):
#{{{
        """
        end-user/application display of content using print() or log.msg()
        """
        return ''.join(["\n*dbcentral*:\t%s: %s" % (value,self.dbs[value]) for value in sorted(self.dbs.keys())])

#}}}

    def info(self):
        #{{{

        print "*dbcentral*:\tdbcentral.nickname() => %s" % self.nickname
        print "*dbcentral*:\tdbcentral.type() => %s" % self.type
        print "*dbcentral*:\tdbcentral.path() => %s" % self.path
        print "*dbcentral*:\tdbcentral.list() => %s" % self.list()
        for element in sorted(self.dbs):
            print "*dbcentral*:\t%s => %s" % (element,self.dbs[element]['times'])

        #}}}

    def __call__(self, time=stock.now()):
#{{{
        """
        method to intercepts data requests.
            time default is "now"
        """

        try:
            time = float(time)
        except Exception,e:
            print "\n*dbcentral*: dbcentral() => error in time=>[%s] %s" % (time,time.__class__)
        else:
            for element in sorted(self.dbs):
                if self.dbs[element]['times'][0] < time and time < self.dbs[element]['times'][1]:
                    return element

        print "\n*dbcentral*: dbcentral() => No db match for time=>[%s]" % time
        self.info()
        return False
#}}}

    def __dell__(self):
        #{{{ cleanup vars
        """
        method to intercepts data requests.
        """

        self.dbs = {}

        #}}}

    def _problem(self, log):
        #{{{ Nice print of errors
        """
        method to print problems and raise exceptions
        """
        raise dbcentralException('*dbcentral*: ERROR=> %s' % log)

        #}}}

    def _get_list(self):
        # {{{ private function to load data

        try:

            db = datascope.dbopen(self.path, "r")

        except Exception,e:

            self._problem("Cannot open database. (%s)" % e)
            return False


        try:

            db.lookup('','clusters','','')

        except Exception,e:

            self._problem("Cannot look up 'clusters' table in database. (%s)" % e)
            return False


        try:

            db.query(datascope.dbTABLE_PRESENT)

        except:

            self.type = 'masquerade'
            self.nickname = None
            self.dbs[self.path] = {'times': [-10000000000.0,10000000000.0]}
            if self.debug: print "*dbcentral*: Not a dbcentral database. Openning single database."
            return 

        else:

            self.type = 'dbcentral'
            
            if self.nickname is None:
                self._problem("Need nickname for dbcentral clustername regex.")
                return Flase


        try:

            db.lookup('','clusters','','dbNULL')
            null_time,null_endtime = db.getv('time','endtime')

        except Exception,e:

            self._problem("Cannot look up null values in clusters table. (%s)" % e)
            return False


        expr = "clustername =='%s'" % self.nickname


        try:

            db.subset(expr)

        except Exception,e:

            self._problem("Cannot subset on clustername. %s" % e)
            return False

        try:

            db.sort(['time'])
            nclusters = db.nrecs()


        except Exception,e:

            self._problem("Cannot sort on 'time' . %s" % e)
            return False

        if nclusters < 1:

            self._problem("No matches for nickname.")

        if self.debug: print "*dbcentral*: Records=%s" % nclusters

        for i in range(nclusters):

            db[3]=i

            try:

                dbname_template = db.extfile()

            except Exception, e:

                self._problem("Cannot run db.extfile(). %s" % e)
                return False

            if self.debug: print "*dbcentral*: dbname_template=%s" % dbname_template

            try:

                self.volumes,self.net,time,endtime = db.getv("volumes","net","time","endtime")

            except Exception,e:

                self._problem("Problems with db.getv('volumes','net','time','endtime'). (%s)\n" % e)
                return False

            if self.debug:
                print "*dbcentral*: volumes=%s" % self.volumes
                print "*dbcentral*: net=%s" % self.net
                print "*dbcentral*: time=%s" % time
                print "*dbcentral*: endtime=%s" % endtime

            if endtime == null_endtime:
                #{{{ endtime

                # This will be problematic with realtime systems
                endtime = stock.now()

                if self.debug: print "*dbcentral*: endtime=%s" % endtime

                # }}} endtime

            if self.volumes == 'single':
                # {{{ single

                if self.debug: print "*dbcentral*: Single... dbname=%s" % dbname_template

                if os.path.isfile(dbname_template):

                    self.dbs[dbname_template] = {'times':[time,endtime]}

                else:

                    if glob.glob("%s.*" % dbname_template):

                        self.dbs[dbname_template] = {'times':[time,endtime]}

                        print "*dbcentral*: WARNING: No descriptor file for (%s)." % dbname

                    else:

                        print "*dbcentral*: ERROR: Cannont find dbname=%s" % dbname


                self.dbs[dbname_template] = {'times':[time,endtime]}

                # }}} single

            elif self.volumes == 'year':
                # {{{ year

                start_year = int(stock.epoch2str(time,"%Y"))
                end_year   = int(stock.epoch2str(endtime,"%Y"))

                for y in range(start_year,end_year+1):

                    voltime    = stock.str2epoch("1/1/%s 00:00:00" % y)
                    volendtime = stock.str2epoch("12/31/%s 23:59:59" % y)
                    dbname     = stock.epoch2str(voltime,dbname_template)

                    if self.debug: print "*dbcentral*: Test for year=%s =>> %s" % (y,dbname)

                    if os.path.isfile(dbname):

                        self.dbs[dbname] = {'times': [voltime,volendtime]}

                    else:

                        if glob.glob("$s.*" % dbname):

                            self.dbs[dbname] = {'times': [voltime,volendtime]}

                            print "*dbcentral*: WARNING: No descriptor file for (%s)." % dbname

                        else:

                            print "*dbcentral*: ERROR: Cannont find dbname=%s" % dbname


                # }}} year

            elif self.volumes == 'month':
                # {{{ month

                start_month = int(stock.epoch2str(time,"%L"))
                start_year  = int(stock.epoch2str(time,"%Y"))
                end_month   = int(stock.epoch2str(endtime,"%L"))
                end_year    = int(stock.epoch2str(endtime,"%Y"))

                vol_month   = start_month
                vol_year    = start_year

                # Iterator
                i = 0

                while vol_year < end_year or ( vol_year == end_year and vol_month <= end_month ):

                    voltime           = stock.str2epoch("%d/1/%d" % (vol_month,vol_year) )

                    if vol_month < 12:
                        temp_vol_endmonth = vol_month + 1
                        temp_vol_endyear  = vol_year
                    else:
                        temp_vol_endmonth = 1
                        temp_vol_endyear  = vol_year + 1

                    volendtime = stock.str2epoch("%d/1/%d" % (temp_vol_endmonth,temp_vol_endyear) ) - 1
                    dbname     = stock.epoch2str(int(voltime), dbname_template)

                    if self.debug:
                        print "*dbcentral*: Test for year=%s month=%s =>> %s" % (temp_vol_endyear,temp_vol_endmonth,dbname)

                    if os.path.isfile(dbname):

                        self.dbs[dbname] = {'times': [voltime,volendtime] }

                    else:

                        if glob.glob("%s.*" % dbname):

                            self.dbs[dbname] = {'times': [voltime,volendtime]}

                            print "*dbcentral*: WARNING: No descriptor file for (%s)." % dbname

                        else:

                            print "*dbcentral*: ERROR: Cannont find dbname=%s" % dbname


                    if vol_month < 12:
                        vol_month = vol_month + 1
                    else:
                        vol_year = vol_year + 1
                        vol_month = 1

                    i = i + 1

                # }}} month

            elif self.volumes == 'day':
                # {{{ day

                start_day = int(stock.yearday(time))
                end_day   = int(stock.yearday(endtime))

                vol_day   = start_day

                # Iterator
                i = 0

                while vol_day <= end_day:

                    voltime    = stock.epoch(vol_day)
                    volendtime = voltime + 86399 # one second less than a full day
                    dbname     = stock.epoch2str(voltime, dbname_template)
                    
                    if self.debug: print "*dbcentral*: Test for time=%s =>> %s" % (voltime,dbname)

                    if os.path.isfile(dbname):

                        self.dbs[dbname] = {'times': [voltime,volendtime]}

                    else:

                        if glob.glob("%s.*" % dbname):

                            self.dbs[dbname] = {'times': [voltime,volendtime]}

                            print "*dbcentral*: WARNING: No descriptor file for (%s)." % dbname

                        else:

                            print "*dbcentral*: ERROR: Cannont find dbname=%s" % dbname

                    vol_day = stock.yearday((stock.epoch(vol_day)+86400))

                    i += 1

                # }}} day

            else:

                return "*dbcentral*: ERROR: Volumes type '%s' in cluster database not understood" % volumes

        if self.debug:
            print ''
            print "*dbcentral*: DBS=%s" % self.dbs.keys()
            print ''


        # }}} 

    def list(self):
        #{{{ return values to the user

        try:
            return self.dbs.keys()
        except:
            raise dbcentralException('*dbcentral*: ERROR=> Cannot check content of list!')

        #}}}

    def purge(self,tbl=None):
        #{{{ remove a database from the list

        """
        Method to clean dbcentral object by removing a database
        """
        if not tbl:
            raise dbcentralException('*dbcentral*: dbcentral.purge() => No db')

        print '*dbcentral*: dbcentral.purge() => %s' % tbl

        try: 
            del self.dbs[tbl] 
        except :
            pass

        self.info()

        #}}}

if __name__ == '__main__':
#{{{ Tests for class dbcentral
    """
    This will run if the file is called directly.
    """

    time =  1262404000.00000

    dbcntl = dbcentral('/opt/antelope/data/db/demo/demo')

    print 'dbcntl = dbcentral("%s","%s")' % (dbcntl.path,dbcntl.nickname)
    print 'dbcntl.type => %s' % dbcntl.type
    print 'dbcntl.nickname => %s' % dbcntl.nickname
    print ''
    print '%s' % dbcntl
    print ''
    print 'dbcntl.list() => %s' % dbcntl.list()
    print ''
    print 'dbcntl(%s) => %s' % (time,dbcntl(time))
    print ''
    try:
        dbcntl.purge('test')
    except Exception, e:
        print 'dbcntl.purge(%s) => %s' % ('test',e)

    print ''


#}}}
