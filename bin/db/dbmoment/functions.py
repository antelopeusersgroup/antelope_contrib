#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


from __main__ import *      # Get all the libraries from parent

def open_db(db):
    '''
    Open a database (or database pointer)

    Remember to free the return pointer later!!!
    '''

    elog.debug( 'Open db' )

    # Verify if we have a string or a pointer DB object
    if isinstance( db, datascope.Dbptr ):
        elog.debug( 'got db pointer( %s )' % db )
        dbview = db
        dbview.table = datascope.dbALL
        dbview.record = datascope.dbALL
        dbview.field = datascope.dbALL
    else:
        elog.debug( 'dbopen( %s )' % db )
        try:
            dbview = datascope.dbopen( db, "r+" )
        except Exception,e:
            elog.error('Problems opening database: %s %s %s' % \
                    (db,Exception, e) )

    try:
        # Verify this database
        elog.debug( 'dbDBPATH => %s' % \
                dbview.query(datascope.dbDBPATH))
        elog.debug( 'dbDATABASE_NAME => %s' % \
                dbview.query(datascope.dbDATABASE_NAME))
        elog.debug( 'dbDATABASE_IS_WRITABLE => %s' % \
                dbview.query(datascope.dbDATABASE_IS_WRITABLE))
    except Exception,e:
        elog.error('Problem with database [%s]: %s [%s]' % ( db, Exception,e) )

    return dbview


def open_table(db, tablename=None):
    '''
    Open a database (or database pointer) and verify a table

    Remember to free the return pointer later!!!
    '''

    elog.debug( 'Open db/table [%s]' % tablename )

    dbview = open_db(db)

    if not tablename:
        return dbview

    elog.debug( 'Verify table [%s]' % tablename )

    tableview = dbview.lookup(table=tablename)

    # Verify if we have the table
    elog.debug( 'dbTABLE_PRESENT => %s' % \
            tableview.query(datascope.dbTABLE_PRESENT))


    # Check if we have something to continue
    if not tableview.query(datascope.dbTABLE_PRESENT):
        elog.warning( 'Missing table [%s] in db view.' % tablename )

    if not tableview.record_count:
        elog.warning( 'EMPTY table %s' % tablename )

    # Return valid view if table is present
    return tableview


def nice_print_results( results ,split=False, prelim='' ):

    text = '*** NULL RESULTS *** '
    text2 = '*** NULL RESULTS ***'

    if  results and 'Quality' in results:
        # First panel
        text =  "%s %s\n" % ( results['estatus'],prelim )
        text += "\n"
        #text += "Location: ( {:0.2f}, {:0.2f} ) ".format( event.lat, event.lon)
        #text += " {0} km\n".format( event.depth )
        #text += "\n"
        #text += "Model:     {0}\n".format( event.model )
        text += "VarRed:    {:0.1f} %\n".format( float(results['VarRed']) )
        text += "Mo:        {0}\n".format( results['Mo'] )
        text += "S:%s  R:%s  D:%s\n" % \
                ( results['Strike'], results['Rake'], results['Dip'] )
        # Second panel
        text2  = "Pdc:       {0:0.1f}%\n".format( float( (results['Pdc'] * 100) ) )
        text2 += "Pclvd:     {0:0.1f}%\n".format( float( (results['Pclvd'] * 100) ) )
        text2 += "VAR:       {0:0.1e} \n".format( float( results['Variance'] ) )
        text2 += "Var/Pdc:   {0:0.1e} \n".format( float( results['Var/Pdc'] ) )
        text2 += "Mxx:%0.3f  Myy:%0.3f\n" % \
                (float(results['Mxx']), float(results['Myy']))
        text2 += "Mxy:%0.3f  Mxz:%0.3f\n" % \
                (float(results['Mxy']), float(results['Mxz']))
        text2 += "Myz:%0.3f  Mzz:%0.3f\n" % \
                (float(results['Myz']), float(results['Mzz']))

    else:
        elog.warning( 'Cannot find useful information in results.' )
        elog.warning( results )

    if split:
        return text, text2
    else:
        return text + text2

def roll_logfile( log_filename, log_max_count ):
    elog.info( 'Using log: {0}'.format( log_filename ) )

    # Clean log folder
    logfiles = glob.glob('%s%s' % ( log_filename, "*" ) )
    elog.info( 'Previous files: {0}'.format( logfiles ) )
    for f in sorted(logfiles, reverse=True):
        elog.info( 'Verify log: {0}'.format( f ) )
        m = re.match( "^(%s)(\.)?(\d)*$" % log_filename, f )
        try:
            name = m.group(1)
        except:
            elog.info( 'No filenames with this format:{0}'.format( log_filename ) )
            continue

        try:
            version = int( m.group(3) )
        except:
            version = 0

        elog.info( 'File:{0} Version:{1}'.format( name, version ) )

        if version >= int(log_max_count):
            elog.info( 'Remove old log: {0}'.format( f ) )
            os.remove(f)
        else:
            newname = "%s.%s.tmp" % (m.group(1), version + 1 )
            elog.info( 'Move %s to %s ' % ( f, newname) )
            os.rename( f, newname )
    # Set final names
    logfiles = glob.glob('%s*.tmp' % ( log_filename ) )
    for f in sorted(logfiles, reverse=True):
        elog.info( 'Verify log: {0}'.format( f ) )
        m = re.match( "^(%s\.\d*)(\.tmp)$" % log_filename, f )
        try:
            elog.info( 'Move %s to %s ' % ( f, m.group(1)) )
            os.rename( f, m.group(1) )
        except Exception,e:
            elog.info( 'ERROR: rename tmp to final({0}): {1}'.format( f, e ) )



def valid_number( test ):

    if test != test: return False
    if float(test) == float('Inf'): return False
    if float(test) == float('-Inf'): return False

    return True

def add_padding(tr):

    elog.debug('Add padding to trace object ' )

    #trcopy = tr.trcopy()
    #trcopy.trfilter('BW 20 4 0 0')

    # Need to double our traces
    for rec in tr.iter_record():


        # PADD WITH REVERSED DATA
        #data = array( rec.trdata() )
        #reversed_data = data[::-1]
        #rec.trputdata( concatenate([reversed_data , data]) )


        # PADD WITH FAKE DATA
        #data = array( rec.trdata() )
        #padd = ones( len(data) )
        #padd = zeros( len(data) )
        #padd = zeros( 200 )
        #padd *= data[0]
        padd = zeros( 5000 )
        rec.trputdata( concatenate([padd , rec.trdata()]) )


        # PADD WITH FILTERED DATA
        #trcopy.record = rec.record
        #rec.trputdata( concatenate([trcopy.trdata() , rec.trdata()]) )


    #trcopy.free()

    return tr


#def remove_padding(tr,samples):
#
#    elog.debug('Remove padding on trace object ' )
#
#    # remove the padding
#    for rec in tr.iter_record():
#
#        #data = array( rec.trdata() )
#        #rec.trputdata( data[-int(len(data)/2):] )
#        rec.trputdata( rec.trdata()[-samples:] )
#
#    return tr


def myround(x, base=5):
    #from __main__ import elog
    elog.debug('Round %s to base %s' % (x, base) )
    return int(int(base) * round(float(x)/int(base)))

def run(cmd,directory='.',max_try=5):
    attempt = 1

    while( attempt < max_try ):
        attempt += 1
        elog.debug("run()  -  Running: %s" % cmd)
        p = subprocess.Popen([cmd], stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            cwd=directory, shell=True)
        stdout, stderr = p.communicate()

        if stdout:
            for line in iter(stdout.split('\n')):
                elog.debug('stdout:\t%s'  % line)
        if stderr:
            for line in iter(stderr.split('\n')):
                elog.debug('stderr:\t%s'  % line)

        if p.returncode != 0 :
            elog.notify('Exitcode (%s) on [%s]' % (p.returncode,cmd))
            continue

        if stdout:
            return iter(stdout.split('\n'))
        if stderr:
            return iter(stderr.split('\n'))

    elog.error('Cannot successfully run [%s]' % cmd)

def fix_amplitud(tr, value):
    #from __main__ import elog
    elog.debug('Fix amplitud by %s' % (value) )

    # Need to double our traces
    for rec in tr.iter_record():
        data = array( rec.trdata() )

        rec.trputdata( data * value )

    return tr


def add_trace_to_plot( data, style='r', label='signal', count=1, item=1, delay=0, jump=1):

    start =  int(delay * jump)
    plot_axis = range( start, int( len(data) * jump ) + start, int(jump) )

    pyplot.subplot(count,1,item)
    pyplot.plot( plot_axis, data, style, label=label )
    pyplot.legend(loc=1)


def plot_tr_object(tr, name='signal', fig=False, style='r', delay=0, jump=1, display=False, debug=False):

    if not fig:
        fig = pyplot.figure()

    this = 1
    for rec in tr.iter_record():
        #data = rec.trdata()
        data = []
        tempdata = rec.trdata()
        padd = 0
        for i in range (0, len(tempdata)-1):
            if tempdata[i] > 1.e20:
                padd += 1
                continue
            data.append( tempdata[i] )


        if debug:
            # Test rotations with this synth data. Just a spike
            original_samples = len(data)
            ones_data = list( [1] * len(data) )
            ones_data[ (rec+1) * 1000 ] = 30000
            tr.trputdata( ones_data )

        add_trace_to_plot( data, style=style, label='%s-%s' % (name, rec.getv('chan')[0]),
                count=tr.record_count, item=this, delay=delay+padd, jump=jump)

        # start and end marks
        #pyplot.axvline( (start - got_time ) * samprate, color='y')
        #pyplot.axvline( ( (start - got_time ) + tw )* samprate , color='y')

        this += 1

    return fig


def apply_response(tr, filename, samprate):
    #from __main__ import elog
    elog.debug('Aplly response %s' % (filename) )

    if not filename :
        elog.log( 'No response file provided' )
        return tr

    if not os.path.isfile( filename ):
        elog.warning('Problems loading response file: [%s]' % (filename))
        return tr

    try:
        respaddr = response._get_response(filename)
        elog.debug('respaddr')
        elog.debug(respaddr)
        if not respaddr: raise RuntimeError
    except Exception,e:
        elog.error('Problems loading response file: %s => %s' % (filename, e))

    for rec in tr.iter_record():
        data = array( rec.trdata() )
        elog.debug( 'Total samples: %s ' % len(data) )

        npts = len(data)

        elog.debug('compute the one-dimensional discrete Fourier Transform')
        fft_values = fft(data)[0:int(npts/2)+1]

        # Return the Discrete Fourier Transform sample frequencies
        elog.debug('compute the Fourier Transform sample frequencies')
        freq = fftfreq(len(data), d=1.0/samprate)[range(0,npts/2+1)]

        # the last element is negative, because of the symmetry, but should
        # be positive
        fft_values[-1] *= -1


        convolved = []
        for f in freq:
            w = f * 2 * pi
            convolved.append( response._eval_response(respaddr, w ) )

        data = irfft( convolved * fft_values )

        elog.debug( 'Total samples: %s ' % len(data) )
        rec.trputdata( data )

    response._free_response ( respaddr )

def fix_exec(content):

    global executables
    if len(executables):
        for src, target in executables.iteritems():
            content = re.sub(r"^%s " % src, "%s " % target, content)

    return content


def cleanup_db(db,match):
    #from __main__ import elog
    elog.debug( 'dbTABLE_PRESENT => %s' % db.query(datascope.dbTABLE_PRESENT) )
    elog.debug( 'dbTABLE_IS_WRITABLE => %s' % db.query(datascope.dbTABLE_IS_WRITABLE) )

    if db.query(datascope.dbTABLE_PRESENT):
        while db.record_count:

            try:
                record = db.find(match, first=-1)
            except Exception,e:
                record = -1

            if record < 0: break
            elog.debug('Found previous record %s for %s' % (record, match))
            db.record = record
            db.mark()
            elog.debug('%s marked' % record)



class Records():
    """
    Class for tracking info from a single sta
    """

    def __init__(self,sps=0,segtype=None,response=None):

        #from __main__ import elog
        self.chans = {}
        self.samplerate = sps
        self.file = None
        self.segtype = segtype
        self.response = response

    def __iter__(self):
        self.chan_list = sorted( self.chans.keys() )
        elog.debug( 'Chans in Record: %s' % self.chan_list )
        return self

    def next(self):
        try:
            chan = self.chan_list.pop(0)
            return chan, self.chans[chan]

        except:
            raise StopIteration

    def list(self):
        self.chan_list = sorted( self.chans.keys() )
        #elog.debug( 'Chans in Record: %s' % self.chan_list )
        return self.chan_list

    def flip(self):
        for chan in self.chans.keys():
            elog.debug('Flip channel %s' % chan )
            self.chans[chan] = self.chans[chan] * -1

    def get(self,channame):
        return self.get_data(channame)

    def set(self,channame,data=[]):
        self.set_data(channame,data)

    def trace(self,channame,data=[]):
        self.set_data(channame,data)

    def set_samplerate(self,sps):
        self.samplerate = sps


    def set_data(self,channame,data=[]):
        data = array( data )
        self.chans[channame] = data

    def samplecount(self,channame=False):
        if not channame:
            try:
                channame = self.list()[0]
            except:
                return 0
        return len( self.chans[channame] )

    def getmin(self,channame=False):
        if not channame:
            channame = self.list()

        vmin = False
        for c in channame:
            v = min(self.chans[c])
            try:
                if v < vmin:
                    vmin = v
            except:
                vmin = v

        return vmin

    def getmax(self,channame=False):

        if not channame:
            channame = self.list()

        vmax = False

        for c in channame:
            v = max(self.chans[c])
            try:
                if v > vmax:
                    vmax = v
            except:
                vmax = v

        return vmax

    def apply_calib(self, calib, chan):
        elog.debug('Aplly calibration of [%s] to channel %s' % (calib,chan) )
        elog.debug( self.chans[chan] )

        self.chans[chan] *= calib
        elog.debug( self.chans[chan] )


    def get_data(self,chan):
        #elog.debug('Get %s from trace' % chan)
        if chan and chan in self.chans:
            return self.chans[chan]
        return self.chans

    def window(self,start=0,end=-1):
        #elog.debug('Get %s from trace' % chan)
        for c in self.list():
            self.chans[c] = self.chans[c][start:end]

    def new_script(self,filename,content):

        try:
            os.remove(filename)
        except:
            pass

        outfile = open(filename, 'w')

        global executables
        if len(executables):
            for line in content.split("\n"):
                for src, target in executables.iteritems():
                    line = re.sub(r"^%s " % src, "%s " % target, line)
                outfile.write(line+"\n")
        else:
            outfile.write(content)

        outfile.close()

        st = os.stat(filename)
        os.chmod(filename, st.st_mode | stat.S_IEXEC)

        return filename





class Station(Records):
    """
    Class for keeping all information related to
    a particular station.
    """

    def __init__(self,sta,tmp_folder):
        #from __main__ import elog
        self.sta = sta
        self.tmp_folder = tmp_folder
        self.real = Records()
        self.real_file = False
        self.real_file_name = False
        self.real_samples = 0
        self.real_samplerate = 0
        self.synth = Records()
        self.synth_zrt = Records()
        self.synth_file = False
        self.synth_file_name = False
        self.synth_samples = 0
        self.synth_samplerate = 0
        self.depth = 0
        self.lat  = False
        self.lon  = False
        self.delta  = False
        self.realdistance  = False
        self.distance  = False
        self.azimuth  = False
        self.zcor  = 0
        self.sdelay  = 0
        self.vr = None

    def clean(self):
        elog.debug( 'clean object for : %s' % self.sta )
        self._remove_file( self.real_file )
        self._remove_file( self.synth_file )

    def _remove_file(self, filename ):

        if os.path.isfile( filename ):
            try:
                os.remove( filename )
            except:
                elog.warning( 'cannot remove file: %s' % filename )


    def real_trace(self,channame=False):
        return self.real.get_data(channame)

    def real_data(self,data=Records()):
        return self.save('real',data)

    def synth_trace(self,channame=False):
        return self.syncth.data(channame)

    def synth_data(self,data=Records()):
        return self.save('synth',data)

    def real_channels(self):
        return self.real.keys()

    def synth_channels(self):
        return self.synth.keys()

    def flip(self,trace='rea'):
        if trace == 'real':
            self.real.flip()
        else:
            self.synth.flip()
            self.synth_zrt.flip()


    def to_file(self,trace='rea'):
        if trace == 'real':
            self.real_file = makeHelm(self.real,
                    append='%s-real' % self.sta, folder=self.tmp_folder)
            self.real_file_name = os.path.basename(self.real_file)
            return self.real_file
        else:
            self.synth_file = makeHelm(self.synth,
                    append='%s-synth' % self.sta, folder=self.tmp_folder)
            self.synth.file = self.synth_file
            self.synth_file_name = os.path.basename(self.synth_file)
            return self.synth_file

    def save(self,trace='real',data=Records()):
        if trace == 'real':
            self.real = data
            self.real_file = False
            self.real_samples = data.samplecount()
            self.real_samplerate = data.samplerate
        else:
            self.synth = data
            self.synth_file = False
            self.synth_samples = data.samplecount()
            self.synth_samplerate = data.samplerate

    def max_min_all(self):
        rmax = self.real.getmax()
        rmin = self.real.getmin()
        smax = self.synth.getmax()
        smin = self.synth.getmin()


        rsamps = self.real_samples
        ssamps = self.synth_samples

        mx = max([rmax, smax])
        mn = min([rmin, smin])
        tsamps = max([rsamps, ssamps])
        return (0, tsamps, mn, mx)

    def max_min(self,trace='real'):
        if trace == 'real':
            vmax = self.real.getmax()
            vmin = self.real.getmin()
            samps = self.real_samples
        else:
            elog.debug( 'synt: %s' % self.synth_samples )
            vmax = self.synth.getmax()
            vmin = self.synth.getmin()
            samps = self.synth_samples

        return (0,samps,vmin,vmax)

    def convert_synth(self, results):

        elog.debug(' apply results to greens for plotting ' )

        # Find time shift
        try:
            self.zcor = float(results['zcor'][self.sta])
        except:
            self.zcor = 0

        strike = results['Strike'][0]
        rake = results['Rake'][0]
        dip = results['Dip'][0]
        moment = float( results['Mo'] )
        isomoment = float( results['isomoment'] )



        strike = float(self.azimuth) - strike
        strike *= pi/180.0
        rake *= pi/180.0
        dip *= pi/180.0


        moment /= 1.0e+20
        isomoment /= 1.0e+20


        A0=sin(2.0*strike)*cos(rake)*sin(dip) + 0.5*cos(2.0*strike)*sin(rake)*sin(2.0*dip)
        A1=cos(strike)*cos(rake)*cos(dip) - sin(strike)*sin(rake)*cos(2.0*dip)
        A2=0.5*sin(rake)*sin(2.0*dip)
        A3=cos(2.0*strike)*cos(rake)*sin(dip) - 0.5*sin(2.0*strike)*sin(rake)*sin(2.0*dip)
        A4=sin(strike)*cos(rake)*cos(dip) + cos(strike)*sin(rake)*cos(2.0*dip)
        A4 *= -1.0

        elog.debug( "A1=%f A2=%f A3=%f A4=%f A5=%f" % (A0,A1,A2,A3,A4) )


        # synth_channels = ['TSS','TDS','XSS','XDS','XDD','ZSS','ZDS','ZDD','REX','ZEX']
        #                     0     1     2     3     4     5     6     7     8     9

        tss = self.synth.get( 'TSS' )
        tds = self.synth.get( 'TDS' )
        xss = self.synth.get( 'XSS' )
        xds = self.synth.get( 'XDS' )
        xdd = self.synth.get( 'XDD' )
        zss = self.synth.get( 'ZSS' )
        zds = self.synth.get( 'ZDS' )
        zdd = self.synth.get( 'ZDD' )
        rex = self.synth.get( 'REX' )
        zex = self.synth.get( 'ZEX' )


        tan = moment * (A3 * tss + A4 * tds)
        self.synth_zrt.set('T', tan )

        rad = moment * (A0 * xss + A1 * xds + A2 * xdd) + isomoment * rex
        self.synth_zrt.set('R', rad )

        ver = -1.0 * moment * (A0 * zss + A1 * zds + A2 * zdd) + isomoment * zex
        self.synth_zrt.set('Z', ver )



    def plot(self,trace='real'):
        # open a plot of the data
        elog.debug('Plot traces for %s' % self.sta )

        if trace is 'real':
            records = self.real
        else:
            records = self.synth

        if not records:
            elog.error('Empty Records for %s data on %s' % (trace,self.sta) )

        axs = self.max_min(trace)

        total = 0
        pyplot.figure()
        channels = records.list()
        for chan, data in records:
            elog.debug( 'add %s_%s to plot' % (self.sta,chan) )
            pyplot.subplot(len(channels),1,total)
            pyplot.plot(data)
            pyplot.legend(["%s_%s" % (self.sta,chan)])
            pyplot.axis( axs )
            total += 1

        pyplot.suptitle("%s data: Trace %s" % (trace,self.sta) )
        pyplot.show()


def find_executables( execs ):
    '''
    Let's find some execs on the system and
    keep the full path on a variable.
    This is to avoid problems with some of
    the scripts that we need to run with
    Dreger's original code.
    '''

    #from __main__ import elog

    if not len(execs): return

    global executables

    for ex in execs:
        elog.debug("Find executable for (%s)" % ex)

        newex = spawn.find_executable(ex)

        if not newex:
            elog.error("Cannot locate executable for [%s] in $PATH = \n%s" % \
                    (ex,os.environ["PATH"].split(os.pathsep)) )

        executables[ex] = os.path.abspath( newex )

        elog.debug("(%s) => [%s]" % (ex, newex) )

def plot_results( results, bb_colors={},
        folder='./', acknowledgement='dbmoment',
        beachball=False, prelim=''):

    event = results['event']
    stations = results['stations']


    # Done with the inversion. Now set values for plotting results
    for sta in stations.keys():
        elog.debug('convert_synth( %s )' % sta)
        #stations[sta].convert_synth_original( results )
        stations[sta].convert_synth( results )

    total = 1

    try:
        event_color = "#%s" % bb_colors[ str(results['Quality']) ]
    except:
        event_color = 'k'

    elog.debug('Beachball/Event color: %s => [%s]' % (results['Quality'],event_color) )



    total_stations = len(stations.keys()) + 2
    gcf = pyplot.gcf()
    fig = pyplot.figure(figsize=( 20, 2 * total_stations ))

    max_all = []
    min_all = []
    points_all = []

    #for sta in sorted(stations.keys()):
    for sta in sorted(stations.keys(), key=lambda x: stations[x].realdistance):
        axs = stations[sta].max_min_all()
        points_all.append( axs[1] )
        min_all.append( axs[2] )
        max_all.append( axs[3] )

    max_plot = max(max_all)
    min_plot =  min(min_all)
    points_plot = max(points_all)



    text, text2  = nice_print_results( results ,split=True, prelim=prelim)

    text += "\n( {:0.1f}, {:0.1f} ) ".format( event.lat, event.lon)
    text += "{0} km depth\n".format( event.depth )
    text += "ID:{0}    ".format( event.orid )
    text += "Model:{0}".format( event.model )

    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( text, (0, 1), xycoords="axes fraction", va="top", ha="left",
                 fontsize=12, bbox=dict(edgecolor='none',boxstyle="round, pad=2", fc="w"))
    total += 1

    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( text2, (0, 1), xycoords="axes fraction", va="top", ha="left",
                 fontsize=12, bbox=dict(edgecolor='none',boxstyle="round, pad=2", fc="w"))

    # Only run if library ObsPy is present on system.
    if beachball:

        mt = [ float(results['Mxx']), float(results['Myy']), float(results['Mzz']),
            float(results['Mxy']), float(results['Mxz']), float(results['Myz']) ]

        bb = beachball(mt, mopad_basis='NED', xy=(0,0), width=30, linewidth=1,
                edgecolor='k', facecolor=event_color)

        #bb.set_zorder(0)

        ax.add_collection( bb )
        #ax.set_aspect("equal")
        ax.set_xlim((-100, 30))
        ax.set_ylim((-20, 20))


    total += 1

    # Third panel for beachball
    ax = fig.add_subplot(total_stations,3,total, frameon=True)
    ax.patch.set_alpha(0.0)
    #ax.set_aspect("equal")

    # Calculate size of MT form Event to Station distance
    # Look for closest station for us to calculate a scale factor for our
    # MT beachball on the station map.
    closest = sorted(stations.keys(), key=lambda x: float(stations[x].distance) )[0]
    #event_scale = interp(stations[ closest ].distance,[0,500],[8,15])
    event_scale = 20
    size = interp(len(stations),[0,15],[10,4])
    stanumber = 0

    # If we don't have OBSPY then we can add a marker for the event
    pyplot.plot( results['event'].lon, results['event'].lat,
            '*', ms=event_scale, color=event_color, zorder=0)

    for sta in sorted(stations.keys(), key=lambda x: float(stations[x].realdistance) ):

        lat,lon = results['event'].location(sta)
        elog.debug( '%s (%s,%s)' % (sta, lat, lon) )

        color =  colors.cnames.items()[stanumber][0]

        pyplot.plot( lon, lat, '^', ms=size, color=color)

        if lon > results['event'].lon:
            horizontal = 'left'
        else:
            horizontal = 'right'

        ax.annotate(sta, xy=(lon, lat),
                ha=horizontal, va='bottom', size=10)


        stanumber += 1

    ax.set_ymargin(0.25);
    ax.set_xmargin(0.25);
    ax.autoscale_view()
    pyplot.yticks(ax.get_yticks()[::3],size=7)
    pyplot.xticks(ax.get_xticks()[::3],size=7)

    total += 1

    stanumber = -1
    for sta in sorted(stations.keys(), key=lambda x: float(stations[x].realdistance) ):

        stanumber += 1
        elog.debug('Plot traces for results on %s' % sta )
        if not stations[sta].real:
            elog.error('Empty Records for data on %s' % sta )
        if not stations[sta].synth_zrt:
            elog.error('Empty Records for converted ZRT on %s' % sta)

        distance = int( float(stations[sta].realdistance) )
        azimuth = int( float(stations[sta].azimuth) )
        #real = stations[sta].real
        convertedsynth = stations[sta].synth_zrt
        try:
            zcor = results['zcor'][sta]
        except:
            zcor = '-'
        variance = round( results['variance'][sta], 1)

        try:
            f = stations[sta].filter.split()
            if len(f) == 5:
                filter_used = '%s-%s Hz' % (f[1],f[3])
            else:
                filter_used = stations[sta].filter
        except:
            filter_used = 'unknown filter'

        # Scale all traces the same way
        axs = (0, points_plot, min_plot, max_plot)


        #for chan, data in real:
        for chan in ['T', 'R', 'Z']:
            ax = fig.add_subplot(total_stations,3,total)
            #real_line, = pyplot.plot(data, label='data' )
            if zcor > 0:
                real_line, = pyplot.plot(stations[sta].real.get(chan)[zcor:], label='data' )
                synth_line, = pyplot.plot(stations[sta].synth_zrt.get(chan)[:-zcor],
                        linestyle='--', label='synth')
            else:
                real_line, = pyplot.plot(stations[sta].real.get(chan)[:-zcor], label='data' )
                synth_line, = pyplot.plot(stations[sta].synth_zrt.get(chan)[zcor:],
                        linestyle='--', label='synth')

            if beachball:
                box_color =  colors.cnames.items()[stanumber][0]
                ax.spines['bottom'].set_color(box_color)
                ax.spines['top'].set_color(box_color)
                ax.spines['right'].set_color(box_color)
                ax.spines['left'].set_color(box_color)

            pyplot.legend(loc=5, fontsize=8)

            pyplot.axis( axs )
            pyplot.ylabel('centimeters', fontsize=8)
            pyplot.yticks(size=8)

            ax.get_xaxis().set_ticks([])
            #ax.get_yaxis().set_ticks([])
            ax.yaxis.set_major_formatter(pyplot.FormatStrFormatter('%.1e'))

            # Top of plot
            pyplot.text (0.5, 0.9, r'$\mathbf{%s}\ -\ \mathbf{%s}\ \ %s_{km}\ \ %s ^o$ %s' % \
                    (sta,chan,distance,azimuth,filter_used),
                  horizontalalignment='center', fontsize=13,
                  verticalalignment='center', transform = ax.transAxes)

            max_amp = stations[sta].real.getmax( chan )
            min_amp = stations[sta].real.getmin( chan )
            abs_amp = max_amp - min_amp

            # Bottom of plot
            text = "MaxAmp: %0.1e (cm)   zcor:%s   VR:%s" % (abs_amp, zcor, int(variance))
            pyplot.text (0.5, 0.1,text, horizontalalignment='center', fontsize=11,
                  verticalalignment='center', transform = ax.transAxes)


            pyplot.draw()
            total += 1

    title = "%s %s Mw    " % ( prelim, results['Mw'] )
    title += results['event'].strtime
    pyplot.suptitle(title, fontsize=18, ha='center')

    text = "%s \n" % results['event'].strtime

    # Acknowledgement panel
    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( unicode(acknowledgement, "utf-8"), (0, 0), xycoords="axes fraction", va="bottom", ha="left",
                 fontsize=8, bbox=dict(edgecolor='gray',boxstyle="round, pad=2", fc="w"))

    # Extra info panel
    total += 1
    text = "%s\n" % ' '.join( sys.argv )
    #text += "%s\n" % os.environ['ANTELOPE']
    text += "Generated at %s" % stock.strtime( stock.now() )

    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( unicode(text, "utf-8"), (0, 0), xycoords="axes fraction", va="bottom", ha="left",
                 fontsize=8, bbox=dict(edgecolor='gray',boxstyle="round, pad=2", fc="w"))

    try:
        if not os.path.isdir(folder): os.makedirs(folder)
    except Exception,e:
        elog.error("Problems while creating folder [%s] %s" % (folder,e))

    # Name of file for image
    database_name = os.path.basename(event.database)
    if prelim:
        filename = "%s/dbmoment_%s_%s_%s-PRELIMINARY-RESULT.png" % ( folder, database_name, event.orid,
                event.model )
    else:
        filename = "%s/dbmoment_%s_%s_%s.png" % ( folder, database_name, event.orid,
                event.model )

    if os.path.isfile( filename ):
        elog.notify( 'Remove previous version of the image file: %s' %  filename)
        try:
            os.remove( filename )
        except Exception,e:
            elog.error( 'Cannot remove previous version of image [%s]' % filename )

    elog.notify( 'Save plot with results to temp folder: %s' %  filename)
    pyplot.savefig(filename,bbox_inches='tight', edgecolor='none',pad_inches=0.5, dpi=100)

    return filename


def clean_trace(data, samplerate, trace_start, trace_end, need_start=False, need_end=False):
    '''
    Apply time stamps to our values and cut extra data.
    Return clean list of touples.
    '''

    #from __main__ import elog

    new_data = []
    period = 1/samplerate

    if not need_start: need_start = trace_start
    if not need_end: need_end = trace_end

    elog.debug( "clean_trace samplerate:[%s]" % (samplerate) )
    elog.debug( "clean_trace points:[%s]" % (len(data)) )
    elog.debug( "actual[%s,%s]" % (trace_start, trace_end) )
    elog.debug( "needed[%s,%s]" % (need_start, need_end) )

    for d in data:
        if trace_start >= need_start and trace_start <= need_end:
            new_data.append(d)
        trace_start += period

    elog.debug('New trace is: %s' % len(new_data) )

    return new_data

def verify_trace_time(start, end, time, endtime):
    '''
    Verify that we have the requested data
    '''
    #from __main__ import elog
    elog.debug( "Requested time: [%s,%s]" % (stock.strtime(start),stock.strtime(end)) )
    elog.debug( "In trace object: [%s,%s]" % (stock.strtime(time),stock.strtime(endtime)) )

    tw = end - start

    if endtime - time < tw:
        elog.warning('Got %s secs but expected [%s].' % ( (endtime-time), tw) )
        return False

    if start < time:
        elog.warning('Trace starts [%s] seconds late.' % ( time - start ) )
        return False

    if endtime < end:
        elog.warning('Trace ends [%s] seconds early.' % ( end - endtime ) )
        return False

    return True

def dynamic_loader(module):
    '''
    Load some libs defined on the pf file.
    '''
    #from __main__ import elog
    elog.debug( "load dbmoment.%s" % module )
    try:
        return __import__("dbmoment.%s" % module, globals(), locals(), [module], -1)
    except Exception,e:
        elog.error("Import Error: [%s] => [%s]" % (module,e) )

def cleanup(folder):
    """
    There are serveral files that we produce and keep
    in the tmp directory. Some are known but others are
    random names.
    """

    #from __main__ import elog

    try:
        if not os.path.isdir(folder): os.makedirs(folder)
    except Exception,e:
        elog.error("Problems while creating folder [%s] %s" % (folder,e))

    filelist  = [
        'dbmoment*.Helm_data',
        'temp_data*',
        'tmp_data*',
        'tmp*',
        'mt_inv*',
        'run_fkrsort',
        'run_filter',
        'TEMP_MODEL',
        'GREEN.1',
        'junk',
        'plot',
        'vec'
    ]

    for f in filelist:
        try:
            temp_file = "%s/%s" % (folder,f)
            elog.debug( 'unlink( %s )' % temp_file )
            map( os.unlink, glob.glob( temp_file ) )
        except Exception, e:
            elog.error('Cannot remove temp file %s [%s]' % (temp_file,e) )

def new_Helm_header(samples,samplerate):
    temp = '     %0.4e     %0.4e      0  0  0.00\n' % (0,0)
    temp += '%8d   %0.5f  %.4e\n' % (samples,1/samplerate,0)
    return temp

def fix_format_data(point, decimals, total):
    new = "%0.*e" % (decimals,point)
    return new.rjust(total)

def makeHelm(traces, append='', outputfile='', perline=7, total=14, decimal=5, folder='.dbmoment'):
    """
    Routine to write Helmberger Format Seismograms
    """
    elog.debug('makeHelm')

    filename = {}

    if not outputfile:
        (f, outputfile) = mkstemp(suffix='.Helm_data', prefix='dbmoment_%s_' % append,
                                dir=folder, text=True)
        f = os.fdopen(f, 'w')
    else:
        try:
            f = open(outputfile, 'w')
        except Exception,e:
            self.elog.error('Cannot open new file %s %s'% (outputfile,e))

    elog.debug('New data file %s' % outputfile )


    if len(traces.list()) == 3:
        # From dbmoment.xpy code. Also in data.py
        global seismic_channels
        channels = seismic_channels
    else:
        # From dbmoment.xpy code.
        global synth_channels
        channels = synth_channels

    # File header
    text = ''
    text += "%8d\n" % ( len(traces.list()) )
    text += "(%de%d.%d)\n" % (perline, total, decimal)

    elog.debug('coded channels %s' % channels )

    elog.debug('in object %s' % traces.list() )

    for chan in channels:
        elog.debug('appending %s to %s' % (chan, outputfile) )
        #elog.debug('data[ %s:%s ]' % (index_min, index_max) )

        data = traces.get(chan)
        #data = data[0:60]
        elog.debug('data[ %s ]' % ( len(data) ) )
        #data = data[index_min:index_max]
        samples = len(data)

        # Add channel block header
        text += new_Helm_header( samples, traces.samplerate )

        newline = 0

        for point in data:

            newline += 1
            text += fix_format_data(point, decimal, total)

            if newline == perline:
                text += '\n'
                newline = 0

        text += "\n"

    try:
        f.write(text)
        f.close()
    except Exception,e:
        elog.error('Cannot write to new file %s %s'% (outputfile,e))

    return outputfile


def readHelm(inputfile):
    """
    Routine to read Helmberger Format Seismograms
    Returning full object:
        traces[chan]['data'] = data
        return traces

    """
    #from __main__ import elog
    elog.debug('readHelm: %s' % inputfile)
    results = {}


    fo = open(inputfile, "r")

    # Number of channels
    try:
        total_chans = int( fo.readline().strip() )
        elog.debug( "Total channels [%s]" % total_chans )
    except:
        elog.error("NOT VALID FILE [%s]" % inputfile)

    if total_chans == 1:
        channels = 'X'
    if total_chans == 3:
        # From dbmoment.xpy code. Also in data.py
        global seismic_channels
        channels = seismic_channels
    else:
        # From dbmoment.xpy code.
        global synth_channels
        channels = synth_channels

    # Data Format
    try:
        data_format = fo.readline().strip()
        elog.debug('file format: %s' % data_format )
        temp = re.match("\((\d+)e(\d+)\.(\d+)\)",data_format)
        perline = int(temp.group(1))
        spaces = int(temp.group(2))
        elog.debug('perline: %s  spaces: %s' % (perline, spaces) )


    except:
        elog.error( "NOT VALID FILE [%s]" % inputfile )

    for chan in range(total_chans):
        channame = channels[chan]
        elog.debug( "chan %s" % channame )
        try:
            header1 = fo.readline().strip().split()
            header2 = fo.readline().strip().split()
            total_points = int( header2[0] )
            samplerate =  1/float( header2[1] )
        except:
            elog.error( "NOT VALID FILE [%s]" % inputfile )

        if not total_points or not samplerate:
            elog.error( "NOT VALID FILE [%s]" % inputfile )

        cache = []
        elog.debug( "Total points [%s]" % total_points )
        elog.debug( "Sampelrate [%s]" % samplerate )

        while (total_points > 0):
            row = fo.readline().strip('\n')
            #elog.info('row: %s' % row)
            #elog.info('missing: %s' % total_points)
            if not row: elog.error( 'Problems readHelm(%s)' % inputfile )

            while ( len(row) > 0 ):
                point = float(row[0:spaces])
                row = row[spaces:]
                #elog.info('%s' % point)
                cache.append( point )
                total_points -= 1

            #row = [float(row[i:i+spaces]) for i in range(0, perline)]
            #cache.extend( row )
            #total_points -= len( row )

        cache = array( cache )
        if total_chans == 1: return cache

        if not results:
            # need new object
            results =  Records(samplerate)
            results.file = inputfile

        results.trace( channame, cache )

    return results


def open_verify_pf(pf,mttime=False):
    '''
    Verify that we can get the file and check
    the value of PF_MTTIME if needed.
    Returns pf_object
    '''

    elog.debug( 'Look for parameter file: %s' % pf )

    if mttime:
        elog.debug( 'Verify that %s is newer than %s' % (pf,mttime) )

        PF_STATUS = stock.pfrequire(pf, mttime)
        if PF_STATUS == stock.PF_MTIME_NOT_FOUND:
            elog.complain( 'Problems looking for %s. PF_MTTIME_NOT_FOUND.' % pf )
            elog.die( 'No MTTIME in PF file. Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_MTIME_OLD:
            elog.complain( 'Problems looking for %s. PF_MTTIME_OLD.' % pf )
            elog.die( 'Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_SYNTAX_ERROR:
            elog.complain( 'Problems looking for %s. PF_SYNTAX_ERROR.' % pf )
            elog.die( 'Need a working version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_NOT_FOUND:
            elog.complain( 'Problems looking for %s. PF_NOT_FOUND.' % pf )
            elog.die( 'No file  %s found!!!' % pf )

        elog.debug( '%s => PF_MTIME_OK' % pf )

    try:
        return stock.pfread( pf )
    except Exception,e:
        elog.die( 'Problem looking for %s => %s' % ( pf, e ) )


def get_model_pf( mfile, path=[]):
    '''
    EARTH VELOCITY MODEL FILE:

    Need to verify if we have the listed velocity model.
    The file has a PF format but is not placed on the
    regular folder with the rest of the parameter files
    from contrib. That requires a full search on several
    paths that we get from a parameter in the dbmoment.pf
    file.
    '''
    pf = False

    elog.debug('Get model: %s in %s' % (mfile, path) )

    for d in path:
        try:
            elog.debug('Look for model: %s' % os.path.join(d, mfile) )
            pf = stock.pfin(os.path.join(d, mfile) )
            break
        except:
            pass
        else:
            break # Stop if we find one

    if not pf:
        elog.die('Missing [%s] in [%s]' % ( mfile, ', '.join(path) ) )

    return pf



def safe_pf_get(pf,field,defaultval=False):
    '''
    Safe method to extract values from parameter file
    with a default value option.
    '''

    value = defaultval
    if pf.has_key(field):
        try:
            value = pf.get(field,defaultval)
        except Exception,e:
            elog.die('Problems safe_pf_get(%s,%s)' % (field,e))
            pass

    if isinstance( value, (list, tuple)):
        value = [x for x in value if x]

    elog.debug( "pf.get(%s,%s) => %s" % (field,defaultval,value) )

    return value




if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
