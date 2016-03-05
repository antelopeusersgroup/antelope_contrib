from __main__ import *      # Get all the libraries from parent

def myround(x, base=5):
    logging.debug('Round %s to base %s' % (x, base) )
    return int(int(base) * round(float(x)/int(base)))

def run(cmd,directory='.'):
    logging.debug("run()  -  Running: %s" % cmd)
    p = subprocess.Popen([cmd], stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE,
                         cwd=directory, shell=True)
    stdout, stderr = p.communicate()

    if stdout:
        for line in iter(stdout.split('\n')):
            logging.debug('stdout:\t%s'  % line)
    if stderr:
        for line in iter(stderr.split('\n')):
            logging.debug('stderr:\t%s'  % line)

    if p.returncode != 0:
        logging.error('Exitcode (%s) on [%s]' % (p.returncode,cmd))

    if stdout:
        return iter(stdout.split('\n'))
    if stderr:
        return iter(stderr.split('\n'))
    return iter()

def fix_amplitud(tr, value):
    logging.debug('Fix amplitud by %s' % (value) )

    # Need to double our traces
    for rec in tr.iter_record():
        data = array( rec.trdata() )

        rec.trputdata( data * value )

    return tr

#def filter_with_padding(tr, filter):
#    logging.debug('Aplly filter %s' % (filter) )
#
#
#    # Need to double our traces
#    for rec in tr.iter_record():
#
#        data = array( rec.trdata() )
#
#        #ones = pylab.ones( len(data) )
#        zeros_data = zeros( len(data) )
#        #zeros_data = zeros( 200 )
#        #ones_data *= data[0]
#
#        rec.trputdata( pylab.concatenate([zeros_data , data]) )
#
#
#    # apply the filter
#    tr.trfilter( filter )
#
#    # remove the padding
#    for rec in tr.iter_record():
#
#        data = array( rec.trdata() )
#        rec.trputdata( data[len(data)/2:] )
#        #rec.trputdata( data[200:] )
#
#    return tr

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
        data = rec.trdata()

        if debug:
            # Test rotations with this synth data. Just a spike
            original_samples = len(data)
            ones_data = list( [1] * len(data) )
            ones_data[ (rec+1) * 1000 ] = 30000
            tr.trputdata( ones_data )

        add_trace_to_plot( data, style=style, label='%s-%s' % (name, rec.getv('chan')[0]),
                count=tr.record_count, item=this, delay=delay, jump=jump)

        # start and end marks
        #pyplot.axvline( (start - got_time ) * samprate, color='y')
        #pyplot.axvline( ( (start - got_time ) + tw )* samprate , color='y')

        this += 1

    return fig


def apply_response(tr, filename, samprate):
    logging.debug('Aplly response %s' % (filename) )

    if not filename or not os.path.isfile( filename ):
        logging.warning('Problems loading response file: [%s]' % (filename))
        return tr

    try:
        respaddr = response._get_response(filename)
        logging.debug('respaddr')
        logging.debug(respaddr)
        if not respaddr: raise RuntimeError
    except Exception,e:
        logging.error('Problems loading response file: %s => %s' % (filename, e))

    for rec in tr.iter_record():
        data = array( rec.trdata() )
        logging.debug( 'Total samples: %s ' % len(data) )

        npts = len(data)

        logging.debug('compute the one-dimensional discrete Fourier Transform')
        fft_values = fft(data)[0:int(npts/2)+1]

        # Return the Discrete Fourier Transform sample frequencies
        logging.debug('compute the Fourier Transform sample frequencies')
        freq = fftfreq(len(data), d=1.0/samprate)[range(0,npts/2+1)]

        # the last element is negative, because of the symmetry, but should
        # be positive
        fft_values[-1] *= -1


        convolved = []
        for f in freq:
            w = f * 2 * pi
            convolved.append( response._eval_response(respaddr, w ) )

        data = irfft( convolved * fft_values )

        logging.debug( 'Total samples: %s ' % len(data) )
        rec.trputdata( data )

    response._free_response ( respaddr )

    return tr

def decimate_trace( tr, newsps ):
    """
    Function to correctly apply some decimation filters

    """

    logging.debug('Now try to decimate the data')


    # Find samplerate
    tr.record = 0
    oldsps = tr.getv('samprate')[0]

    total = tr.record_count

    # no need. return
    if oldsps == newsps: return tr

    # Need to double our traces
    for rec in range(tr.record_count):
        tr.record = rec
        data = tr.trdata()
        zeros_data = zeros( len(data) )
        ones_data = ones( len(data) )
        ones_data *= data[0]
        tr.trputdata( concatenate([zeros_data , data]) )

    # Bring pointer back
    tr.record = datascope.dbALL

    try:
        tr.trfilter('DECIMATE BY %i' % oldsps)
        # decimate data
        #for f in decimate_file(oldsps,newsps):
        #    full_path = os.environ['ANTELOPE'] + '/contrib/data/responses/' + f
        #    logging.info('filter(DECIMATE %s)' % full_path)
        #    tr.trfilter('DECIMATE %s' % full_path)
    except Exception,e:
        logging.error('decimate %s: %s' % (Exception,e))

    # Need to cut back the traces
    for rec in range(tr.record_count):
        tr.record = rec
        #data = tr.trdata()
        #start = int(len(data)/2)
        #start = len(data)/2
        #end =  -1
        #tr.trputdata( data[start:end] )
        #tr.trputdata( tr.trdata() )
        #tr.trputdata( data[len(ones):-1] )


    tr.record = 0
    samprate = tr.getv('samprate')[0]
    logging.debug('Old sps: %s New sps: %s' % (oldsps, samprate))
    if ( samprate != newsps): logging.error('Problems decimating from %s to %s' % ( sps, newsps ) )

    tr.record = datascope.dbALL

    return tr


def decimate_file(have=1,want=1):
    """
    Function to return needed response file for proper decimation.

        DECIMATE fir_file
            This implements a decimation filter. The decimation is performed
            after applying one or more FIR anti-alias filters to  the  data.
            The  FIR  filters  are  specified  in  the  standard  instrument
            response  file,   fir_file   (the   format   is   specified   in
            response(5)).   The  response  function  specified  in  fir_file
            should only contain FIR stages with  their  decimation  factors.

    Selecting a series of files with simple decimation factors to build our
    new time-series with the requested samplerate.
        % egrep "decimation factor" * |grep trident
            (standard input):96:trident_100sps_fir1:# decimation factor     15
            (standard input):97:trident_100sps_fir2:# decimation factor     10
            (standard input):95:trident_1000sps_fir3:# decimation factor     2

    """

    logging.debug("FKRPROG.PY: decimate_file()")

    factor = int(have/want)

    logging.debug("FKRPROG.PY: decimate_file()  -  factor [%s]" % factor)

    if factor == 1:
        return []
    elif factor == 2:
        return ['F96CM']
    elif factor == 4:
        return ['F72BM']
    elif factor == 10:
        return ['F260M']
    elif factor == 20:
        return ['dec20to1']
    elif factor == 40:
        return ['dec40to1']
    elif factor == 50:
        return ['FS2D5M','F260M']
    elif factor == 100:
        return ['F260M','F260M']
    else:
        raise SystemExit('\n\nERROR: Cannot decimate by (%s) only [2,4,10,20,40,50,100]\n\n' % factor)


def fix_exec(content):

    global executables
    if len(executables):
        for src, target in executables.iteritems():
            content = re.sub(r"^%s " % src, "%s " % target, content)

    return content


def cleanup_db(db,match):
    logging.debug( 'dbTABLE_PRESENT => %s' % db.query(datascope.dbTABLE_PRESENT) )
    logging.debug( 'dbTABLE_IS_WRITABLE => %s' % db.query(datascope.dbTABLE_IS_WRITABLE) )

    need_crunch = False
    if db.query(datascope.dbTABLE_PRESENT):
        while db.record_count:

            try:
                record = db.find(match, first=-1)
            except Exception,e:
                record = -1

            if record < 0: break
            logging.debug('Found previous record %s for %s' % (record, match))
            db.record = record
            db.mark()
            logging.debug('%s marked' % record)
            need_crunch = True

        if need_crunch:
            logging.debug('db.crunch()' )
            db.crunch()



class fkrprogException(Exception):
    """
    Local class to raise Exceptions
    """
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return "fkrprogException: %s" % self.msg
    def __str__(self):
        return repr(self)


class Records():
    """
    Class for tracking info from a single sta
    """

    def __init__(self,sps=0,segtype=None,response=None):
        self.chans = {}
        self.samplerate = sps
        self.file = None
        self.segtype = segtype
        self.response = response

    def __iter__(self):
        self.chan_list = sorted( self.chans.keys() )
        logging.debug( 'Chans in Record: %s' % self.chan_list )
        return self

    def next(self):
        try:
            chan = self.chan_list.pop(0)
            return chan, self.chans[chan]

        except:
            raise StopIteration

    def list(self):
        self.chan_list = sorted( self.chans.keys() )
        #logging.debug( 'Chans in Record: %s' % self.chan_list )
        return self.chan_list

    def flip(self):
        for chan in self.chans.keys():
            logging.debug('Flip channel %s' % chan )
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

    def getmin(self,chan=False):
        if not chan:
            channame = self.list()
        else:
            channame = [chan]
        vmin = False
        for c in channame:
            v = min(self.chans[c])
            try:
                if v < vmin:
                    vmin = v
            except:
                vmin = v

        return vmin

    def getmax(self,chan=False):
        if not chan:
            channame = self.list()
        else:
            channame = [chan]

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
        logging.debug('Aplly calibration of [%s] to channel %s' % (calib,chan) )
        logging.debug( self.chans[chan] )

        self.chans[chan] *= calib
        logging.debug( self.chans[chan] )


    def get_data(self,chan):
        #logging.debug('Get %s from trace' % chan)
        if chan and chan in self.chans:
            return self.chans[chan]
        return self.chans

    def window(self,start=0,end=-1):
        #logging.debug('Get %s from trace' % chan)
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

    def clean(self):
        logging.debug( 'clean object for : %s' % self.sta )
        self._remove_file( self.real_file )
        self._remove_file( self.synth_file )

    def _remove_file(self, filename ):

        if os.path.isfile( filename ):
            try:
                os.remove( filename )
            except:
                logging.warning( 'cannot remove file: %s' % filename )


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
            #self.real_file = makeHelm(self.real, 0, self.real_samples,
            self.real_file = makeHelm(self.real,
                    append='%s-real' % self.sta, folder=self.tmp_folder)
            self.real_file_name = os.path.basename(self.real_file)
            return self.real_file
        else:
            #self.synth_file = makeHelm(self.synth, 0, self.synth_samples,
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
            logging.debug( 'synt: %s' % self.synth_samples )
            vmax = self.synth.getmax()
            vmin = self.synth.getmin()
            samps = self.synth_samples

        return (0,samps,vmin,vmax)


    def convert_synth_original(self, results):

        # apply results to greens for plotting

        try:
            self.zcor = float(results['zcor'][self.sta])
        except:
            self.zcor = 0

        chans =  {'rad':'R','ver':'Z','tan':'T'}

        for m in ['rad','ver','tan']:
            for f in ['synth.data.','new.','']:
                try:
                    os.unlink('%s/%s%s' % (self.tmp_folder,f,m))
                except:
                    pass

        # Get the data into sac format
        cmd = "putmt in=%s out=synth.data azimuth=%s " % (self.synth_file,int(float(self.azimuth)) )
        cmd += " mxx=%s" % results['Mxx']
        cmd += " mxy=%s" % results['Mxy']
        cmd += " mxz=%s" % results['Mxz']
        cmd += " myy=%s" % results['Myy']
        cmd += " myz=%s" % results['Myz']
        cmd += " mzz=%s" % results['Mzz']
        cmd += " moment=%s" % results['Mo']
        logging.debug( cmd )
        run(fix_exec(cmd),self.tmp_folder)

        for f in ['rad','ver','tan']:
            cmd = 'sac2bin in=synth.data.%s out=%s' % (f,f)
            logging.debug( cmd )
            run(fix_exec(cmd),self.tmp_folder)

            cmd = 'mkHelm ntr=1 nt=%s dt=1.0 format="(6e12.5)" < %s > new.%s' % (self.synth_samples,f,f)
            logging.debug( cmd )
            run(fix_exec(cmd),self.tmp_folder)

            data = readHelm( '%s/new.%s' % (self.tmp_folder,f) )

            logging.debug( 'zcor is %s' % self.zcor)

            if self.zcor < 0.0:
                logging.debug( 'remove %s points' % self.zcor)
                data = delete(data, self.zcor, 0)
            elif self.zcor > 0.0:
                logging.debug( 'add %s points' % self.zcor)
                data = insert(data, 0, ones(int(self.zcor)) * data[0], 0)

            self.synth_zrt.set(chans[f], data )

        self.flip('real')
        #self.flip('synth')


    #def convert_synth(self, results):


    #    self.zcor = float(results['zcor'][self.sta])
    #    az = array( float(self.azimuth) )
    #    self.synth_zrt = Records()

    #    TSS = array( self.synth.get('TSS') )
    #    TDS = array( self.synth.get('TDS') )
    #    XSS = array( self.synth.get('XSS') )
    #    XDS = array( self.synth.get('XDS') )
    #    XDD = array( self.synth.get('XDD') )
    #    ZSS = -1 * array( self.synth.get('ZSS') )
    #    ZDS = -1 * array( self.synth.get('ZDS') )
    #    ZDD = -1 * array( self.synth.get('ZDD') )

    #    XX = array(float(results['Mxx']))
    #    YY = array(float(results['Myy']))
    #    ZZ = array(float(results['Mzz']))
    #    XY = array(float(results['Mxy']))
    #    XZ = array(float(results['Mxz']))
    #    YZ = array(float(results['Myz']))

    #    Z = XX*( (ZSS/2 - cos(2*az)) * ZDD/3 )
    #    Z += YY*( -1 * (ZSS/2 * cos(2*az)) - ZDD/6 )
    #    Z += ZZ*( ZDD/3 )
    #    Z += XY* ZSS * sin(2*az)
    #    Z += XZ* ZDS * cos(az)
    #    Z += YZ* ZDS * sin(az)
    #    self.synth_zrt.set('Z',Z)

    #    R = XX*( (XSS/2 * cos(2*az)) - XDD/6 )
    #    R += YY*( -1 * (XSS/2 * cos(2*az)) - XDD/6 )
    #    R += ZZ*( XDD/3 )
    #    R += XY* XSS * sin(2*az)
    #    R += XZ* XDS * cos(az)
    #    R += YZ* XDS * sin(az)
    #    self.synth_zrt.set('R',R)

    #    T = XX * TSS / 2 * sin(2*az)
    #    T -= YY / 2 * TSS * sin(2*az)
    #    T -= XY * TSS * cos(2*az)
    #    T -= XZ* TDS * sin(az)
    #    T += YZ* TDS * cos(az)
    #    self.synth_zrt.set('T',T)

    #    if self.zcor < 0.0:
    #        T = delete(T, self.zcor, 0)
    #        R = delete(R, self.zcor, 0)
    #        Z = delete(Z, self.zcor, 0)
    #    elif self.zcor > 0.0:
    #        T = insert(T, 0, ones(self.zcor) * T[0], 0)
    #        R = insert(R, 0, ones(self.zcor) * T[0], 0)
    #        Z = insert(Z, 0, ones(self.zcor) * T[0], 0)

    #    self.synth_zrt.set('T', T )
    #    self.synth_zrt.set('R', R )
    #    self.synth_zrt.set('Z', Z )

    #    self.flip('synth')

    def plot(self,trace='real'):
        # open a plot of the data
        logging.debug('Plot traces for %s' % self.sta )

        if trace is 'real':
            records = self.real
        else:
            records = self.synth

        if not records:
            logging.error('Empty Records for %s data on %s' % (trace,self.sta) )

        axs = self.max_min(trace)

        total = 0
        pyplot.figure()
        channels = records.list()
        for chan, data in records:
            logging.debug( 'add %s_%s to plot' % (self.sta,chan) )
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

    if not len(execs): return

    global executables

    for ex in execs:
        logging.debug("Find executable for (%s)" % ex)

        newex = spawn.find_executable(ex)

        if not newex:
            logging.error("Cannot locate executable for [%s] in $PATH = \n%s" % \
                    (ex,os.environ["PATH"].split(os.pathsep)) )

        executables[ex] = os.path.abspath( newex )

        logging.debug("(%s) => [%s]" % (ex, newex) )

def plot_results( id, stations, results, event, folder='./',
                       acknowledgement='dbmoment'):


    total = 1


    #N = len(stations.keys())
    total_stations = len(stations.keys()) + 2
    gcf = pyplot.gcf()
    #fig = pyplot.figure(figsize=( 20, (3*N) ))
    fig = pyplot.figure(figsize=( 20, 3 * total_stations ))

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



    # First panel
    text = "%s \n" % event.strtime
    text += "\n"
    text += "ID: %s   %s\n" % (id, results['estatus'] )
    text += "\n"
    text += "Location: \n"
    text += "   Lat:    %s \n" % event.lat
    text += "   Lon:    %s \n" % event.lon
    text += "   Depth:  %s km\n" % event.depth
    text += "Filter:    %s \n" % event.filter
    text += "Model:     %s" % event.model
    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( text, (0, 1), xycoords="axes fraction", va="top", ha="left",
                 fontsize=12, bbox=dict(edgecolor='none',boxstyle="round, pad=2", fc="w"))
    total += 1

    # Second panel
    text = "Mw:       %s \n" % results['Mw']
    text += "Strike:%s Rake:%s Dip:%s\n" % \
            ( results['Strike'], results['Rake'], results['Dip'] )
    text += "Pdc:      %0d %%\n" % (results['Pdc'] * 100)
    text += "Pclvd:    %0d %%\n" % (results['Pclvd'] * 100)
    text += "VAR:   %s \n" % results['Variance']
    text += "VarRed:     %s \n" % results['VarRed']
    text += "Var/Pdc:    %s \n" % results['Var/Pdc']
    text += "Mo:         %s \n" % results['Mo']
    text += "Mxx:%0.3f  Mxy:%0.3f  Mxz:%0.3f\n" % (float(results['Mxx']), float(results['Mxy']), float(results['Mxz']))
    text += "Myy:%0.3f  Myz:%0.3f  Mzz:%0.3f\n" % (float(results['Myy']), float(results['Myz']), float(results['Mzz']))
    ax = fig.add_subplot(total_stations,3,total, frameon=False)
    ax.xaxis.set_visible(False)
    ax.yaxis.set_visible(False)
    ax.patch.set_alpha(0.0)
    ax.annotate( text, (0, 1), xycoords="axes fraction", va="top", ha="left",
                 fontsize=12, bbox=dict(edgecolor='none',boxstyle="round, pad=2", fc="w"))
    total += 1

    # Only run if library ObsPy is present on system.
    if beachball:
        # Third panel for beachball
        ax = fig.add_subplot(total_stations,3,total, frameon=False)
        ax.xaxis.set_visible(False)
        ax.yaxis.set_visible(False)
        ax.patch.set_alpha(0.0)

        ax.annotate( 'ObsPy used for beachball image', (0.5, 0), xycoords="axes fraction", va="top", ha="left",
                     fontsize=7, bbox=dict(edgecolor='none',boxstyle="round, pad=2", fc="w"))

        # TEST FOR COMPARING 3 and 6 components methods
        #mt = [ float(results['Mxx']), float(results['Myy']), float(results['Mzz']),
        #    float(results['Mxy']), float(results['Mxz']), float(results['Myz']) ]
        #mt2 = [ results['Strike'][0], results['Dip'][0], results['Rake'][0], ]
        #bb = beachball(mt, mopad_basis='NED', xy=(-50, -50),width=80)
        #bb2 = beachball(mt2, xy=(50,50), width=80)
        #ax.add_collection( bb )
        #ax.add_collection( bb2 )

        mt = [ float(results['Mxx']), float(results['Myy']), float(results['Mzz']),
            float(results['Mxy']), float(results['Mxz']), float(results['Myz']) ]
        bb = beachball(mt, mopad_basis='NED')
        ax.add_collection( bb )
        ax.set_xlim((-110, 110))
        ax.set_ylim((-110, 110))
        ax.set_aspect("equal")

    total += 1

    #for sta in sorted(stations.keys()):
    for sta in sorted(stations.keys(), key=lambda x: float(stations[x].realdistance) ):

        logging.debug('Plot traces for results on %s' % sta )
        if not stations[sta].real:
            logging.error('Empty Records for data on %s' % sta )
        if not stations[sta].synth_zrt:
            logging.error('Empty Records for converted ZRT on %s' % sta)

        distance = int( float(stations[sta].realdistance) )
        azimuth = int( float(stations[sta].azimuth) )
        #real = stations[sta].real
        convertedsynth = stations[sta].synth_zrt
        try:
            zcor = results['zcor'][sta]
        except:
            zcor = '-'
        variance = round( results['variance'][sta], 1)

        # Scale all traces the same way
        #axs = stations[sta].max_min_all()
        axs = (0, points_plot, min_plot, max_plot)


        #for chan, data in real:
        for chan in ['T', 'R', 'Z']:
            ax = fig.add_subplot(total_stations,3,total)
            #real_line, = pyplot.plot(data, label='data' )
            real_line, = pyplot.plot(stations[sta].real.get(chan), label='data' )
            synth_line, = pyplot.plot(stations[sta].synth_zrt.get(chan), linestyle='--', label='synth')

            pyplot.legend(loc=5, fontsize=8)

            pyplot.axis( axs )
            pyplot.ylabel('centimeters', fontsize=8)
            pyplot.yticks(size=8)

            ax.get_xaxis().set_ticks([])
            #ax.get_yaxis().set_ticks([])
            ax.yaxis.set_major_formatter(pyplot.FormatStrFormatter('%.1e'))

            # Top of plot
            pyplot.text (0.5, 0.9, r'$\mathbf{%s}\ -\ \mathbf{%s}\ \ %s_{km}\ \ %s ^o$' % (sta,chan,distance,azimuth),
                  horizontalalignment='center', fontsize=13,
                  verticalalignment='center', transform = ax.transAxes)

            # Bottom of plot
            text = "zcor:%s   variance_reduction:%s%%" % (zcor, variance)
            pyplot.text (0.5, 0.1,text, horizontalalignment='center', fontsize=11,
                  verticalalignment='center', transform = ax.transAxes)


            pyplot.draw()
            total += 1

    title = "%s Mw    " % results['Mw']
    title += "%s   " % event.strtime
    pyplot.suptitle(title, fontsize=18, ha='center')

    text = "%s \n" % event.strtime

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
        logging.error("Problems while creating folder [%s] %s" % (folder,e))

    filename = "%s/dbmoment_%s_%s_%s.png" % ( folder,id,event.model,event.strdate.replace(' ','_').replace('/','-') )
    logging.notify( 'Save plot with results to temp folder: %s' %  filename)
    #pylab.savefig(filename,bbox_inches='tight', facecolor=gcf.get_facecolor(), edgecolor='none',pad_inches=0.5, dpi=100)
    pyplot.savefig(filename,bbox_inches='tight', edgecolor='none',pad_inches=0.5, dpi=100)

    return filename


def open_verify_pf(pf,mttime=False):
    '''
    Verify that we can get the file and check
    the value of PF_MTTIME if needed.
    Returns pf_object
    '''

    logging.debug( 'Look for parameter file: %s' % pf )

    if mttime:
        logging.debug( 'Verify that %s is newer than %s' % (pf,mttime) )

        PF_STATUS = stock.pfrequire(pf, mttime)
        if PF_STATUS == stock.PF_MTIME_NOT_FOUND:
            logging.warning( 'Problems looking for %s. PF_MTTIME_NOT_FOUND.' % pf )
            logging.error( 'No MTTIME in PF file. Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_MTIME_OLD:
            logging.warning( 'Problems looking for %s. PF_MTTIME_OLD.' % pf )
            logging.error( 'Need a new version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_SYNTAX_ERROR:
            logging.warning( 'Problems looking for %s. PF_SYNTAX_ERROR.' % pf )
            logging.error( 'Need a working version of the %s file!!!' % pf )
        elif PF_STATUS == stock.PF_NOT_FOUND:
            logging.warning( 'Problems looking for %s. PF_NOT_FOUND.' % pf )
            logging.error( 'No file  %s found!!!' % pf )

        logging.debug( '%s => PF_MTIME_OK' % pf )

    try:
        return stock.pfread( pf )
    except Exception,e:
        logging.error( 'Problem looking for %s => %s' % ( pf, e ) )


def get_model_pf( mfile, path=[], forced=None):
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

    # Maybe we have an entry in command-line
    if forced:
        try:
            logging.info('Look for model: %s' % os.path.abspath(forced) )
            pf = stock.pfin(os.path.abspath(forced))
        except:
            # Maybe we should look for it on the model_path array
            mfile = forced

    # If not on command-line then get value from parameter file
    if not pf:
        for d in path:
            try:
                logging.info('Look for model: %s' % os.path.join(d, mfile) )
                pf = stock.pfin(os.path.join(d, mfile) )
            except:
                pass
            else:
                break # Stop if we find one

    if not pf:
        logging.error('Missing [%s] in [%s]' % ( mfile, ', '.join(path) ) )

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
            logging.warning('Problems safe_pf_get(%s,%s)' % (field,e))
            pass

    logging.debug( "pf.get(%s,%s) => %s" % (field,defaultval,value) )

    return value


def clean_trace(data, samplerate, trace_start, trace_end, need_start=False, need_end=False):
    '''
    Apply time stamps to our values and cut extra data.
    Return clean list of touples.
    '''


    new_data = []
    period = 1/samplerate

    if not need_start: need_start = trace_start
    if not need_end: need_end = trace_end

    logging.debug( "clean_trace samplerate:[%s]" % (samplerate) )
    logging.debug( "clean_trace points:[%s]" % (len(data)) )
    logging.debug( "actual[%s,%s]" % (trace_start, trace_end) )
    logging.debug( "needed[%s,%s]" % (need_start, need_end) )

    for d in data:
        if trace_start >= need_start and trace_start <= need_end:
            new_data.append(d)
        trace_start += period

    logging.debug('New trace is: %s' % len(new_data) )

    return new_data

def verify_trace_time(start, end, time, endtime):
    '''
    Verify that we have the requested data
    '''
    logging.debug( "Requested time: [%s,%s]" % (stock.strtime(start),stock.strtime(end)) )
    logging.debug( "In trace object: [%s,%s]" % (stock.strtime(time),stock.strtime(endtime)) )

    tw = end - start

    if endtime - time < tw:
        logging.warning('Got %s secs but expected [%s].' % ( (endtime-time), tw) )
        return False

    if start < time:
        logging.warning('Trace starts [%s] seconds late.' % ( time - start ) )
        return False

    if endtime < end:
        logging.warning('Trace ends [%s] seconds early.' % ( end - endtime ) )
        return False

    return True

def dynamic_loader(module):
    '''
    Load some libs defined on the pf file.
    '''
    logging.debug( "load dbmoment.%s" % module )
    try:
        return __import__("dbmoment.%s" % module, globals(), locals(), [module], -1)
    except Exception,e:
        logging.error("Import Error: [%s] => [%s]" % (module,e) )

def cleanup(folder):
    """
    There are serveral files that we produce and keep
    in the tmp directory. Some are known but others are
    random names.
    """

    try:
        if not os.path.isdir(folder): os.makedirs(folder)
    except Exception,e:
        logging.error("Problems while creating folder [%s] %s" % (folder,e))

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
            logging.debug( 'unlink( %s )' % temp_file )
            map( os.unlink, glob.glob( temp_file ) )
        except Exception, e:
            logging.error('Cannot remove temp file %s [%s]' % (temp_file,e) )

def new_Helm_header(samples,samplerate):
    temp = '     %0.4e     %0.4e      0  0  0.00\n' % (0,0)
    temp += '%8d   %0.5f  %.4e\n' % (samples,1/samplerate,0)
    return temp

def fix_format_data(point, decimals, total):
    new = "%0.*e" % (decimals,point)
    return new.rjust(total)

#def makeHelm(traces, index_min, index_max, append='', outputfile='', perline=7, total=14, decimal=5, folder='.dbmoment'):
def makeHelm(traces, append='', outputfile='', perline=7, total=14, decimal=5, folder='.dbmoment'):
    """
    Routine to write Helmberger Format Seismograms
    """
    logging.debug('makeHelm')

    filename = {}
    #global tmp_folder

    if not outputfile:
        (f, outputfile) = mkstemp(suffix='.Helm_data', prefix='dbmoment_%s_' % append,
                                dir=folder, text=True)
        f = os.fdopen(f, 'w')
    else:
        try:
            f = open(outputfile, 'w')
        except Exception,e:
            self.logging.error('Cannot open new file %s %s'% (outputfile,e))

    logging.debug('New data file %s' % outputfile )


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

    logging.debug('coded channels %s' % channels )

    logging.debug('in object %s' % traces.list() )

    for chan in channels:
        logging.debug('appending %s to %s' % (chan, outputfile) )
        #logging.debug('data[ %s:%s ]' % (index_min, index_max) )

        data = traces.get(chan)
        #data = data[0:60]
        logging.debug('data[ %s ]' % ( len(data) ) )
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
        logging.error('Cannot write to new file %s %s'% (outputfile,e))

    return outputfile


def readHelm(inputfile):
    """
    Routine to read Helmberger Format Seismograms
    Returning full object:
        traces[chan]['data'] = data
        return traces

    """
    logging.debug('readHelm: %s' % inputfile)
    results = {}


    fo = open(inputfile, "r")

    # Number of channels
    try:
        total_chans = int( fo.readline().strip() )
        logging.debug( "Total channels [%s]" % total_chans )
    except:
        logging.error("NOT VALID FILE [%s]" % inputfile)

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
        logging.debug('file format: %s' % data_format )
        temp = re.match("\((\d+)e(\d+)\.(\d+)\)",data_format)
        perline = int(temp.group(1))
        spaces = int(temp.group(2))
        logging.debug('perline: %s  spaces: %s' % (perline, spaces) )


    except:
        logging.error( "NOT VALID FILE [%s]" % inputfile )

    for chan in range(total_chans):
        channame = channels[chan]
        logging.debug( "chan %s" % channame )
        try:
            header1 = fo.readline().strip().split()
            header2 = fo.readline().strip().split()
            total_points = int( header2[0] )
            samplerate =  1/float( header2[1] )
        except:
            logging.error( "NOT VALID FILE [%s]" % inputfile )

        if not total_points or not samplerate:
            logging.error( "NOT VALID FILE [%s]" % inputfile )

        cache = []
        logging.debug( "Total points [%s]" % total_points )
        logging.debug( "Sampelrate [%s]" % samplerate )

        while (total_points > 0):
            row = fo.readline().strip('\n')
            #logging.info('row: %s' % row)
            #logging.info('missing: %s' % total_points)
            if not row: logging.error( 'Problems readHelm(%s)' % inputfile )

            while ( len(row) > 0 ):
                point = float(row[0:spaces])
                row = row[spaces:]
                #logging.info('%s' % point)
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


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
