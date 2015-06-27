from __main__ import *      # Get all the libraries from parent


def log(msg=''):
    if not isinstance(msg, str):
        msg = pprint(msg)
    logger.info(msg)


def debug(msg=''):
    if not isinstance(msg, str):
        msg = pprint(msg)
    logger.debug(msg)


def warning(msg=''):
    if not isinstance(msg, str):
        msg = pprint(msg)
    logger.warning("\t*** %s ***" % msg)


def notify(msg=''):
    if not isinstance(msg, str):
        msg = pprint(msg)
    logger.log(35,msg)


def error(msg=''):
    if not isinstance(msg, str):
        msg = pprint(msg)
    logger.critical(msg)
    sys.exit("\n\n\t%s\n\n" % msg)


def pprint(obj):
    return "\n%s" % json.dumps( obj, indent=4, separators=(',', ': ') )

def run(cmd,directory='./'):
    log("run()  -  Running: %s" % cmd)
    p = subprocess.Popen([cmd], stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT,
                         cwd=directory, shell=True)
    stdout, stderr = p.communicate()

    for line in iter(stdout.split('\n')):
        debug('stdout:\t%s'  % line)
    if stderr: error('stderr present: %s'  % stderr)
    if p.returncode != 0:
        error('Exitcode (%s) on [%s]' % (p.returncode,cmd))

    return iter(stdout.split('\n'))

def fix_exec(content):

    global executables
    if len(executables):
        for src, target in executables.iteritems():
            content = re.sub(r"^%s " % src, "%s " % target, content)

    return content


def cleanup_db(db,match):
    debug( 'dbTABLE_PRESENT => %s' % db.query(datascope.dbTABLE_PRESENT) )
    debug( 'dbTABLE_IS_WRITABLE => %s' % db.query(datascope.dbTABLE_IS_WRITABLE) )

    need_crunch = False
    if db.query(datascope.dbTABLE_PRESENT):
        while True:
            try:
                record = db.find(match, first=-1)
            except Exception,e:
                #log('Problem on dbfind %s %s' % (record, match))
                record = -1
            if record < 0: break
            log('Found previous record %s for %s' % (record, match))
            db.record = record
            db.mark()
            need_crunch = True

        if need_crunch:
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

    def __init__(self,sps=0,segtype=None):
        self.chans = {}
        self.samplerate = sps
        self.file = None
        self.segtype = segtype

    def __iter__(self):
        self.chan_list = sorted( self.chans.keys() )
        log( 'Chans in Record: %s' % self.chan_list )
        return self

    def next(self):
        try:
            chan = self.chan_list.pop(0)
            return chan, self.chans[chan]

        except:
            raise StopIteration

    def list(self):
        self.chan_list = sorted( self.chans.keys() )
        #debug( 'Chans in Record: %s' % self.chan_list )
        return self.chan_list

    def flip(self):
        for chan in self.chans.keys():
            debug('Flip channel %s' % chan )
            self.chans[chan] = self.chans[chan] * -1

    def get(self,channame):
        return self.get_data(channame)

    def set(self,channame,data=[]):
        self.set_data(channame,data)

    def trace(self,channame,data=[]):
        self.set_data(channame,data)

    def set_data(self,channame,data=[]):
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

    def get_data(self,chan):
        #debug('Get %s from trace' % chan)
        if chan and chan in self.chans:
            return self.chans[chan]
        return self.chans

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



    def filter_script(self,trace_type,hpass=0.02,lpass=0.1,npts=1024,period=0.5):

        global tmp_folder

        text =  "#! /bin/csh\nset dt=%s\nset npts=%s\nset lcrn=%s\nset hcrn=%s\n" % \
                (period,npts,hpass,lpass)

        text +=  '\ncd %s \n' % tmp_folder

        if trace_type == 'real':
            text +=  '''
rm -f t r z tmp*
fromHelm < $1 > tmp2
window nt=$npts nx=3 nv=1 v0=0 < tmp2 > tmp3
bin2sac npts=$npts stime=0.0 dt=$dt < tmp3 > t
window nt=$npts nx=3 nv=1 v0=1 < tmp2 > tmp4
bin2sac npts=$npts stime=0.0 dt=$dt < tmp4 > r
window nt=$npts nx=3 nv=1 v0=2 < tmp2 > tmp5
bin2sac npts=$npts stime=0.0 dt=$dt < tmp5 > z

sac << sacend
setbb HCRN $hcrn
getbb HCRN
setbb LCRN $lcrn
getbb LCRN
cut 0 200
read t r z
bp co %LCRN %HCRN p 2
interpolate delta 1.0
write over
quit
sacend

sac2bin in=t out=tmp
mv tmp t
sac2bin in=r out=tmp
mv tmp r
sac2bin in=z out=tmp
mv tmp z
cat t r z > tmp6
rm $2
mkHelm ntr=3 nt=200 dt=1.0 format="(6e12.5)" < tmp6 > $2
rm -f t r z tmp*
'''

        else:
            text +=  '''
rm -f t r z tmp*
fromHelm < $1 > tmp7
window nt=$npts nx=8 nv=1 v0=0 < tmp7 > tmp8
bin2sac npts=$npts stime=0.0 dt=$dt < tmp8 > tss
window nt=$npts nx=8 nv=1 v0=1 < tmp7 > tmp9
bin2sac npts=$npts stime=0.0 dt=$dt < tmp9 > tds
window nt=$npts nx=8 nv=1 v0=2 < tmp7 > tmp10
bin2sac npts=$npts stime=0.0 dt=$dt < tmp10 > xss
window nt=$npts nx=8 nv=1 v0=3 < tmp7 > tmp11
bin2sac npts=$npts stime=0.0 dt=$dt < tmp11 > xds
window nt=$npts nx=8 nv=1 v0=4 < tmp7 > tmp12
bin2sac npts=$npts stime=0.0 dt=$dt < tmp12 > xdd
window nt=$npts nx=8 nv=1 v0=5 < tmp7 > tmp13
bin2sac npts=$npts stime=0.0 dt=$dt < tmp13 > zss
window nt=$npts nx=8 nv=1 v0=6 < tmp7 > tmp14
bin2sac npts=$npts stime=0.0 dt=$dt < tmp14 > zds
window nt=$npts nx=8 nv=1 v0=7 < tmp7 > tmp15
bin2sac npts=$npts stime=0.0 dt=$dt < tmp15 > zdd

sac << sacend
setbb HCRN $hcrn
getbb HCRN
setbb LCRN $lcrn
getbb LCRN
cut 0 200
read tss tds xss xds xdd zss zds zdd
bp co %LCRN %HCRN p 2
interpolate delta 1.0
write over
quit
sacend

sac2bin in=tss out=tmp
mv tmp tss
sac2bin in=tds out=tmp
mv tmp tds
sac2bin in=xss out=tmp
mv tmp xss
sac2bin in=xds out=tmp
mv tmp xds
sac2bin in=xdd out=tmp
mv tmp xdd
sac2bin in=zss out=tmp
mv tmp zss
sac2bin in=zds out=tmp
mv tmp zds
sac2bin in=zdd out=tmp
mv tmp zdd
cat tss tds xss xds xdd zss zds zdd > tmp16
rm $2
mkHelm ntr=8 nt=200 dt=1.0 format="(6e12.5)" < tmp16 > $2
rm -f tss tds xss xds xdd zss zds zdd tmp*
'''
        return text



class Station(Records):
    """
    Class for keeping all information related to
    a particular station.
    """

    def __init__(self,sta):
        self.sta = sta
        self.real = Records()
        self.real_file = False
        self.real_samples = 0
        self.real_samplerate = 0
        self.synth = Records()
        self.synth_zrt = Records()
        self.synth_file = False
        self.synth_samples = 0
        self.synth_samplerate = 0
        self.depth = 0
        self.lat  = False
        self.lon  = False
        self.delta  = False
        self.distance  = False
        self.azimuth  = False
        self.zcor  = False

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
            self.real_file = mkHelm(self.real,append='%s-real' % self.sta)
            self.real.file = self.real_file
            return self.real_file
        else:
            self.synth_file = mkHelm(self.synth,append='%s-synth' % self.sta)
            self.synth.file = self.synth_file
            return self.synth_file

    def save(self,trace='real',data=Records()):
        if trace == 'real':
            self.real = data
            self.real_samples = self.real.samplecount()
            self.real_samplerate = self.real.samplerate
        else:
            self.synth = data
            self.synth_samples = self.synth.samplecount()
            self.synth_samplerate = self.synth.samplerate

    def max_min(self,trace='real'):
        if trace == 'real':
            usecache = self.real
        else:
            usecache = self.synth

        vmax = usecache.getmax()
        vmin = usecache.getmin()

        return (0,100,vmin,vmax)


    def convert_synth(self, results, tmp_folder):

        # Convert GF's to T,R,Z
        #"putmt in=dbmoment_STA1-synth_OdTd83.Helm_data out=synth.data azimuth=10 mxx=-0.289 mxy=0.637 mxz=0.198 myy=0.109 myz=-0.446 mzz=0.181 moment=8.22293e+19"
        #'/opt/antelope/5.4post/bin/sac2bin in=synth.data.rad out=rad'
        #'/opt/antelope/5.4post/bin/mkHelm ntr=1 nt=200 dt=1.0 format="(6e12.5)" < rad > new_rad'

        self.zcor = float(results['zcor'][self.sta])

        chans =  {'rad':'R','ver':'Z','tan':'T'}

        for m in ['rad','ver','tan']:
            for f in ['synth.data.','new.','']:
                try:
                    os.unlink('%s/%s%s' % (temp_folder,f,m))
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
        log( cmd )
        run(fix_exec(cmd),tmp_folder)

        for f in ['rad','ver','tan']:
            cmd = 'sac2bin in=synth.data.%s out=%s' % (f,f)
            log( cmd )
            run(fix_exec(cmd),tmp_folder)

            cmd = 'mkHelm ntr=1 nt=200 dt=1.0 format="(6e12.5)" < %s > new.%s' % (f,f)
            log( cmd )
            run(fix_exec(cmd),tmp_folder)

            #self.synth_zrt.set(chans[f], readHelm( '%s/new.%s' % (tmp_folder,f) ) )

            #data = pylab.array( readHelm( '%s/new.%s' % (tmp_folder,f) )  )
            data = readHelm( '%s/new.%s' % (tmp_folder,f) )
            log( 'zcor is %s' % self.zcor)
            if self.zcor < 0.0:
                log( 'remove %s points' % self.zcor)
                data = pylab.delete(data, self.zcor, 0)
            elif self.zcor > 0.0:
                log( 'add %s points' % self.zcor)
                data = pylab.insert(data, 0, pylab.ones(self.zcor) * data[0], 0)

            #data = -1 * data
            self.synth_zrt.set(chans[f], data )
        self.flip('synth')

        #if self.zcor > 0.0:
        #    T = pylab.delete(T, self.zcor, 0)
        #    R = pylab.delete(R, self.zcor, 0)
        #    Z = pylab.delete(Z, self.zcor, 0)
        #elif self.zcor < 0.0:
        #    T = pylab.insert(T, 0, pylab.ones(self.zcor) * T[0], 0)
        #    R = pylab.insert(R, 0, pylab.ones(self.zcor) * T[0], 0)
        #    Z = pylab.insert(Z, 0, pylab.ones(self.zcor) * T[0], 0)


    #def synth_zrt(self, results):

    #    self.zcor = float(results['zcor'][self.sta])
    #    az = pylab.array( float(self.azimuth) )
    #    self.synth_zrt = Records()

    #    TSS = pylab.array( self.synth.get('TSS') )
    #    TDS = pylab.array( self.synth.get('TDS') )
    #    XSS = pylab.array( self.synth.get('XSS') )
    #    XDS = pylab.array( self.synth.get('XDS') )
    #    XDD = pylab.array( self.synth.get('XDD') )
    #    ZSS = -1 * pylab.array( self.synth.get('ZSS') )
    #    ZDS = -1 * pylab.array( self.synth.get('ZDS') )
    #    ZDD = -1 * pylab.array( self.synth.get('ZDD') )

    #    XX = pylab.array(float(results['Mxx']))
    #    YY = pylab.array(float(results['Myy']))
    #    ZZ = pylab.array(float(results['Mzz']))
    #    XY = pylab.array(float(results['Mxy']))
    #    XZ = pylab.array(float(results['Mxz']))
    #    YZ = pylab.array(float(results['Myz']))


    #    Z = XX*( (ZSS/2 - pylab.cos(2*az)) * ZDD/3 )
    #    Z += YY*( -1 * (ZSS/2 * pylab.cos(2*az)) - ZDD/6 )
    #    Z += ZZ*( ZDD/3 )
    #    Z += XY* ZSS * pylab.sin(2*az)
    #    Z += XZ* ZDS * pylab.cos(az)
    #    Z += YZ* ZDS * pylab.sin(az)
    #    self.synth_zrt.set('Z',Z)

    #    R = XX*( (XSS/2 * pylab.cos(2*az)) - XDD/6 )
    #    R += YY*( -1 * (XSS/2 * pylab.cos(2*az)) - XDD/6 )
    #    R += ZZ*( XDD/3 )
    #    R += XY* XSS * pylab.sin(2*az)
    #    R += XZ* XDS * pylab.cos(az)
    #    R += YZ* XDS * pylab.sin(az)
    #    self.synth_zrt.set('R',R)

    #    T = XX * TSS / 2 * pylab.sin(2*az)
    #    T -= YY / 2 * TSS * pylab.sin(2*az)
    #    T -= XY * TSS * pylab.cos(2*az)
    #    T -= XZ* TDS * pylab.sin(az)
    #    T += YZ* TDS * pylab.cos(az)
    #    self.synth_zrt.set('T',T)

    #    if self.zcor > 0.0:
    #        T = pylab.delete(T, self.zcor, 0)
    #        R = pylab.delete(R, self.zcor, 0)
    #        Z = pylab.delete(Z, self.zcor, 0)
    #    elif self.zcor < 0.0:
    #        T = pylab.insert(T, 0, pylab.ones(self.zcor) * T[0], 0)
    #        R = pylab.insert(R, 0, pylab.ones(self.zcor) * T[0], 0)
    #        Z = pylab.insert(Z, 0, pylab.ones(self.zcor) * T[0], 0)


    def plot(self,trace='real'):
        # open a pylab plot of the data
        debug('Plot traces for %s' % self.sta )

        if trace is 'real': records = self.real
        else: records = self.synth

        if not records:
            error('Empty Records for %s data on %s' % (trace,self.sta) )

        total = 0
        pyplot.figure()
        channels = records.list()
        for chan, data in records:
            notify( 'add %s_%s to plot' % (self.sta,chan) )
            pyplot.subplot(len(channels),1,total)
            pyplot.plot(data)
            pyplot.legend(["%s_%s" % (self.sta,chan)])
            total += 1

        pyplot.suptitle("Real Data: Trace %s" % self.sta)
        pyplot.show()

    def filter(self,trace='real',hpass=0.02,lpass=0.1):

        global tmp_folder

        if trace == 'real':
            convert = self.real
            infile = self.to_file('real')
            points = self.real_samples
            period = 1/self.real_samplerate
        else:
            convert = self.synth
            infile = self.to_file('synth')
            points = self.synth_samples
            period = 1/self.synth_samplerate

        outfile = '%s/TEMP_FILTER_DATA' % tmp_folder

        script = '%s/run_filter' % tmp_folder
        script = self.new_script( script, self.filter_script(trace,hpass,lpass,points,period) )

        cmd = '%s %s %s' % (script,infile,outfile)
        for line in run(cmd):
            log(line)

        record = readHelm( outfile )

        os.remove( outfile )
        os.remove( infile )

        if trace == 'real':
            self.real_data(record)
            self.real = record
            self.real_file = ''
            self.real_samples = record.samplecount()
            self.real_samplerate  = record.samplerate
        else:
            self.synth_data(record)
            self.synth_file = ''
            self.synth_samples = record.samplecount()
            self.synth_samplerate  = record.samplerate


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
        log("Find executable for (%s)" % ex)

        newex = spawn.find_executable(ex)

        if not newex:
            error("Cannot locate executable for [%s] in $PATH = \n%s" % \
                    (ex,os.environ["PATH"].split(os.pathsep)) )

        executables[ex] = os.path.abspath( newex )

        log("(%s) => [%s]" % (ex, newex) )

#def safe_db_get_value(db,field,default):
#    '''
#    Safe method to extract values from db
#    with a default value option.
#    '''
#    log( "getv(%s)" % field )
#    try:
#        value = db.getv(field)[0]
#    except Exception,e:
#        log("[%s]" % db.query(datascope.dbNAME) )
#        log("problem during getv(%s) => [%s]" % (field,e) )
#        return default
#
#    return value
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
            warning('Problems safe_pf_get(%s,%s)' % (field,e))
            pass

    log( "pf.get(%s,%s) => %s" % (field,defaultval,value) )

    return value


def clean_trace(data, samplerate, trace_start, trace_end, need_start=False, need_end=False):
    '''
    Apply time stamps to our values and cut extra data.
    Return clean list of touples.
    '''


    new_data = []
    if not need_start: need_start = trace_start
    if not need_end: need_end = trace_end

    debug( "clean_trace points:[%s]" % (len(data)) )
    debug( "actual[%s,%s]" % (trace_start, trace_end) )
    debug( "needed[%s,%s]" % (need_start, need_end) )

    if len(data):

        sps = int( len(data) / (trace_end - trace_start) )
        period = 1.0/sps

        debug( "calculated sps:[%s] period:[%s]" % (sps,period) )

        if samplerate == sps:
            for d in data:
                if trace_start >= need_start and trace_start <= need_end:
                    new_data.append(d)
                trace_start += period
            debug('New trace is: %s' % len(new_data) )
        else:
            warning('problem on calculated samplerate %s != %s' % (samplerate,sps) )

    else:
        warning( 'No data on this trace object' )

    return new_data

def verify_trace_time(start, end, time, endtime):
    '''
    Verify that we have the requested data
    '''
    debug( "Requested time: [%s,%s]" % (stock.strtime(start),stock.strtime(end)) )
    debug( "In trace object: [%s,%s]" % (stock.strtime(time),stock.strtime(endtime)) )

    tw = end - start

    if endtime - time < tw:
        warning('Got %s secs but expected [%s].' % ( (endtime-time), tw) )
        return False

    if start < time:
        warning('Trace starts [%s] seconds late.' % ( time - start ) )
        return False

    if endtime < end:
        warning('Trace ends [%s] seconds early.' % ( end - endtime ) )
        return False

    return True

def dynamic_loader(module):
    '''
    Load some libs defined on the pf file.
    '''
    log( "load moment_tensor.%s" % module )
    try:
        return __import__("moment_tensor.%s" % module, globals(), locals(), [module], -1)
    except Exception,e:
        error("Import Error: [%s] => [%s]" % (module,e) )

def cleanup(folder):
    """
    There are serveral files that we produce and keep
    in the tmp directory. Some are known but others are
    random names.
    """

    try:
        if not os.path.isdir(folder): os.makedirs(folder)
    except Exception,e:
        error("Problems while creating folder [%s] %s" % (folder,e))

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
            log( 'unlink( %s )' % temp_file )
            map( os.unlink, glob.glob( temp_file ) )
        except Exception, e:
            error('Cannot remove temp file %s [%s]' % (temp_file,e) )

def new_Helm_header(samples,samplerate):
    temp = '     %0.4e     %0.4e      0  0  0.00\n' % (0,0)
    temp += '%8d   %0.5f  %.4e\n' % (samples,1/samplerate,0)
    return temp

def fix_format_data(point, decimals, total):
    new = "%0.*e" % (decimals,point)
    return new.rjust(total)

def mkHelm(traces, append='', outputfile='', perline=7, total=14, decimal=5):
    """
    Routine to write Helmberger Format Seismograms
    """
    log('mkHelm')

    filename = {}
    global tmp_folder

    if not outputfile:
        (f, outputfile) = mkstemp(suffix='.Helm_data',
                prefix='dbmoment_%s_' % append,dir=tmp_folder,text=True)
        f = os.fdopen(f, 'w')
    else:
        try:
            f = open(outputfile, 'w')
        except Exception,e:
            self.error('Cannot open new file %s %s'% (outputfile,e))

    debug('New data file %s' % outputfile )


    if len(traces.list()) == 3:
        global seismic_channels
        channels = seismic_channels
    else:
        global synth_channels
        channels = synth_channels

    # File header
    text = ''
    text += "%8d\n" % ( len(traces.list()) )
    text += "(%de%d.%d)\n" % (perline, total, decimal)

    debug('coded channels %s' % channels )

    debug('in object %s' % traces.list() )

    for chan in channels:
        debug('appending %s to %s' % (chan, outputfile) )

        data = traces.get(chan)
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
        error('Cannot write to new file %s %s'% (outputfile,e))

    return outputfile


def readHelm(inputfile):
    """
    Routine to read Helmberger Format Seismograms
    Returning full object:
        traces[chan]['data'] = data
        return traces

    """
    log('readHelm')
    results = {}


    fo = open(inputfile, "r")

    # Number of channels
    try:
        total_chans = int( fo.readline().strip() )
        log( "Total channels [%s]" % total_chans )
    except:
        error("NOT VALID FILE [%s]" % inputfile)

    if total_chans == 1:
        channels = 'X'
    if total_chans == 3:
        global seismic_channels
        channels = seismic_channels
    else:
        global synth_channels
        channels = synth_channels

    # Data Format
    try:
        data_format = fo.readline().strip()
    except:
        error( "NOT VALID FILE [%s]" % inputfile )

    for chan in range(total_chans):
        channame = channels[chan]
        log( "chan %s" % channame )
        try:
            header1 = fo.readline().strip().split()
            header2 = fo.readline().strip().split()
            total_points = int( header2[0] )
            samplerate =  1/float( header2[1] )
        except:
            error( "NOT VALID FILE [%s]" % inputfile )

        if not total_points or not samplerate:
            error( "NOT VALID FILE [%s]" % inputfile )

        cache = []
        log( "Total points [%s]" % total_points )
        log( "Sampelrate [%s]" % samplerate )

        while (total_points > 0):
            row = fo.readline().strip('\n')
            if not row: error( 'Problems readHelm(%s)' % inputfile )
            row = [float(row[i:i+12]) for i in range(0, len(row), 12)]
            cache.extend( row )
            total_points -= len( row )

        cache = pylab.array( cache )
        if total_chans == 1: return cache

        if not results:
            # need new object
            results =  Records(samplerate)
            results.file = inputfile

        results.trace( channame, cache )

    return results


if __name__ == "__main__":
    """ If we call this script directly, then output error  """

    print "\n\t** Not to run directly!!!! **\n"
    print "\n\nNo BRTT support."
    print "Juan Reyes <reyes@ucsd.edu>\n\n"
