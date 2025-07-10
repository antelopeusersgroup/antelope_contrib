#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.



from __main__ import *      # Get all the libraries from parent

class Synthetics():
    """
    The pf_file is the parameter file that contains the model that we want to use.
    Generate GFs for depth of 8km and distance of 1km.
        GF.generate_python(depth=8,distance=10)

    Plot the GFs for the first 150 samples.
        GF.plot(0,150)

    """
    def __init__( self, dbfolder, tmp_folder='/tmp/dbmoment' ):

        self.dbfolder = dbfolder
        self.tmp_folder = tmp_folder

    def set_model( self, model  ):
        # Read configuration from parameter file self.model
        self.model = model
        self._read_model()

        self.database = "%s/synthetics-%s" % (self.dbfolder,self.model_short_name)
        self.dbdir = os.path.dirname( self.database )
        elog.debug( 'Init Synthetics Class: model=%s db=%s' % \
                (self.model['name'],self.database) )

        # Return a relative  path to save the files
        self.archive = self.dbdir + '/files/'
        elog.debug( 'Archive for GF: %s' % self.archive )

        # Recursive directory creation function
        try:
            if not os.path.exists(self.archive): os.makedirs(self.archive)
        except Exception as e:
            elog.error('Cannot create directory (%s) %s => %s' % (self.archive,Exception,e))

        if not os.path.exists(self.archive):
            elog.error( 'Missing GF archive: %s' % self.archive )

        # Get db ready
        try:
            self.db = datascope.dbopen( self.database, "r+" )
            self.db = self.db.lookup(table='wfdisc')
        except Exception as e:
            elog.error('Problems opening wfdisc: %s %s' % (self.database,e) )

    def get_synth(self, depth, distance, tw=-1, response='', filters=None, debug_plot=False):
        """
        Main function to retrieve ( and produce if missing ) the requested elements.

        tw: return only this amount of seconds
        filter: Apply this filter to the data

        """

        self.depth = int(depth)
        self.distance = "{:03d}".format(distance)
        self.tw = tw
        self.filters = filters
        self.response = response

        self.debug_plot = debug_plot

        if not distance: elog.error('Problems with distance. %s' % distance )

        elog.debug("get_synth(%s,%s,tw=%s)" % \
                (self.depth,self.distance,self.tw))

        self._read_model()

        if distance < self.d_min:
            elog.warning("distance to event [%s] under model's limit [%s]" % \
                    (self.distance,self.d_min))
            return None

        if distance > self.d_max:
            elog.warning("distance to event [%s] over model's limit [%s]" % \
                    (self.distance,self.d_max))
            return None

        # LAZY EVALUATION HERE....

        elog.debug("Try to get from database.")
        # get from database
        synthetics = self._get_from_db()

        if not synthetics:
            # Clean database from previous values for this depth
            steps = ['dbsubset sta =~ /%s/' % depth ]

            with datascope.freeing(self.db.process( steps )) as dbview:
                if dbview.record_count:
                    #elog.debug( 'Clean %s records after subset.' % dbview.record_count )
                    #dbview.record = 0
                    #for trace in dbview.iter_record():
                    #    elog.debug( 'trace.delete()')
                    #    try:
                    #        os.remove( trace.extfile()[1] )
                    #    except Exception,e:
                    #        elog.error('Cannot remove %s [%s]' % \
                    #                (trace.extfile()[1], e) )
                    #    trace.delete()
                    elog.warning('********************' )
                    elog.warning('Synthetics for this depth already in [%s]' % self.database )
                    elog.warning('Missing the traces for depth:[%s] dist:[%s]' % (self.depth,self.distance) )
                    elog.warning('Maybe some parameters in MODEL [%s] are new.' % self.model_short_name )
                    elog.warning('Clean [%s] archive and restart process may help.' % self.database )
                    elog.warning('********************' )
                    return False

            elog.debug('Missing synthetics for depth:[%s] dist:[%s]' % (self.depth,self.distance) )
            elog.debug("Generate new.")
            self._generate()

            # Now try to get from database again
            synthetics = self._get_from_db()

        return synthetics

    def _generate(self):
        """
        Configure model based on parameter file and run shell command.
        """

        elog.notify("generate synthetics for depth:%s" % self.depth )

        try:
            os.remove('%s/TEMP_MODEL') % self.tmp_folder
            os.remove('%s/GREEN.1') % self.tmp_folder
            os.remove('%s/junk') % self.tmp_folder
            os.remove('%s/vec') % self.tmp_folder
        except:
            pass

        elog.debug("FKRPROG.PY: generate()  -  Fix model")

        d = 0.0
        padding = 2.5
        for y in range(len(self.D)):

            now = d
            missing = self.depth - now

            d += float(self.D[y])

            elog.debug('Test if depth: %f is above model layer: %f' % (self.depth, d))
            if d > self.depth:
                # Add new layer
                elog.debug('insert at: %s' % y)
                self.D.insert( y, missing )
                self.A.insert( y, self.A[y] )
                self.B.insert( y, self.B[y] )
                self.RHO.insert( y, self.RHO[y] )
                self.QA.insert( y, self.QA[y] )
                self.QB.insert( y, self.QB[y] )

                # layer bellow event
                self.LMAX = y + 2

                break

        if not self.LMAX:
            # Add one at the end if event is too deep
            self.D.append( missing + padding )
            self.A.append( self.A[-1] )
            self.B.append( self.B[-1] )
            self.RHO.append( self.RHO[-1] )
            self.QA.append( self.QA[-1] )
            self.QB.append( self.QB[-1] )
            self.LMAX = len(self.D)



        self.MMAX = len(self.D)

        model = ''


        for I in range(len(self.D)):
            # FOR MODEL:  FORMAT(4E11.4,2F10.2)$
            model += "{:11.4E}".format(self.D[I])
            model += "{:11.4E}".format(self.A[I])
            model += "{:11.4E}".format(self.B[I])
            model += "{:11.4E}".format(self.RHO[I])
            model += "{:10.2f}".format(600)
            model += "{:10.2f}".format(300)
            model += "\n"

        model = self._file_format(self.MMAX,model,self.LMAX)
        elog.debug( "MODEL:" )
        elog.debug( model )

        # Open temp file to put model.
        try:
            elog.debug( "Write model to %s/TEMP_MODEL" % self.tmp_folder ) 
            f = open('%s/TEMP_MODEL' % self.tmp_folder, 'w')
            f.write(model)
            f.close()
        except Exception as e:
            elog.error('Cannot open temp file TEMP_MODEL %s %s'% (Exception,e))


        elog.debug("generate()  -  Running: fortran_fkrprog < %s/TEMP_MODEL" % self.tmp_folder)
        cmd = 'fortran_fkrprog < TEMP_MODEL'
        run(fix_exec(cmd),self.tmp_folder)

        if not os.path.isfile('%s/GREEN.1' % self.tmp_folder):
            elog.error('Problem during "fortran_fkrprog < %s/TEMP_MODEL" command' % self.tmp_folder)

        script = 'run_fkrsort'
        script = self._new_script( script, self.tmp_folder, self._new_fkrsort_script(self.DT) )

        cmd = 'csh -c "run_fkrsort temp_data %s %s %s"' % (self.distances[0], self.depth,len(self.distances))
        run(cmd,self.tmp_folder)

        for dist in self.distances:
            newfile = 'temp_data%sd%s.raw_synthetics' % (dist,self.depth)
            elog.debug( 'READ file for SYNTH: %s' % newfile )
            record = readHelm( "%s/%s" % ( self.tmp_folder,newfile ) )

            self._save_to_db(record,dist,self.depth)

        return


    def _new_script(self, filename, folder, content):

        filename = "%s/%s" % (folder, filename)

        try:
            os.remove(filename)
        except:
            pass

        elog.debug('new_script(%s)' % filename)
        outfile = open(filename, 'w')

        global executables
        if len(executables):
            for line in content.split("\n"):
                for src, target in executables.items():
                    line = re.sub(r"^%s " % src, "%s " % target, line)
                outfile.write(line+"\n")
        else:
            elog.debug('content = %s' % content)
            outfile.write(content)

        outfile.close()

        st = os.stat(filename)
        os.chmod(filename, 0x774)

        return filename


    def _get_from_db(self):
        """
        Open the database and extract the traces for
        requested depth and distance.

        The functions are archived in ascii files referenced by Datascope using a simple
        wfdisc table. The value for the station is our DEPTH to the event. The value for the channel
        is our DISTANCE to the station and the seismic element is specified in the location code.
        i.e.
            depth: 8
            distance: 10
            element: TDS
            => 8_10_TDS ( format: sta_chan_loc )


        All data will be extracted from the database and archived in memory internal to the class. We
        will use the dictionary self.DATA for this. Each key in the dictionary will be dedicated
        to a different component of the fundamental Green's Functions and will include objects for metadata.
        """

        results = {}

        elog.debug("get_from_db()")

        steps = ['dbsubset sta =~ /%s/ && chan =~ /%s_.*/' % (self.depth,self.distance) ]

        elog.debug( steps )

        with datascope.freeing(self.db.process( steps )) as dbview:
            # Get list of elements and get values for time, endtime, nsamp, samprate and LOC_CODE.
            elog.debug( 'Found %s records after subset.' % dbview.record_count )
            if not dbview.record_count: return False

            if dbview.record_count < 10:
                elog.warning( 'OLD FORMAT FOR SYNTHETICS. NEED 10 ELEMENTS FOR *NEW* ISO_INV.' )
                elog.error( 'Remove old synthetics database and restart process.' )

            dbview.record = 0

            samprate = dbview.getv('samprate')[0]
            time = dbview.getv('time')[0]
            #endtime = dbview.getv('endtime')[0]
            endtime = time + ( self.tw * 2 )

            try:
                elog.debug('trloadchan(%s,%s)'% (time,endtime))
                tr = dbview.trload_cssgrp(time,endtime)
                tr.trsplice()
            except Exception as e:
                elog.error('Could not read synthetics for %s:%s [%s]' % (self.depth,self.distance, e))
                return False


            # Need to track original
            original = {}
            for t in tr.iter_record():
                original[ t.getv('chan')[0] ] = t.trdata()

            #
            # Loop over every possible filter
            #
            for f in self.filters:

                # Return this version to the original data
                for t in tr.iter_record():
                    t.trputdata( original[ t.getv('chan')[0] ] )

                if self.debug_plot:
                    fig = plot_tr_object( tr, 'raw', style='r')

                #tr = apply_response( tr, self.response, samprate)
                apply_response( tr, self.response, samprate)

                if self.debug_plot:
                    plot_tr_object( tr, 'add-response', style='b', fig=fig)

                # Demean the trace
                tr.trfilter('BW 0 0 2 4')
                tr.trfilter('DEMEAN')

                tr.trapply_calib()


                elog.debug('Filter synth with [%s]' % f)
                tr.trfilter( f )

                if self.debug_plot:
                    plot_tr_object( tr, f, style='y', fig=fig)

                results[ f ] = Records(1)

                this = 1
                for record in tr.iter_record():
                    (depth,chan,nsamp,time,endtime) = \
                            record.getv('sta','chan','nsamp','time','endtime')

                    elog.debug('getv()=> (%s,%s,%s,%s,%s)' % \
                            (depth,chan,nsamp,time,endtime))

                    # Extract the element name from the channel text (distance)
                    try:
                        m = re.match(".*_(\\w{3})",chan)
                        element = m.group(1)
                    except Exception as e:
                        elog.error('Problems in regex [.*_(\\w{3})] on [%s] %s: %s' % (chan,Exception,e))

                    if not element:
                        elog.error('Cannot find component name in wfdisc entry: %s_%s' % (self.depth,self.distance))


                    # SIMPLE decimation method.
                    # We are way above the min freq for this to be a problem.
                    data = []
                    elog.debug( 'Extract data' )
                    temp_data = record.trdata()
                    elog.debug( 'Simple decimation' )
                    for x in range(0,len(temp_data),int(samprate)):
                        data.append( temp_data[x] )
                    data = data[:self.tw]


                    # Verify if we have NULL values in array
                    data = [ x for x in data if x < 1.e20 ]


                    elog.debug( '%s: %s samples' % (m.group(1),len(data)) )

                    if not data:
                        elog.error('Cannot find data for component [%s] in %s_%s' % \
                                (m.group(1),self.depth,self.distance))

                    # Placing inside Records object
                    results[ f ].trace( element, data)
                    #results[ f ].set_samplerate(newsamprate)

                    if self.debug_plot:
                        add_trace_to_plot( results[ f ].get( element ), '.k', 'final-%s' % element,
                                tr.record_count, this, jump=samprate )
                        this += 1


            tr.trfree()

        if self.debug_plot:
            pyplot.show()


        return results


    def _save_to_db(self,record,distance,depth):
        """ Open the database and save the new traces.

        The functions are archived in ascii files referenced by Datascope using a simple
        wfdisc table. The value for the station is our DEPTH to the event. The value for the channel
        is our DISTANCE to the station  and the element is specified in the location code.
        i.e.
            depth: 8
            distance: 10
            element: TDS
            => 8_010_TDS ( format: sta_chan_loc )

        """

        distance = "{:03d}".format(distance)

        # Save all data to file
        dfile = "%s_%s_%s.gf" % (depth,distance,self.model_short_name)
        elog.debug( 'depth %s' % depth )
        elog.debug( 'distance  %s' % distance  )
        elog.debug( 'model %s' % self.model_short_name )
        elog.debug( 'dfile %s' % dfile )



        try:
            f = open("%s/%s"%(self.archive,dfile), 'w')
        except Exception as e:
            elog.error('Cannot open file %s %s %s'% (dfile,Exception,e))

        for element,data in record:

            elog.debug( 'add element %s to database' % element )

            sta = '%s' % depth
            chan_loc = '%s_%s' % (distance,element)
            elog.debug( 'name %s_%s' % (sta,chan_loc) )

            samprate = record.samplerate
            nsamp = record.samplecount(element)
            elog.debug( 'samplerate %s' % samprate )
            elog.debug( 'nsamp %s' % nsamp )

            time = 1.0
            endtime = (nsamp*samprate)+time

            # Add small header
            f.write('%s\t%s\t%s\n'%(element,nsamp,samprate))
            start = f.tell()

            [f.write('%s\n' % x )for x in data ]

            try:
                wfid = self.db.nextid('wfid')
                keys = ('sta','chan','time','endtime','nsamp','samprate',
                        'calib','datatype','dir','dfile','foff','wfid')
                values = (sta,chan_loc,time,endtime,nsamp,samprate,1.0,
                        'as','files/',dfile,start,wfid)
                keyvals = list(zip(keys,values))

                self.db.addv(*keyvals)
            except Exception as e:
                elog.error('Cannot add new line [%s] %s %s'% (element,Exception,e))

        try:
            f.close()
        except Exception as e:
            elog.error('Cannot close file %s %s %s'% (file,Exception,e))


        return record



    def _file_format(self,total,model,under_event):
        """
        Template for the earth model.
        """
        elog.debug("file_format()")

        text =  ".T.\n     0   64\n"
        text += "GREEN.1\n"

        # FORMAT(2F10.4,3I5,F10.4,I5,I5)
        text += "{:10.1f}".format(self.DECAY)
        text += "{:10.2f}".format(self.depth)
        text += "{:5d}".format(self.N1)
        text += "{:5d}".format(self.N2)
        text += "{:5d}".format(self.N)
        text += "{:10.3f}".format(1/self.DT)
        text += "{:5d}".format(total)
        text += "{:5d}\n".format(1)

        for x in self.ISRC:  text += "{:5d}".format(x)
        text += "{:5d}\n".format(self.JBDRY)

        text += model

        # For LMAX:  FORMAT(I5,4F10.4)
        text += "{:5d}\n".format(under_event)

        text += "  0.4000000E+03  1.500000E+00         0\n"
        text += "{:5d}".format( len(self.distances) )
        text += "{:10.1f}".format(self.CMAX)
        text += "{:10.1f}".format(self.C1)
        text += "{:10.1f}".format(self.C2)
        text += "{:10.1f}\n".format(self.CMIN)

        for d in self.distances:
            # LAST LINE: FORMAT(3F10.4)
            text += "{:10.2f}".format(d)
            text += "{:10.1f}".format(0.0)
            text += "{:10.1f}\n".format(self.VEL_RED)

        return text



    def _read_model(self):
        """
        Read parameters from configuration file.
        """
        elog.debug("read_model(%s)" % self.model)

        self.model_short_name = self.model[ 'name' ]

        self.name = self.model['name']

        try:
            self.DT = float(self.model['samplerate'])
        except Exception as e:
            elog.error('Wrong Format of samplerate PF file[%s]. %s %s'% (self.model,Exception,e))

        try:
            self.DECAY = float(self.model['decay'])
        except Exception as e:
            elog.error('Wrong Format of decay PF file[%s]. %s %s'% (self.model,Exception,e))

        try:
            self.N1 = int( self.model['start_frequency'] )
            self.N2 = int( self.model['end_frequency'] )

            #self.N  = (self.N2-self.N1+1)*4 # 2 times the total number of freqs
            self.N  = (self.N2)*4 # 4 times the total number of freqs
        except Exception as e:
            elog.error('Wrong Format of PF file[%s]. %s %s'% (self.model,Exception,e))

        elog.debug("read_model()  -  DECAY=%s N1=%s N2=%s N=%s DT=%s " % (self.DECAY,self.N1,self.N2,self.N,self.DT))


        # ISRC, JBDRY
        self.ISRC = [ 1 for x in range(10) ]
        self.JBDRY = 0
        elog.debug("read_model()  -  ISRC=%s JBDRY=%s " % (self.ISRC,self.JBDRY))


        # Init vars for model layers
        self.D   = list()
        self.A   = list()
        self.B   = list()
        self.RHO = list()
        self.QA  = list()
        self.QB  = list()
        self.LMAX = 0
        self.MMAX = 0

        try:
            temp  = self.model['model'].splitlines()

            for x in temp:
                elog.debug(x)

            for x in range(len(temp)):
                t = temp[x].split()
                elog.debug('temp:[%s]' % t)
                if not t: continue
                self.D.append( float(t[0]) )
                self.A.append( float(t[1]) )
                self.B.append( float(t[2]) )
                self.RHO.append( float(t[3]) )
                self.QA.append( 1/float(t[4]) )
                self.QB.append( 1/float(t[5]) )
                elog.debug('D:%s A:%s B:%s RHO:%s QA:%s QB:%s' % (self.D[x],self.A[x],self.B[x],self.RHO[x],self.QA[x],self.QB[x]))

        except Exception as e:
            elog.error('Wrong Format of input file[%s]. %s(%s) RAW: %s'% (self.model,Exception,e,temp))



        self.CMAX = float(self.model['cmax'])
        self.C1   = float(self.model['c1'])
        self.C2   = float(self.model['c2'])
        self.CMIN = float(self.model['cmin'])
        self.VEL_RED = float(self.model['velocity_reduction'])
        elog.debug("read_model()  -  CMAX=%s C1=%s C2=%s CMIN=%s " % (self.ISRC,self.C1,self.C2,self.CMIN))
        elog.debug("read_model()  -  VELOCITY_REDUCTION=%s " % self.VEL_RED)



        self.d_max = int( self.model['distance_max'] )
        self.d_min = int( self.model['distance_min'] )
        self.d_step = int( self.model['distance_step'] )

        if not self.d_min: self.d_min = self.d_step

        self.distances = list(range(self.d_min, self.d_max + self.d_step, self.d_step))

        return



    def plot(self,start=0,end=-1):
        """ Plot all traces in memory. They are containe in
        the dictionary DATA in self.

        """
        elog.debug("plot()")

        total = len(self.DATA)
        half = int(total/2)
        now = 0

        if half != total/2: half += 1

        for trace in self.DATA:

            try:
                now += 1
                pyplot.subplot(5,2,now)
                data = self.DATA[trace]['data']
                pyplot.plot(data[start:end])
                pyplot.legend([trace])
            except Exception as e:
                elog.error('Problem plotting green functions.[%s => %s]' % (Exception,e) )

        pyplot.suptitle("Green Functions: depth:%s distance:%s" % (self.DEPTH,self.DISTANCE))
        pyplot.show()



    def _new_fkrsort_script(self,samplerate):
        elog.debug('New fkrsort script samplerate=[%s]' % samplerate)

        text = "#! /bin/csh -f\n"
        text += "set dt={:0.4f}\n".format(1/samplerate)
        text += "set npts={0}\n".format( int(512*samplerate) )
        text += '''
set name=$1
set dist=$2
set depth=$3
set loopend=$4
set count=0
set j=1
set vshift=0
set i=0
set nvec=0
set n=0
rehash

##First inverse fft complex spectra to produce displacement time series
wvint9 << eof
d
eof

rm -f tmp*
@ nvec=($loopend - $count) * 10
while ($count < $loopend)
rm -f junk
@ vshift=$i + 7
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp1$$
@ vshift=$i + 4
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp2$$
@ vshift=$i + 6
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp3$$
@ vshift=$i + 3
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp4$$
@ vshift=$i + 1
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp5$$
@ vshift=$i + 5
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp6$$
@ vshift=$i + 2
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp7$$
@ vshift=$i
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp8$$
@ vshift=$i + 8
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp9$$
@ vshift=$i + 9
window v0=$vshift e0=0 nt=$npts nx=$nvec nv=1 < vec > tmp10$$
cat tmp1$$ tmp2$$ tmp3$$ tmp4$$ tmp5$$ tmp6$$ tmp7$$ tmp8$$ tmp9$$ tmp10$$ > junk
echo $j
mkHelm format="(6e12.5)" ntr=10 dt=$dt nt=$npts < junk > {$name}{$dist}d{$depth}.raw_synthetics
rm -f tmp*$$
@ i += 10
@ count++
@ j++
'''
        text += '@ dist += %d\nend\n' % self.d_step

        return text



if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
