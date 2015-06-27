from __main__ import *      # Get all the libraries from parent

class Greens():
    """
    The pf_file is the parameter file that contains the model that we want to use.
    Generate GFs for depth of 8km and distance of 1km.
        GF.generate_python(depth=8,distance=10)

    Plot the GFs for the first 150 samples.
        GF.plot(0,150)

    """
    def __init__( self, dbname, model, tmp_folder='/tmp/dbmoment' ):
        self.tmp_folder = tmp_folder
        self.modelname = model
        self.database = dbname
        self.dbdir = os.path.dirname( self.database )
        log( 'Init Greens Class: model=%s db=%s' % \
                (self.modelname,self.database) )

        self.model = stock.pfread(model)

        # Read configuration from parameter file self.model
        self._read_model()

        # Return a normalized absolutized version of the path to save the files
        self.archive = self.dbdir + '/gf_files/'
        #self.archive = os.path.relpath(self.archive, self.dbdir )
        log( 'Archive for GF: %s' % self.archive )

        # Recursive directory creation function
        try:
            if not os.path.exists(self.archive): os.makedirs(self.archive)
        except Exception,e:
            error('Cannot create direcotry (%s) %s => %s' % (self.archive,Exception,e))

        if not os.path.exists(self.archive):
            error( 'Missing GF archive: %s' % self.archive )

        # Get db ready
        try:
            self.db = datascope.dbopen( self.database, "r+" )
            self.db = self.db.lookup(table='wfdisc')
        except Exception,e:
            error('Problems opening wfdisc: %s %s' % (self.database,e) )

    def get_synth(self,depth,distance,startpoint=0,totalpoints=False,sps=False,filter=None,segtype=None):
        """
        Main function to retrieve ( and produce if missing ) the requested elements.

        startpoint: start the array at this data point
        totalpoints: return only this amount of points
        sps: The requested samplerate for the data
        filter: Apply this filter to the data

        """

        #new_distance = int(distance)
        #depth = int(depth)

        if not distance: error('Problems with distance. %s' % distance )

        #distance = new_distance

        log("get_synth(%s,%s,sps=%s,startpoint=%s,totalpoints=%s,filter=%s)" % \
                (depth,distance,sps,startpoint,totalpoints,filter))

        log("Try to get from database.")
        greens = self._get_from_db(depth,distance,sps=sps,startpoint=startpoint,
                totalpoints=totalpoints,filter=filter,segtype=segtype)

        if not greens:
            debug("Not in db. Generate new.")
            greens = self._generate(depth,distance,segtype='D')
            #greens = self._generate(depth,distance,segtype='V')

            greens = self._get_from_db(depth,distance,sps=sps,startpoint=startpoint,
                    totalpoints=totalpoints,filter=filter,segtype=segtype)

        #notify( 'File for SYNTH: %s' % greens.file )

        return greens

    def _generate(self,depth,distance,segtype):
        """
        Configure model based on parameter file and run shell command.
        """
        intdepth = depth
        depth = float(depth)

        self._read_model()

        log("generate(%s,%s,)" % (depth,distance))

        try:
            os.remove('%s/TEMP_MODEL') % self.tmp_folder
            os.remove('%s/GREEN.1') % self.tmp_folder
            os.remove('%s/junk') % self.tmp_folder
            os.remove('%s/vec') % self.tmp_folder
        except:
            pass

        debug("FKRPROG.PY: generate()  -  Fix model")
        d = 0.0
        for y in range(len(self.D)):

            now = d
            missing = depth - now

            d += float(self.D[y])

            log('Test if depth: %f is above model layer: %f' % (depth, d))
            if d > depth:
                #if not missing: missing = 0.1

                if y == 0:
                    prev = 0
                else:
                    prev = y-1
                # Add new leyer
                log('insert at: %s' % y)
                self.D.insert( y, 8)
                self.A.insert( y, self.A[prev] )
                self.B.insert( y, self.B[prev] )
                self.RHO.insert( y, self.RHO[prev] )
                self.QA.insert( y, self.QA[prev] )
                self.QB.insert( y, self.QB[prev] )

                # layer bellow event
                self.LMAX = y + 1

                break

        if not self.LMAX:
            # Add one at the end if event is too deep
            self.D.append( missing  )
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
            #model += " %1.4E %1.4E %1.4E %1.4E   600.00    300.00\n" % \
            #        (self.D[I],self.A[I],self.B[I],self.RHO[I])

        #template = self._file_format()
        #model =  template % (6.0,depth,(1/self.DT),self.MMAX,model,self.LMAX,distance)
        model = self._file_format(6.0,depth,(1/self.DT),self.MMAX,model,self.LMAX,distance)
        log( "MODEL:" )
        log( model )

        # Open temp file to put model.
        try:
            debug( "Write model to %s/TEMP_MODEL" % self.tmp_folder ) 
            f = open('%s/TEMP_MODEL' % self.tmp_folder, 'w')
            f.write(model)
            f.close()
        except Exception,e:
            error('Cannot open temp file TEMP_MODEL %s %s'% (Exception,e))


        log ("generate()  -  Running: fortran_fkrprog < %s/TEMP_MODEL" % self.tmp_folder)
        cmd = 'fortran_fkrprog < %s/TEMP_MODEL' % self.tmp_folder
        run(cmd, self.tmp_folder)

        if not os.path.isfile('%s/GREEN.1' % self.tmp_folder):
            error('Problem during "fortran_fkrprog < %s/TEMP_MODEL" command' % self.tmp_folder)

        script = '%s/run_fkrsort' % self.tmp_folder
        script = self._new_script( script, self._fkrsort_script(segtype) )

        newfile = '%s/temp_data%sd%s.raw_greens' % (self.tmp_folder,distance,intdepth)

        cmd = '%s %s/temp_data %s %s 1' % (script,self.tmp_folder,distance,intdepth)
        run(cmd, self.tmp_folder)

        log( 'READ file for SYNTH: %s' % newfile )
        record = readHelm( newfile )

        return self._save_to_db(record,distance,intdepth,segtype)


    def _new_script(self,filename,content):

        try:
            os.remove(filename)
        except:
            pass

        log('new_script(%s)' % filename)
        outfile = open(filename, 'w')

        global executables
        if len(executables):
            for line in content.split("\n"):
                for src, target in executables.iteritems():
                    line = re.sub(r"^%s " % src, "%s " % target, line)
                outfile.write(line+"\n")
        else:
            log('content = %s' % content)
            outfile.write(content)

        outfile.close()

        st = os.stat(filename)
        os.chmod(filename, st.st_mode | stat.S_IEXEC)

        return filename


    def _get_from_db(self,depth,distance,startpoint=0,totalpoints=False,sps=False,filter=None,segtype=None):
        """
        Open the database and extract the traces for
        requested depth and distance.

        The functions are archived in ascii files referenced by Datascope using a simple
        wfdisc table. The value for the station is our DISTANCE. The value for the channel
        is our DEPTH and the element is specified in the location code.
        i.e.
            depth: 8
            distance: 10
            element: TDS
            => 10_8_TDS ( format: sta_chan_loc )


        All data will be extracted from the database and archived in memory internal to the class. We
        will use the dictionary self.DATA for this. Each key in the dictionary will be dedicated
        to a different component of the fundamental Green's Functions and will include objects for metadata.
        """

        traces = False

        log("get_from_db()")

        steps = ['dbsubset sta =~ /%s/ && chan =~ /%s_.*/' % (distance,depth) ]
        if segtype:
            steps.extend( ['dbsubset segtype =~ /%s/' % segtype] )

        log( steps )

        with datascope.freeing(self.db.process( steps )) as dbview:
            # Get list of elements and get values for time, endtime, nsamp, samprate and LOC_CODE.
            log( 'Found %s records after subset.' % dbview.record_count )
            if not dbview.record_count: return False
            for record in dbview.iter_record():
                (dist,depth_elem,nsamp,time,endtime,samprate,stype) = \
                        record.getv('sta','chan','nsamp','time','endtime','samprate','segtype')

                log('getv()=> (%s,%s,%s,%s,%s,%s)' % \
                        (dist,depth_elem,nsamp,time,endtime,samprate))

                # Extract the element name from the channel text
                try:
                    m = re.match(".*_(...)",depth_elem)
                    element = m.group(1)
                except Exception,e:
                    error('Problems in regex [.*_(...)] on [%s] %s: %s' % (depth_elem,Exception,e))

                if not element:
                    error('Cannot find component name in wfdisc entry: %s_%s' % (dist,dpeth_elem))

                #trace_name = "%s_%s" % (distance,depth)

                #  Build/clean object for selected channel
                if not traces:
                    # need new object
                    traces = Records(samprate,stype)


                log('trloadchan(%s,%s,%s,%s)'% (time,endtime,dist,depth_elem))
                tr = dbview.trloadchan(time,endtime,dist,depth_elem)

                with datascope.trfreeing(tr):
                    tr.record = 0
                    data = tr.trdata() 

                    if startpoint:
                        data = data[startpoint:]

                    if totalpoints:
                        data = data[0:totalpoints-1]

                    if not data:
                        error('Cannot build component [%s]: %s_%s' % (m.group(1),dsit,depth_elem))

                    # Placing inside Records object
                    traces.trace( element, data)


        return traces


    def _save_to_db(self,record,distance,depth,segtype):
        """ Open the database and save the new traces.

        The functions are archived in ascii files referenced by Datascope using a simple
        wfdisc table. The value for the station is our DISTANCE. The value for the channel
        is our DEPTH and the element is specified in the location code.
        i.e.
            depth: 8
            distance: 10
            element: TDS
            => 10_8_TDS ( format: sta_chan_loc )

        """


        # Save all data to file
        dfile = "%s_%s_%s_%s.gf" % (distance,depth,self.modelname,segtype)
        log( 'distance  %s' % distance  )
        log( 'depth %s' % depth )
        log( 'modal %s' % self.modelname )
        log( 'segtype %s' % segtype )
        log( 'dfile %s' % dfile )

        try:
            f = open("%s/%s"%(self.archive,dfile), 'w')
        except Exception,e:
            raise SystemExit('\n\nERROR: Cannot open file %s %s %s\n'% (dfile,Exception,e))

        for element,data in record:

            log( 'add element %s to database' % element )

            sta = '%s' % distance
            chan_loc = '%s_%s' % (depth,element)
            log( 'name %s_%s' % (sta,chan_loc) )
            #data = record.data(element)

            #record.data(element)
            samprate = record.samplerate
            nsamp = record.samplecount(element)
            log( 'samplerate %s' % samprate )
            log( 'nsamp %s' % nsamp )

            #nsamp = len(self.DATA[element]['data'])
            #samprate = self.DATA[element]['samplerate']
            if segtype == 'D':
                time = 1.0
            else:
                time = 2.0
            endtime = (nsamp*samprate)+time

            # Add small header
            f.write('%s\t%s\t%s\n'%(element,nsamp,samprate))
            start = f.tell()

            #[f.write('%s\n' % x )for x in record.data(elementa) ]
            [f.write('%s\n' % x )for x in data ]

            try:
                wfid = self.db.nextid('wfid')
                keys = ('sta','chan','time','endtime','nsamp','samprate',
                        'calib','datatype','dir','dfile','foff','segtype','wfid')
                values = (sta,chan_loc,time,endtime,nsamp,samprate,1,
                        'as','./gf_files/',dfile,start,segtype,wfid)
                keyvals = zip(keys,values)

                self.db.addv(*keyvals)
            except Exception,e:
                raise SystemExit('\n\nERROR: Cannot add new line [%s] %s %s\n'% (element,Exception,e))

        try:
            f.close()
        except Exception,e:
            raise SystemExit('\n\nERROR: Cannot close file %s %s %s\n'% (file,Exception,e))


        return record




    def _file_format(self,alpha,depth,samplerate,total,model,underevent,distance):
        #_file_format(6.0,depth,(1/self.DT),self.MMAX,model,self.LMAX,distance)
        """
        Template for the earth model.
        """
        log("file_format()")

        text =  ".F.\n     0   64\n"
        text += "%s/GREEN.1\n" % self.tmp_folder

        # FORMAT(2F10.4,3I5,F10.4,I5,I5)
        #text += "%10.1f   %10.1f    1  512 1024%10.f%5d    1\n"
        #text += "    6.0   %05.1f      1  512 1024    %03.1f      %2d    1\n"
        text += "{:8.1f}  ".format(alpha)
        text += "{:8.1f}  ".format(depth)
        text += "    1  512 1024"
        text += "{:8.1f}  ".format(samplerate)
        text += "{:5d}".format(total)
        text += "{:5d}".format(1)
        text += "\n"
        text += "    1    1    1    1    1    1    1    1    1    1    0\n"
        text += model


        # For LMAX:  FORMAT(I5,4F10.4)
        text += "{:5d}".format(underevent)
        text += "\n"

        text += "  0.4000000E+03  1.500000E+00         0\n"
        text += "    1  10000.0     30.0      2.9       2.5\n"

        # LAST LINE: FORMAT(3F10.4)
        text += "{:10.4f}".format(distance)
        text += "    0.0      10.0   \n"

        return text



    def _read_model(self):
        """
        Read parameters from configuration file.
        """
        log("read_model(%s)" % self.model)

        self.name = self.model['name']

        try:
            self.DT = float(self.model['samplerate'])
        except Exception,e:
            raise SystemExit('\n\nWrong Format of samplerate PF file[%s]. %s %s\n'% (self.model,Exception,e))

        try:
            self.DECAY = self.model['decay']
        except Exception,e:
            raise SystemExit('\n\nWrong Format of decay PF file[%s]. %s %s\n'% (self.model,Exception,e))

        try:
            self.N1 = float( self.model['start_frequency'] )
            self.N2 = float( self.model['end_frequency'] )
            self.N  = (self.N2-self.N1+1)*2 # 2 times the total number of freqs
        except Exception,e:
            raise SystemExit('\n\nWrong Format of PF file[%s]. %s %s\n'% (self.model,Exception,e))

        log("read_model()  -  DECAY=%s N1=%s N2=%s N=%s DT=%s " % (self.DECAY,self.N1,self.N2,self.N,self.DT))


        # ISRC, JBDRY
        self.ISRC = [ 1 for x in range(10) ]
        self.JBDRY = 0
        log("read_model()  -  ISRC=%s JBDRY=%s " % (self.ISRC,self.JBDRY))


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
                log(x)

            for x in range(len(temp)):
                t = temp[x].split()
                debug('temp:[%s]' % t)
                if not t: continue
                self.D.append( float(t[0]) )
                self.A.append( float(t[1]) )
                self.B.append( float(t[2]) )
                self.RHO.append( float(t[3]) )
                self.QA.append( 1/float(t[4]) )
                self.QB.append( 1/float(t[5]) )
                debug('D:%s A:%s B:%s RHO:%s QA:%s QB:%s' % (self.D[x],self.A[x],self.B[x],self.RHO[x],self.QA[x],self.QB[x]))

        except Exception,e:
            raise SystemExit('\n\nWrong Format of input file[%s]. %s(%s) \n RAW: %s'% (self.model,Exception,e,temp))



        #self.CMAX = self.model['cmax']
        #self.C1   = self.model['c1']
        #self.C2   = self.model['c2']
        #self.CMIN = self.model['cmin']
        #debug("read_model()  -  CMAX=%s C1=%s C2=%s CMIN=%s " % (self.ISRC,self.C1,self.C2,self.CMIN))

        return



    def plot(self,start=0,end=-1):
        """ Plot all traces in memory. They are containe in
        the dictionary DATA in self.

        """
        log("plot()")

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
            except Exception,e:
                sys.exit('ERROR: problem plotting green functions.[%s => %s]' % (Exception,e) )

        pyplot.suptitle("Green Functions: depth:%s distance:%s" % (self.DEPTH,self.DISTANCE))
        pyplot.show()



    def _fkrsort_script(self,segtype):
        log('New fkrsort script segtype=[%s]' % segtype)

        text =  '''#! /bin/csh
# Window out the eight vectors for many distances
# Remember to set NT and DT CORRECTLY!
# USE wvint9 (The flipped traces are corrected within this
# code rather than by the external program flip
#
'''
        text += "cd %s" % self.tmp_folder
        text += '''
#
set dt=0.50
set npts=1024
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
#
##First inverse fft complex spectra to produce displacement time series
wvint9 << eof
'''

        text += "%s" % segtype.lower()

        text += '''
eof
#
rm -f junk
rm -f tmp*
@ nvec=($loopend - $count) * 10
while ($count < $loopend)
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
cat tmp1$$ tmp2$$ tmp3$$ tmp4$$ tmp5$$ tmp6$$ tmp7$$ tmp8$$ > junk
echo $j
mkHelm format="(6e12.5)" ntr=8 dt=$dt nt=$npts < junk > {$name}{$dist}d{$depth}.raw_greens
rm -f tmp*$$
@ i += 10
@ count++
@ j++
@ dist += 5
end
'''
        return text



if __name__ == "__main__":
    """ Exit if we call this script directly  """

    print "Moment Tensor Green's Functions Library:"
    print "\tDon't run directly. Import into code."
    print "\n\n"
    print "Juan Reyes <reyes@ucsd.edu>"
    sys.exit(9)
