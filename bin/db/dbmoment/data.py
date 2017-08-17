#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


from __main__ import *      # _et all the libraries from parent

class Waveforms():
    """Class for extracting wavform data
    from the database and creating the plots.
    """

    def __init__(self,dbname,allowed_segtype=[]):

        self.databasename = dbname
        self.database = None
        self.dbpath = os.path.dirname( os.path.abspath(dbname) )
        self.allowed_segtype = allowed_segtype
        elog.debug( 'Init Waveforms Class: db=%s' % self.database )

        # From main dbmoment.xpy script. Also used in functions.py
        global seismic_channels
        self.seismic_channels = seismic_channels

    def get_waveforms(self, sta, chans, start_time, esaz=0, seaz=0, tw=200,
                    filters=None, debug_plot=False):

        elog.debug('Start data extraction for %s' % sta )
        results = False

        if isinstance(chans, basestring):
            chans = [chans]

        start = start_time - tw
        end = start_time + tw

        elog.debug('Get %s data from %s' % (sta,self.database) )
        elog.debug('start: %s end:%s' % (start,end) )

        # Build REGEX for subset of database
        channel_regex =  []
        for c in chans:
            channel_regex.append( 'chan =~ /%s/' % c )

        channel_string = ' || '.join( channel_regex )


        # Get db ready
        if self.database:
            try:
                self.database.free()
            except:
                pass

        if not self.databasename:
            elog.error('Missing database for Origin query.')


        # Get db ready
        try:
            self.database = datascope.dbopen( self.databasename, "r+" )
            self.db = self.database.lookup(table='wfdisc')
        except Exception,e:
            elog.error('Problems opening wfdisc: %s %s' % (self.database,e) )

        if not self.db.record_count:
            elog.error( 'No data in wfdisc %s' % self.database )


        steps = ['dbopen wfdisc']
        steps.extend(['dbsubset sta=~/%s/ && endtime > %s && time < %s && ( %s ) ' % \
                ( sta, start, end, channel_string) ])
        steps.extend(['dbjoin sensor'])
        steps.extend(['dbjoin instrument'])
        steps.extend(['dbsort sta chan'])

        elog.debug( 'Database query for waveforms:' )
        elog.debug( ', '.join(steps) )

        dbview = self.database.process( steps )

        if not dbview.record_count:
            # This failed.
            elog.warning( 'No traces after subset for sta =~ [%s]' % sta )
            dbview.free()
            return False

        elog.debug( '%s traces after subset for sta =~ [%s]' % (dbview.record_count,sta) )


        for c in chans:

            dbsubset = dbview.subset('chan =~ /%s/' % c)

            if not dbsubset.record_count:
                # This failed.
                elog.debug( 'No traces after subset for %s [%s]' % (sta,c) )
                dbsubset.free()
                continue

            if dbsubset.record_count != 3:
                # This failed.
                elog.warning( 'Need ONLY 3 traces after subset for chan == [%s]. Now %s' % \
                        (c, dbview.record_count) )
                dbsubset.free()
                continue

            results = self._extract_waveforms(dbsubset, sta, c, start, end, esaz,
                    seaz, tw, filters, debug_plot)

            if results:
                elog.debug('Got data.')
                dbsubset.free()
                break
            else:
                elog.warning('Empty object returned.')

            dbsubset.free()

        dbview.free()

        return results



    def _extract_waveforms(self, dbview, sta, chans, start, end, esaz, seaz, tw,
                    filters, debug_plot):

        temp_results = {}

        # Look for information in the first record
        dbview.record = 0

        # Get response file path from database row
        elog.debug('Get reponse file from database entry')
        responsefile = dbview.extfile('instrument')
        elog.debug(responsefile)

        if len(responsefile) > 0:
            responsefile = responsefile[1]
        else:
            responsefile = False

        elog.debug('Response file: %s' % responsefile)


        # Extract some parameters from instrument and wfdsic table
        ( samprate, ncalib, segtype, dfile ) = \
                dbview.getv( 'wfdisc.samprate', 'instrument.ncalib',
                        'instrument.rsptype', 'wfdisc.dfile')


        # Return view to all traces
        dbview.record = datascope.dbALL


        # Segtypes are set in the parameter file. Need to know what
        # we get from the wfdisc so we can match GreensFunctions units.
        if not segtype in self.allowed_segtype:
            # SKIP THIS SITE
            elog.warning( 'Skipping station: %s Wrong data type: %s' % (sta,segtype) )
            return False


        # Bring the data into memory. Join segments if fragmented.
        try:
            tr = dbview.trload_cssgrp( start, end )
            tr.trsplice()
            tr.trsplit()
        except Exception, e:
            elog.warning('Could not read data for %s:%s [%s]' % (sta,chans, e))
            return False


        # Stop here if we don't have something to work with.
        if not tr.record_count:
            elog.warning( 'No data after trload for %s' % sta )
            return False

        if tr.record_count > 3:
            # Recursive call to a new subset
            elog.warning( 'Too many traces after trload_cssgrp for [%s]. Now %s' % \
                    (sta, tr.record_count) )
            return False

        #
        # CONVERT TO CM
        #

        # Use calib values from instrument table if missing on wfdisc
        for t in tr.iter_record():
            if not float( t.getv('calib')[0] ):
                t.putv( ('calib',ncalib) )

        # Need real units, not counts.
        tr.trapply_calib()

        # Integrate if needed to get displacement
        if segtype == 'A':
            tr.trfilter('INT2')
            # Need to bring the data from nm to cm and match the gf's
            tr.trfilter( "G 0.0000001" )
            segtype = 'D'
        elif segtype == 'V':
            tr.trfilter('INT')
            # Need to bring the data from nm to cm and match the gf's
            tr.trfilter( "G 0.0000001" )
            segtype = 'D'
        elif segtype == 'D':
            # Need to bring the data from nm to cm and match the gf's
            tr.trfilter( "G 0.0000001" )
        else:
            elog.warning( 'Unknown data type [%s] for %s in dbmoment. [A,V,D]' % \
                    (segtype, sta) )
            return False

        # Clean up the traces a bit. Remove high frequency energy
        tr.trfilter('BW 0 0 2 4')

        # Demean the trace
        tr.trfilter('DEMEAN')


        #
        # ROTATION
        #
        #Pull name of one channel.
        tr.record = 0
        channame = tr.getv('chan')[0]
        original_points = len(tr.trdata())
        original_secs = original_points / samprate
        tr.record = datascope.dbALL

        #Verify if we need rotation. Example 1 was alredy on TRZ format.
        if not channame in self.seismic_channels:
            elog.debug( 'Rotate to esaz:%s' %  esaz )


            """
            Now we need to rotate the horizontal channels.
            """
            try:
                tr.trrotate(  float(esaz), 0, self.seismic_channels )
            except Exception,e:
                elog.warning('Problems with trrotate %s => %s' % (Exception,e))
                return False

            elog.debug('Number of traces for after rotation %s: %s' % (sta, tr.record_count))
        else:
            elog.notify('Not rotation needed. Found %s in wfdisc.' % channame)



        # Subset for the rotated channels only
        tr = tr.subset('chan =~ /R|T|Z/')
        elog.debug('Number of traces after subset for R|T|Z %s: %s' % (sta, tr.record_count))


        # Stop here if we lost traces.
        if tr.record_count != 3:
            elog.warning('Problem after rotation  or gaps detected in [%s] records' % (tr.record_count))
            return False


        # Need to track original
        original = {}
        for t in tr.iter_record():
            original[ t.getv('chan')[0] ] = t.trdata()


        #
        # Loop over every possible filter
        #
        for f in filters:

            # Return this version to the original data
            for t in tr.iter_record():
                t.trputdata( original[ t.getv('chan')[0] ] )


            if debug_plot:
                fig = plot_tr_object( tr, 'rotaded-Disp', style='r')
                #plot_tr_object( tr, 'displacement', style='b', fig=fig)


            elog.debug('Filter data from %s with %s' % (sta, f))
            try:
                tr.trfilter( f )
            except Exception,e:
                elog.warning('Problems with the filter %s => %s:%s' % \
                        (f,Exception,e))
                return False

            if debug_plot:
                plot_tr_object( tr, f, style='y', fig=fig)


            #
            # EXTRACT DATA
            #
            elog.debug( 'Extract final data' )

            # Make a new Record object and pass the value of the instrument
            # response file.
            temp_results[ f ] =  Records(1, segtype=segtype, response=responsefile)

            # Verify gaps
            for t in tr.iter_record():

                tsta, tchan, nsamp, samprate, time, endtime = \
                        t.getv('sta', 'chan', 'nsamp', 'samprate', 'time', 'endtime')

                elog.debug('sta:%s chan:%s time:%s endtime:%s nsamp:%s samprate:%s)' % \
                        (tsta, tchan, time, endtime, nsamp, samprate))


                if int(samprate) > 1:

                    if (endtime - time) < (end - start) * 0.95:
                        elog.debug('Short trace. Avoid [DECIMATE BY] filter')
                        elog.debug('%s secs vs %s secs' % \
                                ( (endtime - time), (end - start)) )
                        # SIMPLE decimation method.
                        # We are way above the min freq for this to be a problem.
                        data = []
                        #data = trace.trdata()[int(samprate):int(tw*samprate)]
                        elog.debug( 'Extract data' )
                        temp_data = t.trdata()
                        elog.debug( 'Simple decimation' )
                        for x in range(0,len(temp_data),int(samprate)):
                            data.append( temp_data[x] )

                    else:
                        decimate_string = 'DECIMATE BY %i' % samprate
                        elog.debug( decimate_string )
                        t.trfilter ( decimate_string )
                        elog.debug( 'Extract data' )
                        data = t.trdata()

                else:

                    elog.debug( 'Extract data' )
                    data = t.trdata()

                # Subset the trace to select only the last segment
                data = data[-tw:]

                # Verify if we have NULL values in array
                data = [ x for x in data if x < 1.e20 ]

                if not data:
                    elog.warning('No data after extraction.')
                    return False


                elog.debug('Got %s points for %s seconds' % (len(data), tw))
                if len(data) != tw:
                    elog.warning('Mismatch between data and time window')
                    return False


                # Add to plot
                if debug_plot:

                    gap = int(original_secs - len(data))

                    add_trace_to_plot( data, '.', f,
                            tr.record_count, t.record+1, delay=gap , jump=samprate)



                elog.debug('Appending data to Record on %s %s' % (tsta, tchan))
                temp_results[ f ].set_data( tchan, data)

        tr.free()


        if debug_plot:
            pyplot.suptitle( sta )
            pyplot.show()

        return temp_results


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
