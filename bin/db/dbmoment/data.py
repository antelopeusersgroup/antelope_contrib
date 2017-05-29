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

        self.logging = getLogger('Waveforms')

        self.databasename = dbname
        self.database = None
        self.dbpath = os.path.dirname( os.path.abspath(dbname) )
        self.allowed_segtype = allowed_segtype
        self.logging.debug( 'Init Waveforms Class: db=%s' % self.database )

        # From main dbmoment.xpy script. Also used in functions.py
        global seismic_channels
        self.seismic_channels = seismic_channels

        # Get db ready
        try:
            self.database = datascope.dbopen( self.databasename, "r+" )
            self.db = self.database.lookup(table='wfdisc')
        except Exception,e:
            self.logging.error('Problems opening wfdisc: %s %s' % (self.database,e) )

        if not self.db.record_count:
            self.logging.error( 'No data in wfdisc %s' % self.database )

    def get_waveforms(self, sta, chans, start_time, esaz=0, seaz=0, delay=0, tw=200,
                    bw_filter=None, debug_plot=False):

        self.logging.debug('Start data extraction for %s' % sta )
        results = False

        if isinstance(chans, basestring):
            chans = [chans]

        start = start_time -tw
        end = start_time + tw + delay

        self.logging.debug('Get %s data from %s' % (sta,self.database) )
        self.logging.debug('start: %s end:%s' % (start,end) )

        # Build REGEX for subset of database
        channel_regex =  []
        for c in chans:
            channel_regex.append( 'chan =~ /%s/' % c )

        channel_string = ' || '.join( channel_regex )


        steps = ['dbopen wfdisc']
        steps.extend(['dbsubset sta=~/%s/ && endtime > %s && time < %s && ( %s ) ' % \
                ( sta, start, end, channel_string) ])
        steps.extend(['dbjoin sensor'])
        steps.extend(['dbjoin instrument'])
        steps.extend(['dbsort sta chan'])

        self.logging.debug( 'Database query for waveforms:' )
        self.logging.debug( ', '.join(steps) )

        dbview = self.database.process( steps )

        if not dbview.record_count:
            # This failed.
            self.logging.info( 'No traces after subset for sta =~ [%s]' % sta )
            dbview.free()
            return False

        self.logging.info( '%s traces after subset for sta =~ [%s]' % (dbview.record_count,sta) )


        for c in chans:

            dbsubset = dbview.subset('chan =~ /%s/' % c)

            if not dbsubset.record_count:
                # This failed.
                self.logging.info( 'No traces after subset for %s [%s]' % (sta,c) )
                dbsubset.free()
                continue

            if dbsubset.record_count != 3:
                # This failed.
                self.logging.warning( 'Need 3 traces after subset for chan == [%s]. Now %s' % \
                        (c, dbview.record_count) )
                dbsubset.free()
                continue

            results = self._extract_waveforms(dbsubset, sta, c, start, end, esaz,
                    seaz, delay, tw, bw_filter, debug_plot)

            if results:
                self.logging.debug('Got data.')
                dbsubset.free()
                break
            else:
                self.logging.warning('Empty object returned.')

            dbsubset.free()

        dbview.free()

        return results



    def _extract_waveforms(self, dbview, sta, chans, start, end, esaz, seaz, delay, tw,
                    bw_filter, debug_plot):

        results = 0
        demo = False

        # Look for information in the first record
        dbview.record = 0

        # Get response file path from database row
        self.logging.debug('Get reponse file from database entry')
        responsefile = dbview.extfile('instrument')
        self.logging.debug(responsefile)

        if len(responsefile) > 0:
            responsefile = responsefile[1]
        else:
            responsefile = False

        self.logging.debug('Response file: %s' % responsefile)


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
            self.logging.warning( 'Skipping station: %s Wrong type: %s' % (sta,segtype) )
            return False


        # Verify if we have the TESTDATA.
        if re.search( r'testdata.*', dfile):
            self.logging.debug('Working with test data. Remove data delay of : [%s]' % delay )
            end -= delay
            demo = True

        # Bring the data into memory. Join segments if fragmented.
        try:
            tr = dbview.trload_cssgrp( start, end )
            tr.trsplice()
        except Exception, e:
            self.logging.error('Could not prepare data for %s:%s [%s]' % (sta,chans, e))

        # Stop here if we don't have something to work with.
        if not tr.record_count:
            self.logging.warning( 'No data after trload for %s' % sta )
            return False

        if tr.record_count > 3:
            # Recursive call to a new subset
            self.logging.warning( 'Too many traces after trload_cssgrp for [%s]. Now %s' % \
                    (sta, tr.record_count) )
            return False


        # Demean the trace
        tr.trfilter('BW 0 0 2 4')
        tr.trfilter('DEMEAN')


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

        if debug_plot:
            fig = plot_tr_object( tr, 'displacement', style='b')
            #plot_tr_object( tr, 'displacement', style='b', fig=fig)


        #
        # FILTERING
        #
        self.logging.debug('Filter data from %s with [%s]' % (sta, bw_filter))
        try:
            tr.trfilter( bw_filter )
        except Exception,e:
            self.logging.warning('Problems with the filter %s => %s' % (bw_filter,e))
            return False

        if debug_plot:
            plot_tr_object( tr, 'filtered', style='g', fig=fig)



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
            self.logging.debug( 'Rotate to esaz:%s' %  esaz )


            """
            Now we need to rotate the horizontal channels.
            """
            try:
                tr.trrotate(  float(esaz), 0, self.seismic_channels )
            except Exception,e:
                self.logging.error('Problems with trrotate %s => %s' % (Exception,e))

            self.logging.debug('Number of traces for after rotation %s: %s' % (sta, tr.record_count))
        else:
            self.logging.notify('Not rotation needed. Found %s in wfdisc.' % channame)



        # Subset for the rotated channels only
        tr = tr.subset('chan =~ /R|T|Z/')
        self.logging.debug('Number of traces after subset for R|T|Z %s: %s' % (sta, tr.record_count))


        # Found issues with gaps and the decimate filter. Forcing split on gaps now.
        tr.trsplit()

        # Stop here if we lost traces.
        if tr.record_count != 3:
            self.logging.warning('Problem after rotation  or gaps detected in [%s] records' % (tr.record_count))
            return False

        if debug_plot:
            plot_tr_object( tr, 'rotaded', style='r', fig=fig)


        #
        # DECIMATE
        #
        if int(samprate) > 1:
            tr = add_padding( tr )
            decimate_string = 'DECIMATE BY %i' % samprate
            self.logging.debug( decimate_string )

            # Verify gaps
            gap_in_trace = False
            for t in tr.iter_record():

                nsamp, samprate, time, endtime= t.getv('nsamp', 'samprate', 'time', 'endtime')

                if not valid_number(samprate):
                    gap_in_trace = True
                    break

                if not valid_number(nsamp):
                    gap_in_trace = True
                    break

                data =t.trdata()

                if [ x for x in data if x >= 1.e20 ] :
                    gap_in_trace = True
                    break

                if len(data) != nsamp :
                    gap_in_trace = True
                    break

                self.logging.info('time:%s endtime:%s nsamp:%s samprate:%s)' % \
                        (time, endtime, nsamp, samprate))

            try:
                # set error if gap present
                if gap_in_trace: raise ValueError('Gap in trace object')

                with timeout(seconds=4):
                    tr.trfilter ( decimate_string )

            except Exception,e:
                self.logging.warning('DECIMATE call error. Possible gap. [%s] %s' % (Exception,e))
                return False


        #
        # EXTRACT DATA
        #
        self.logging.debug( 'Extract final data' )

        # Make a new Record object and pass the value of the instrument
        # response file.
        results =  Records(1, segtype=segtype, response=responsefile)

        for trace in tr.iter_record():

            tsta, chan, time, endtime= trace.getv('sta', 'chan', 'time', 'endtime')
            self.logging.debug('%s_%s => requested(%s,%s) got(%s,%s)' % \
                    (tsta, chan, start, end, time, endtime) )


            # Make sure that no NULL values are exported
            data = [ x for x in trace.trdata()[-tw:] if x < 1.e20 ]

            if debug_plot:
                if demo:
                    gap = int(original_secs - len(data))
                else:
                    gap = int(original_secs - len(data)- delay) - 1

                add_trace_to_plot( data, '.k', 'final-%s' % chan,
                        tr.record_count, trace.record+1, delay=gap , jump=samprate)


            if not data:
                # SKIP THIS SITE
                self.logging.warning( 'Skipping station. No data extracted: %s' % sta )
                return False

            self.logging.debug('Appending data to Record on %s %s' % (sta, chan))
            results.set_data( chan, data)


        if debug_plot:
            pyplot.suptitle( sta )
            pyplot.show()

        return results


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
