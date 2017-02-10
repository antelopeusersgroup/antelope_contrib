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

    def get_waveforms(self, sta, chans, start, esaz=0, seaz=0, delay=0, tw=200,
                    bw_filter=None, debug_plot=False):

        self.logging.debug('Start data extraction for %s' % sta )

        for test_chan in chans:
            self.logging.debug('Attempt subset for %s_%s' % (sta, test_chan) )
            results = self._extract_waveforms(sta, test_chan, start, esaz,
                    seaz, delay, tw, bw_filter, debug_plot)

            if not results:
                self.logging.warning('Empty object returned.')
            else:
                self.logging.debug('Got data.')
                break

        return results



    def _extract_waveforms(self, sta, chans, start, esaz, seaz, delay, tw,
                    bw_filter, debug_plot):

        start = int(delay+start)
        end = int(delay+start+tw)

        self.logging.debug('Get %s data from %s' % (sta,self.database) )
        self.logging.debug('start: %s end:%s' % (start,end) )

        results = False

        steps = ['dbopen wfdisc']
        steps.extend(['dbsubset sta=~/%s/ && chan=~/%s/ && endtime > %s && time < %s' % \
                ( sta, chans, start, end ) ])
        steps.extend(['dbjoin sensor'])
        steps.extend(['dbjoin instrument'])
        steps.extend(['dbsort sta chan'])

        self.logging.debug( 'Database query for waveforms:' )
        self.logging.debug( ', '.join(steps) )

        with datascope.freeing(self.database.process( steps )) as dbview:

            if not dbview.record_count:
                # This failed.
                self.logging.warning( 'No traces after subset for [%s]' % ', '.join(steps) )
                return False

            if dbview.record_count != 3:
                # This failed.
                self.logging.warning( 'Need 3 traces after subset for [%s]. Now %s' % \
                        (sta, dbview.record_count) )
                return False


            # Need some information for trace calibration and later
            # instrument response application to the GreensFunctions.
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
            ( insname, instype, samprate, ncalib, segtype ) = \
                    dbview.getv( 'instrument.insname', 'instrument.instype',
                            'wfdisc.samprate', 'instrument.ncalib',
                            'instrument.rsptype')

            dbview.record = datascope.dbALL


            # Segtypes are set in the parameter file. Need to know what
            # we get from the wfdisc so we can match GreensFunctions units.
            if not segtype in self.allowed_segtype:
                # SKIP THIS SITE
                self.logging.warning( 'Skipping station: %s Wrong type: %s' % (sta,segtype) )
                return False

            # Bring the data into memory. Join segments if fragmented.
            try:
                tr = dbview.trload_cssgrp( start, end + 1 )
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
                fig = plot_tr_object( tr, 'raw-disp', style='b')


            #
            # FILTERING
            #

            # Demean the trace
            tr.trfilter('DEMEAN')

            self.logging.debug('Filter data from %s with [%s]' % (sta, bw_filter))
            try:
                tr.trfilter( bw_filter )
            except Exception,e:
                self.logging.error('Problems with the filter %s => %s' % (bw_filter,e))

            if debug_plot:
                plot_tr_object( tr, 'filtered', style='g', fig=fig)






            #
            # ROTATION
            #
            #Pull name of one channel.
            tr.record = 0
            channame = tr.getv('chan')[0]
            tr.record = datascope.dbALL

            #Verify if we need rotation. Example 1 was alredy on TRZ format.
            #if not channame in ['R','T','Z']:
            if not channame in self.seismic_channels:
                self.logging.debug( 'Rotate to esaz:%s' %  esaz )


                """
                Now we need to rotate the horizontal channels.
                """
                #tr.trrotate(  float(esaz), 0, ('T','R','Z') )
                tr.trrotate(  float(esaz), 0, self.seismic_channels )

                self.logging.debug('Number of traces for %s: %s' % (sta, tr.record_count))
            else:
                self.logging.notify('Not rotation needed. Found %s in wfdisc.' % channame)

            # Subset for the rotated channels only
            tr = tr.subset('chan =~ /R|T|Z/')
            self.logging.debug('Number of traces for %s: %s' % (sta, tr.record_count))

            # Stop here if we lost all traces.
            if not tr.record_count: return False

            if debug_plot:
                plot_tr_object( tr, 'rotaded', style='r', fig=fig)






            #
            # DECIMATE
            #

            # OLD Decimate trace
            #tr = decimate_trace( tr, 1 )

            # NEW Decimate trace
            #decimate_string = 'DECIMATE BY %i' % samprate
            #self.logging.debug( decimate_string )
            #tr.trfilter ( decimate_string )

            #if debug_plot:
            #    plot_tr_object( tr, 'decimated', jump=samprate, style='y', fig=fig)

            # Not working for now. Adding some method at extraction time for now.



            #
            # EXTRACT DATA
            #

            # Make a new Record object and pass the value of the instrument
            # response file.
            #responsefile = '%s/%s/%s' % (self.dbpath,rdir,rdfile) 
            results =  Records(1, segtype=segtype, response=responsefile)

            this = 1
            for trace in tr.iter_record():
                (tsta,chan) = trace.getv('sta','chan')

                self.logging.debug( "sta=%s chan=%s" % (tsta,chan) )

                chan, time, endtime= trace.getv('chan', 'time', 'endtime')
                self.logging.debug('%s => requested(%s,%s) got(%s,%s)' % \
                        (chan, start, end, time, endtime) )

                # TEMP decimation method. 
                # We are way above the min freq for this to be a problem.
                newdata = []
                data = trace.trdata()[int(delay*samprate):int(tw*samprate)]
                for x in range(0,len(data),int(samprate)):
                    newdata.append( data[x] )
                data = newdata


                if not data:
                    # SKIP THIS SITE
                    self.logging.warning( 'Skipping station. No data extracted: %s' % sta )
                    return False
                else:
                    self.logging.debug('Appending data to Record on %s %s' % (sta, chan))
                    results.set_data( chan, data)

                if debug_plot:
                    add_trace_to_plot( results.get( chan ), '.k', 'final-%s' % trace.getv('chan')[0],
                            tr.record_count, this, delay=delay, jump=samprate)
                    this += 1

        if debug_plot: pyplot.show()

        return results


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
