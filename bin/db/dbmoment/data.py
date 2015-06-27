from __main__ import *      # _et all the libraries from parent

class Waveforms(Records):
    """Class for extracting wavform data
    from the database and creating the plots.
    """

    def __init__(self,dbname,chan='.*',allowed_segtype=[]):
        self.database = dbname
        self.chan_to_use = chan
        self.allowed_segtype = allowed_segtype
        log( 'Init Waveforms Class: db=%s' % self.database )

        # Get db ready
        try:
            self.db = datascope.dbopen( self.database, "r+" )
            self.db = self.db.lookup(table='wfdisc')
        except Exception,e:
            error('Problems opening wfdisc: %s %s' % (self.database,e) )

        if not self.db.record_count:
            error( 'No data in wfdisc %s' % self.database )

    def get_waveforms(self, sta, start, esaz, tw=200, rotate=True):

        log('Get %s data from %s' % (sta,self.database) )
        log('esaz: %s' % esaz )

        start = float(start)
        end = float(start+tw-1)
        debug('start: %s' % start )
        debug('end: %s' % end )

        results = False

        regex = 'sta=~/%s/ && chan=~/%s/ && endtime > %s && time < %s' % \
                ( sta,self.chan_to_use, start, end )

        with datascope.freeing(self.db.subset( regex )) as dbview:
            if not dbview.record_count:
                # This failed. Lets see what we have in the db
                log( 'No traces after subset for [%s]' % regex )
                return False

            if dbview.record_count > 3:
                # This failed. Lets see what we have in the db
                log( 'Too many traces after subset for [%s]' % regex )
                return False

            try:
                # padd a little the requested trace. we will remove that later
                tr = dbview.trload_css( start-60, end+60 )
                tr.trapply_calib()
                tr.trsplice()
            except Exception, e:
                sys.exit('Could not prepare data for %s:%s [%s]' % (sta,self.chan_to_use, e))

            if not tr.record_count: return results

            if rotate:
                try:
                    # For instance, for a P wave propagating at an azimuth of 45 degrees
                    # (i.e. propagating from SW to NE) with a computed emergence angle of
                    # 15 degrees from the vertical, trrotate would rotate the data into
                    # ray coordinates with X1 being the pure radial direction and x2 being
                    # the pure SH direction by setting phi=-45.0 and theta = -75.0).
                    rotchan = ('R', 'T', 'Z')
                    debug( 'Rotate to %s esaz:%s' %  (rotchan,esaz) )
                    tr.trrotate(-float(esaz), 0, rotchan)
                except Exception, e:
                    error('Could not rotate data %s [%s]' % (sta, e))

            log('Number of traces for %s: %s' % (sta, tr.record_count))

            tr = tr.subset('sta =~ /%s/ && chan =~ /R|T|Z/' % sta)
            tr.record = 0
            sps = tr.getv('samprate')[0]
            segtype = tr.getv('segtype')[0]
            tr.record = datascope.dbALL

            if not segtype in self.allowed_segtype:
                # remove if needed
                warning( 'Skipping station: %s Wrong type: %s' % (sta,segtype) )
                return False


            for trace in tr.iter_record():
                (tsta,chan) = trace.getv('sta','chan')

                debug( "sta=%s chan=%s" % (tsta,chan) )

                #with datascope.trfreeing(tr):
                #    for trace in tr.iter_record():
                chan, time, endtime= trace.getv('chan', 'time', 'endtime')
                debug('%s => requested(%s,%s) got(%s,%s)' % \
                        (chan, start, end, time, endtime) )

                if not verify_trace_time(start, end, time, endtime):
                    # remove from memory if needed
                    warning( 'Skipping station: %s' % sta )
                    return False

                if not results:
                    # need new object
                    results =  Records(sps,segtype)


                # Placing inside Trace object
                data =  clean_trace( trace.trdata(), sps, time, endtime, start, end )
                debug('Appending data to Record on %s %s' % (sta, chan))
                results.set_data( chan, data)
                #log (data[:5])

        return results


if __name__ == "__main__":
    print "Moment Tensor Data Extaction Library:"
    print "\tDon't run directly. Import into code."
    print "\n\n"
    print "No BRTT support."
    print "Juan Reyes <reyes@ucsd.edu>"
    sys.exit(9)
