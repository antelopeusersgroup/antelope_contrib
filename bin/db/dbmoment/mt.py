from __main__ import *      # _et all the libraries from parent



class DbMoment(Station):
    """
    Main class for calculating moment tensors of events
    """

    def __init__( self, database, pf, debug=False ,demo=False):
        log( "DbMoment.__init__(%s,%s)" % (database,pf) )

        self.database = database
        self.pf = stock.pfread(pf)
        self.replace_execs = {}
        self.stations = {}
        self.running_demo = demo
        self.rotate = not self.running_demo
        self.debug = debug

        try:
            self._parse_pf()
        except Exception,e:
            sys.exit('ERROR: problem during parsing of pf file(%s) class.[%s]' % (self.pf,e) )

    def _parse_pf( self ):
        """
        Parse the parameter file and assign to vars that will be
        used throughout the script
        """
        debug( "parse parameter file(%s)" % (self.pf) )

        # main config
        self.model_name = safe_pf_get(self.pf, 'model_name')
        self.tmp_folder = os.path.normpath(safe_pf_get(self.pf, 'tmp_folder','/tmp/dbmoment'))
        self.allowed_segtype = safe_pf_get(self.pf, 'allowed_segtype',['D','V'])
        self.filter_high_pass = safe_pf_get(self.pf, 'filter_high_pass')
        self.filter_low_pass = safe_pf_get(self.pf, 'filter_low_pass')

        # databases
        self.wave_db = safe_pf_get(self.pf, 'wave_db', self.database)
        self.resp_db = safe_pf_get(self.pf, 'resp_db', self.database)
        self.green_db = safe_pf_get(self.pf, 'green_db')

        # libs
        self.gf_lib = safe_pf_get(self.pf, 'gf_lib')
        self.inv_lib = safe_pf_get(self.pf, 'inv_lib')
        self.data_lib = safe_pf_get(self.pf, 'data_lib')
        self.event_lib = safe_pf_get(self.pf, 'event_lib')

        # stations
        self.chan_to_use = safe_pf_get(self.pf, 'chan_to_use','LH.*')
        self.sta_max = int(float(safe_pf_get(self.pf, 'sta_max', 25)))
        self.sta_min = int(float(safe_pf_get(self.pf, 'sta_min', 3)))
        self.distance_min = float(safe_pf_get(self.pf, 'distance_min', 0))
        self.distance_max = float(safe_pf_get(self.pf, 'distance_max', 150))

        # event
        self.depth_min = float(safe_pf_get(self.pf, 'depth_min', 0))
        self.depth_max = float(safe_pf_get(self.pf, 'depth_max', 0))

    def mt(self,orid,select='',reject=''):
        """ Get the moment tensor solution for a particular origin. If event is True
        then we get the origin from the event table.
        """
        log( "mt(%s,select=%s,reject=%s)" % (orid,select,reject) )

        # Load modules
        gf_lib = dynamic_loader( self.gf_lib )
        inv_lib = dynamic_loader( self.inv_lib )
        data_lib = dynamic_loader( self.data_lib )
        event_lib = dynamic_loader( self.event_lib )

        # Instantiate Classes
        try:
            log( 'event_lib.Event(%s)' % self.database )
            self.my_event = event_lib.Event(self.database)
        except Exception,e:
            error("EVENT module init error: [%s]" % e)

        # Instantiate Classes
        try:
            log( 'data_lib.Data(%s)' % self.database )
            self.my_data = data_lib.Waveforms(self.wave_db,self.chan_to_use,self.allowed_segtype)
        except Exception,e:
            error("DATA module init error: [%s]" % e)

        # Instantiate Classes
        try:
            self.my_green = gf_lib.Greens( self.green_db, self.model_name,self.tmp_folder)
        except Exception,e:
            error("GREEN module Init Error: [%s]" % e)


        # Extract event information from database
        log( "Get event [%s]" % orid )
        self.my_event.get_event( orid )

        if self.my_event.depth > self.depth_min and self.my_event.depth < self.depth_max:
            log("Event depth [%s] within our limits." % (self.my_event.depth))
        else:
            error("Event depth [%s] out of limits.[%s,%s]" % \
                    (self.my_event.depth,self.depth_min,self.depth_max))

        # Instantiate Classes
        try:
            self.my_inv = inv_lib.MomentTensor(self.my_event.depth,self.tmp_folder)
            pass
        except Exception,e:
            error("Module Init Error: [%s]" % e)

        # Extract data from database
        total = 0
        for sta in self.my_event:

            if len(self.stations) >= self.sta_max:
                warning('Too many stations for this event. Max [%s]' % self.sta_max)
                continue

            if self.my_event.distance(sta) > self.distance_max:
                warning('%s too far from event [%s]km' % \
                        (sta,self.my_event.distance(sta)))
                continue

            if self.my_event.distance(sta) < self.distance_min:
                warning('%s too close to event [%s]km' % \
                        (sta,self.my_event.distance(sta)))
                continue

            traces = self.my_data.get_waveforms( sta, self.my_event.time,
                                    self.my_event.esaz(sta),
                                    rotate=self.rotate)

            if traces:
                # Save traces to my object
                self.stations[sta] = Station(sta)
                self.stations[sta].save( trace='real', data=traces )

                if not self.running_demo:
                    self.stations[sta].filter('real',
                            self.filter_high_pass,
                            self.filter_low_pass)

                self.stations[sta].distance = self.my_event.distance(sta) 
                self.stations[sta].azimuth = self.my_event.esaz(sta) 

                if False:
                    self.stations[sta].plot('real')

                # Get synth for this station
                distance = self.my_event.distance(sta)
                log( 'Need SYNTH for %s at %s km' % (sta, distance) )

                synth = self.my_green.get_synth(distance=distance,
                                                    depth=self.my_event.depth,
                                                    segtype=traces.segtype )

                if synth:
                    self.stations[sta].save( trace='synth', data=synth )

                    #self.stations[sta].flip('synth')

                    if False:
                        self.stations[sta].plot('synth')

                    self.stations[sta].filter('synth',
                            self.filter_high_pass,
                            self.filter_low_pass)

                    if False:
                        self.stations[sta].plot('synth')

                else:
                    error('Problems during get_synth(%s,%s) for %s' % \
                            (distance, self.my_event.depth, sta) )


                log('Synth traces saved to %s' % self.stations[sta].to_file('synth') )
                log('Real traces saved to %s' % self.stations[sta].to_file('real') )

        if len(self.stations) < self.sta_min:
            error('NOT enough stations [%s] for this event. Need [%s]' % \
                    (len(self.stations), self.sta_min))

        log('determine_mt_solution()')
        self.results = self.my_inv.invert(self.stations,self.my_event)

        debug(self.results)

        for sta in self.stations.keys():
            self.stations[sta].convert_synth(self.results,self.tmp_folder)
            #self.stations[sta].plot_results()

        # open a pylab plot of the data
        pyplot.figure(figsize=(20,15))
        total = 0

        total_stations = len(self.stations.keys())
        for sta in sorted(self.stations.keys()):

            debug('Plot traces for results on %s' % sta )
            if not self.stations[sta].real:
                error('Empty Records for data on %s' % sta )
            if not self.stations[sta].synth_zrt:
                error('Empty Records for ZRT on %s' % sta)

            # Scale all traces the same way
            axis = self.stations[sta].max_min('synth')
            real = self.stations[sta].real
            synth = self.stations[sta].synth_zrt

            for chan, data in real:
                pyplot.subplot(total_stations,3,total)
                pyplot.plot(data)
                pyplot.plot(self.stations[sta].synth_zrt.get(chan))
                pyplot.legend(["%s_%s" % (sta,chan)])
                pyplot.axis(axis)
                pyplot.draw()
                total += 1

        title = 'orid:%s  ' % orid
        title += 'Strike:%s ' % self.results['Strike']
        title += 'Rake:%s ' % self.results['Rake']
        title += 'Dip:%s ' % self.results['Dip']
        title += 'Mo:%s ' % self.results['Mo']
        title += 'Mw:%s ' % self.results['Mw']
        title += 'Pdc:%s ' % (self.results['Pdc'] * 100)
        title += 'Pclvd:%s ' % (self.results['Pclvd'] * 100)
        pyplot.suptitle(title)

        filename = "%s/dbmoment_%s_results.png" % (self.tmp_folder,orid)
        notify( 'Save plot with results to temp folder: %s' %  filename)
        pl = pylab.gcf() 
        pylab.savefig(filename,bbox_inches='tight', facecolor=pl.get_facecolor(), edgecolor='none',pad_inches=0.5,dpi=200)

        if self.debug: os.system( "open %s" % filename )


        return self.results

if __name__ == "__main__":
    """ If we call this script directly, then output error  """

    print "\n\t** Not to run directly!!!! **\n"
    print "\n\nNo BRTT support."
    print "Juan Reyes <reyes@ucsd.edu>\n\n"

