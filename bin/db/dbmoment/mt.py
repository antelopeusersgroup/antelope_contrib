#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


from __main__ import *      # _et all the libraries from parent


class DbMoment(Station):
    '''
    Main class for calculating moment tensors of events

    This class is responsible for importing all modules needed for the
    calculation of the MT. The main PF file "dbmoment.pf" will specify
    the names of the modules that we want and will have the main parameters
    for the inversion. The velocity model needed for the Synthetics is
    also listed in that file. The model is just a secondary parameter file
    with all required values for our velocity model.
    '''

    def __init__( self, database, options, model):

        self.logging = getLogger('DbMoment')

        self.logging.debug( "DbMoment.__init__(%s)" % (database) )

        self.database = database
        self.pf = open_verify_pf(options.pf)
        self.stations = {}
        self.verbose = options.verbose
        self.debug = options.debug
        self.debug_each = options.debug_each

        self.select = options.select
        self.reject = options.reject

        self.model = model

        '''
        ZCOR is a value used for shifting the traces in time
        and achieve a better fit. Each ZCOR is equal to one point
        and since we work with 1Hz data then that is equal to 1 second.
        The main calculation will try to get the best ZCOR unless we provide
        one different than 0 (zero). In that case the value of ZCOR is
        applied to that station.
        '''
        self.zcor = {}
        if options.zcor:
            for kv in options.zcor.split(','):
                each = kv.split(':')
                self.zcor[ each[0] ] = int( each[1] )
            self.logging.debug( self.zcor )

        # Main parameter file: dbmoment.pf
        try:
            self._parse_pf()
        except Exception,e:
            self.logging.error('ERROR: problem during parsing of pf file(%s) class.[%s]' % (self.pf,e) )


    def _parse_pf( self ):
        '''
        Parse the parameter file and assign to vars that will be
        used throughout the script. The default value is dbmoment.pf
        In this method we also have listed some default values for those
        parameters listed in the parameter file.
        '''
        self.logging.debug( "parse parameter file(%s)" % (self.pf) )

        # main config
        self.tmp_folder = os.path.relpath( safe_pf_get(self.pf, 'tmp_folder','.dbmoment') )
        self.img_folder = os.path.relpath( safe_pf_get(self.pf, 'img_folder','dbmoment_images') )
        self.allowed_segtype = safe_pf_get(self.pf, 'allowed_segtype',['D','V'])
        #self.time_window = int( safe_pf_get(self.pf, 'time_window',200) )
        self.arrivals_only = stock.yesno( safe_pf_get(self.pf, 'stations_arrivals_only',True) )
        self.recursive = stock.yesno( safe_pf_get(self.pf, 'recursive_analysis',True) )
        self.min_variance = int( safe_pf_get(self.pf, 'min_variance',70) )
        self.min_quality = int( safe_pf_get(self.pf, 'min_quality',2) )
        #self.default_magnitude  = float(safe_pf_get(self.pf, 'default_magnitude',  4.0) )
        self.filter_individual_mw  = stock.yesno(safe_pf_get(self.pf, 'filter_individual_mw',  False) )
        self.individual_mw_threshold  = float(safe_pf_get(self.pf, 'individual_mw_threshold', 1.0) )
        self.acknowledgement  = safe_pf_get(self.pf, 'acknowledgement', '')


        # overwrite min_variance if option.min_variance is set
        if options.min_fit: self.min_variance = int( options.min_fit )

        # stations
        self.chan_to_use = safe_pf_get(self.pf, 'chan_to_use',['.*'])
        self.sta_max = int(float(safe_pf_get(self.pf, 'sta_max', 25)))
        self.sta_min = int(float(safe_pf_get(self.pf, 'sta_min', 3)))

        # event
        self.depth_min = int(safe_pf_get(self.pf, 'depth_min', 0))
        self.depth_max = int(safe_pf_get(self.pf, 'depth_max', 0))

        # filters
        '''
        The default values come from Dreger's PDF documentation "TDMT_INVC.pdf"
        We use a magnitude dependent frequency passband, where for M<4.0 the
        passband is 0.02 to 0.1 Hz, for 4.0 <= M < 5.0 the passband
        is 0.02 to 0.05 Hz, for M>= 5.0 the passband is 0.01 to 0.05 Hz.
        For very large events (M>7.5) a passband of 0.005 to 0.02 Hz is
        desirable (Fukuyama and Dreger, 2000).
        '''
        #self.filter = {}
        #self.filter[0]  = safe_pf_get(self.pf, 'mag_0_filter',  'BWZ 0.02 4 0.1 4')
        #self.filter[1]  = safe_pf_get(self.pf, 'mag_1_filter',  'BWZ 0.02 4 0.1 4')
        #self.filter[2]  = safe_pf_get(self.pf, 'mag_2_filter',  'BWZ 0.02 4 0.1 4')
        #self.filter[3]  = safe_pf_get(self.pf, 'mag_3_filter',  'BWZ 0.02 4 0.1 4')
        #self.filter[4]  = safe_pf_get(self.pf, 'mag_4_filter',  'BWZ 0.02 4 0.05 4')
        #self.filter[5]  = safe_pf_get(self.pf, 'mag_5_filter',  'BWZ 0.02 4 0.05 4')
        #self.filter[6]  = safe_pf_get(self.pf, 'mag_6_filter',  'BWZ 0.01 4 0.05 4')
        #self.filter[7]  = safe_pf_get(self.pf, 'mag_7_filter',  'BWZ 0.005 4 0.02 4')
        #self.filter[8]  = safe_pf_get(self.pf, 'mag_8_filter',  'BWZ 0.005 4 0.02 4')
        #self.filter[9]  = safe_pf_get(self.pf, 'mag_9_filter',  'BWZ 0.005 4 0.02 4')
        #self.filter[10] = safe_pf_get(self.pf, 'mag_10_filter', 'BWZ 0.005 4 0.02 4')

        self.config = {}
        for x in range(11):
            self.config[x] = safe_pf_get( self.pf, 'mag_%s' % x )

        # databases/folders
        self.wave_db = safe_pf_get(self.pf, 'wave_db', self.database)
        self.resp_db = safe_pf_get(self.pf, 'resp_db', self.database)
        self.synth_db_folder = safe_pf_get(self.pf, 'synth_db_folder', 'synthetics_dbs/')

        # libs
        '''
        Dynamically loaded libraries. The names provided on the PF file could
        change but internally we keep referencing them in the same way.
        We do depend that those libraries respect the global environment  and
        accept and return the expected parameters.
        '''
        self.synth_lib = safe_pf_get(self.pf, 'synth_lib')
        self.inv_lib = safe_pf_get(self.pf, 'inv_lib')
        self.data_lib = safe_pf_get(self.pf, 'data_lib')
        self.event_lib = safe_pf_get(self.pf, 'event_lib')


    def mt(self, orid):
        '''
        Main method to control the inversion.
        Need to run with any origin from the database and will return all results
        for that inversion.
        '''
        self.logging.debug( "mt(%s,select=%s,reject=%s)" % (orid,self.select,self.reject) )

        '''
        Load modules
        All four modules are loaded here. There is a function
        that help with the task but at the end it should return
        and object that we can use for the processing. If we have
        any problem the function is in charge of the exception. We don't
        verify the returned object, we only try to run the expected method
        on each of them.
        '''
        synth_lib = dynamic_loader( self.synth_lib )
        inv_lib = dynamic_loader( self.inv_lib )
        data_lib = dynamic_loader( self.data_lib )
        event_lib = dynamic_loader( self.event_lib )

        # Instantiate Origin Class
        try:
            self.logging.debug( 'event_lib.Origin(%s)' % self.database )
            self.my_event = event_lib.Origin( self.database )
        except Exception,e:
            self.logging.error("EVENT module init error: [%s]" % e)

        # Instantiate Data Class
        try:
            self.logging.debug( 'data_lib.Data(%s)' % self.database )
            self.my_data = data_lib.Waveforms(self.wave_db,self.allowed_segtype)
        except Exception,e:
            self.logging.error("DATA module init error: [%s]" % e)

        # Instantiate Synthetics Class
        try:
            self.my_synthetics = synth_lib.Synthetics( self.synth_db_folder,
                                            self.model, self.tmp_folder)
        except Exception,e:
            self.logging.error("Synthetics module Init Error: [%s]" % e)

        # Instantiate Inversion Classes. Dreger's code wrapper.
        try:
            self.my_inv = inv_lib.MomentTensor()
            pass
        except Exception,e:
            self.logging.error("Inversion Module Init Error: [%s]" % e)

        '''
        ALL THE CODE IS WORKING FINE AT THIS POINT.
        START WORKING ON THE DATABASES.
        '''


        # Extract event information from database
        self.logging.notify( "Get orid [%s]" % orid )
        self.my_event.set_distance_step( self.model['distance_step'] )
        self.my_event.get_origin( orid , self.select, self.reject)
        self.my_inv.set_depth( self.my_event.depth )
        self.my_inv.set_folder( self.tmp_folder )

        # Save short name of model in object
        self.my_event.model = self.my_synthetics.model_short_name


        # Verify we have a valid event
        if self.my_event.depth >= self.depth_min and self.my_event.depth <= self.depth_max:
            self.logging.info("Event depth [%s] within our limits." % (self.my_event.depth))
        else:
            self.logging.error("Event depth [%s] out of limits.[%s,%s]" % \
                    (self.my_event.depth,self.depth_min,self.depth_max))


        # Extract configuration for this event from the ParameterFile.
        temp_config = self.config[ int(self.my_event.magnitude) ]
        self.my_event.filter = temp_config[ 'filter' ]
        self.time_window = int(temp_config[ 'time_window' ])
        self.distance_max = int(temp_config[ 'distance_max' ])
        self.distance_min = int(temp_config[ 'distance_min' ])

        # Verify filter
        if not self.my_event.filter:
            self.logging.error( 'Problems looking for filter for this orid. Magnitude %s' % self.my_event.magnitude )

        # Verify time-window size
        if self.time_window > 200:
            self.logging.error( 'Need to keep time_window under 200. Set to %s' % self.time_window )



        # Find filter to use.
        if options.filter:
            '''
            Use the provided value from command-line
            There is no test for validity of the filter. We do have a try:except during
            the filtering process but it might be good to add something here before
            we start pulling data.
            '''
            self.logging.debug( 'Using filter from command line [%s]' % options.filter )
            self.my_event.filter = options.filter

            ## If we don't have a filter from command-line then try to find one from database values
            #try:
            #    # First we try to get a filter from a previous magnitude of the event
            #    self.logging.debug( 'Magnitude [%s] from event database.' % self.my_event.magnitude )
            #    self.my_event.filter = self.filter[ int(self.my_event.magnitude) ]
            #except Exception,e:
            #    # If we don't have a previous magnitude then we use the default
            #    #self.logging.warning( 'No magnitude from event database. Going with default [%s].' % \
            #    #        self.default_magnitude )
            #    #self.my_event.filter = self.filter[ int(self.default_magnitude) ]
            #    self.logging.error( 'No defined filter for magnitude . [%s].' % self.my_event.magnitude )

        if not self.my_event.filter:
            self.logging.error( 'No defined filter for magnitude: [%s]' % self.my_event.magnitude )
        else:
            self.logging.info( 'Using filter: [%s]' % self.my_event.filter )


        '''
        So we have a list of stations that we got from the event database.
        The code will now try to evaluate an inversion for each station individually
        so we can get a new list ordered by best fit to lowest fit. All stations that
        fall bellow the min_correlation value in the PF file will not make it to the
        list. Then we can do some recursive cleanup on the final inversion.
        '''
        # sorted by distance
        total_stations = self.my_event.station_list()
        self.logging.info( 'Total list of stations: %s' % total_stations )

        good_stations = {}

        for sta in total_stations:

            self.logging.debug('Test individual station [%s]' % sta)
            results = self.add_station( sta )
            test = {}

            if results:
                # Need to put the data into disk
                #   ****** FILES ARE SAVED TO DISK ON THESE 2 LINES. ********
                self.logging.debug('Real traces saved to %s' % results.to_file('real') )
                self.logging.debug('Synthetic traces saved to %s' % results.to_file('synth') )

                # We have the information for this station. Let's try the fit alone.
                test[sta] = results
                single_fit = self.my_inv.invert( test )

                self.logging.debug(single_fit)


                # Test for min-correlation
                if not self._good_fit(single_fit['variance'][sta], self.min_variance):
                    self.logging.warning('DISCARD FOR BAD CORRELATION: %s zcor:%s variance:%s' %
                            (sta, single_fit['zcor'][sta], single_fit['variance'][sta]) )

                    # Call clean() so we remove the temp files from disk
                    results.clean()
                    continue

                # Test if magnitude is in range
                if self.filter_individual_mw and float(self.my_event.magnitude) > 0.0 and \
                    float(single_fit['Mw']) > float(self.my_event.magnitude) + self.individual_mw_threshold:
                    self.logging.warning('DISCARD %s relative Mw:%s > %s + %s' %
                            (sta, single_fit['Mw'], self.my_event.magnitude , self.individual_mw_threshold) )

                    # Call clean() so we remove the temp files from disk
                    results.clean()
                    continue

                # Track the zcor here so we can forced the value on the group inversion
                if sta in self.zcor:
                    results.zcor = self.zcor[sta]
                else:
                    results.zcor = single_fit['zcor'][sta]

                self.logging.debug('Set %s to %s' % ( sta, results.zcor))


                results.variance = single_fit['variance'][sta]
                results.mw = single_fit['Mw']

                # add to list of stations that we want to use
                good_stations[sta] = results

                self.logging.notify('Usign %s zcor:%s variance:%s' %
                        (sta, single_fit['zcor'][sta], single_fit['variance'][sta]) )


        # Let's redefine total_stations with the sorted list of "good" stations
        total_stations = sorted(good_stations.keys(), key=lambda x: float(good_stations[x].variance), reverse=True )


        #for sta in sorted(good_stations.keys(), key=lambda x: float(good_stations[x].variance), reverse=True ):

        self.logging.notify( 'Valid stations for inversion: %s ' % total_stations )

        while True:

            self.logging.debug('Loop for adding stations')

            if len(total_stations) and len(self.stations) < self.sta_max:
                # Bring new station from archive
                sta = total_stations.pop(0)
                #self.logging.info('Add new station to list [%s]' % sta)
                self.logging.notify('Inversion: adding %s zcor:%s variance:%s' %
                        (sta, good_stations[sta].zcor, good_stations[sta].variance) )

                self.stations[sta] = good_stations[sta]

                continue

            # Verify that we have min number of stations
            if len(self.stations) < self.sta_min:
                self.logging.error('NOT enough stations [%s] for this event. Need [%s]' % \
                        (len(self.stations), self.sta_min))


            # Run inversion
            self.logging.debug('determine_mt_solution()')
            self.results = self.my_inv.invert(self.stations)

            self.logging.debug(self.results)

            ## If recursive not set then stop here
            if not self.recursive: break

            # Verify that we have min number of stations
            if len(self.stations) <= self.sta_min: break


            ## Set to True in case nothing gets removed
            stop = True

            variance_list = sorted(self.results['variance'], key=lambda x: float(self.results['variance'][x]) )

            worst = variance_list[0]
            self.logging.info( 'Worst performer was %s with %s variance' % (worst, self.results['variance'][worst]) )

            if not self._good_fit(self.results['variance'][ worst ], self.min_variance):
                self.logging.warning('Remove %s for bad correlation of %s' % (worst, self.results['variance'][ worst ]) )
                self.stations[worst].clean()
                del self.stations[worst]
                stop = False

            if stop: break

        # Verify that we have min quality
        if int(self.results['Quality']) < int(self.min_quality):
            self.logging.warning('Quality[%s] Minimum required [%s]' % \
                    (self.results['Quality'], self.min_quality))
            return {}


        # Done with the inversion. Now set values for plotting results
        for sta in self.stations.keys():
            self.logging.debug('convert_synth( %s )' % sta)
            #self.stations[sta].convert_synth(self.results)
            self.stations[sta].convert_synth_original(self.results)

        if self.verbose: self.logging.notify(self.results)

        filename = plot_results( orid, self.stations, self.results,
                            self.my_event, folder=self.img_folder,
                            acknowledgement=self.acknowledgement )

        if self.verbose or self.debug: os.system( "open %s" % filename )


        return self.results

    def _good_fit(self, variance, min_variance=0.0):
        # Need to run several tests on the correlation value

        if variance != variance: return False
        if float(variance) == float('Inf'): return False
        if float(variance) == float('-Inf'): return False

        if float(variance) > 150.0: return False
        if float(variance) < float(min_variance): return False

        return True


    def add_station(self, sta):
        '''
        Method that will try to bring a new station to the process.
        We take care of pulling the data from the archive and the
        matching synthetics.
        '''

        results  = None

        # Skip this station if event is too close
        if self.my_event.distance(sta) < self.distance_min:
            self.logging.warning('%s too close from event [%s]km' % \
                    (sta,self.my_event.distance(sta)))
            return None

        # Skip this station if event is too far
        if self.my_event.distance(sta) > self.distance_max:
            self.logging.warning('%s too far from event [%s]km' % \
                    (sta,self.my_event.distance(sta)))
            return None

        # Skip this station if we want arrivals only
        if self.arrivals_only and not self.my_event.has_arrival(sta):
            self.logging.warning('%s Missing arrivals on database' % sta)
            return None


        # Calculate some values for station.
        # Then it's simpler to handle in the rest of the loop.
        distance = self.my_event.distance(sta)
        delta = self.my_event.delta(sta)
        seaz = self.my_event.seaz(sta)
        esaz = self.my_event.esaz(sta)
        ptime = self.my_event.ptime(sta)
        stime = self.my_event.stime(sta)
        time = self.my_event.time
        depth = self.my_event.depth

        '''
        Need to test some more if we can start the data extraction
        later in time. At this time we set this to 0 and we
        extract data starting at the event time.
        '''
        delay = 0.0
        #delay = int( ptime - time )
        #delay = int( stime - time )

        gf_delay = 0.0
        #gf_delay = float( ptime - time )
        #gf_delay = float( stime - time )


        # Get the waveforms for this station
        self.logging.debug( 'Get Waveforms for %s' % sta )
        real = self.my_data.get_waveforms( sta, self.chan_to_use, time, delay=delay, esaz=esaz,
                            seaz=seaz, tw=self.time_window, bw_filter=self.my_event.filter,
                            debug_plot=self.debug_each)

        # Verify that we have good information for this site
        if not real:
            self.logging.warning('Problems during get_waveforms() for %s' % sta )
            return results


        # Get synthetics for this station
        self.logging.debug( 'Get SYNTH for %s at %s km' % (sta, distance) )
        synth = self.my_synthetics.get_synth(depth=depth, distance=distance, delay=gf_delay,
                    tw=self.time_window, filter=self.my_event.filter, debug_plot=self.debug_each,
                    response=real.response)


        # Verify that we have good information for this site
        if not synth:
            self.logging.error('Problems during get_synth(%s,%s) for %s' % \
                    (distance, self.my_event.depth, sta) )
            return results


        # Save station-event metadata to my object
        results = Station(sta,self.tmp_folder)
        results.depth = self.my_event.depth
        results.time = self.my_event.time
        results.distance = self.my_event.distance(sta)
        results.realdistance = self.my_event.realdistance(sta)
        results.azimuth = self.my_event.esaz(sta)
        results.timewindow = self.time_window

        # If we have a zcor on the command-line for this site then use it
        #if sta in self.zcor: results.zcor = self.zcor[sta]

        # Save data to object
        results.real_data( real )
        results.synth_data( synth )

        # Plot final version of traces and synthetics
        #if self.debug_each:
        #    results.plot('real')
        #    results.plot('synth')

        return results





if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
