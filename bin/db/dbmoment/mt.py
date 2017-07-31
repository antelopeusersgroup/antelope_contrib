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
    for the inversion.
    '''

    def __init__( self, database=None, orid=None, options=None,
            inv=None, data=None, event=None, synth=None ):

        elog.debug( "DbMoment.__init__(%s)" % (database) )

        '''
        Load modules
        '''
        # Instantiate Origin Class
        try:
            elog.debug( 'event_obj.Origin(%s)' % database )
            self.my_event = event.Origin( database )
        except Exception,e:
            elog.error("EVENT module init error: [%s]" % e)

        # Instantiate Data Class
        try:
            elog.debug( 'data_obj.Data(%s,%s)' % (options.wave_db, options.allowed_segtype) )
            self.my_data = data.Waveforms(options.wave_db, options.allowed_segtype)
        except Exception,e:
            elog.error("DATA module init error: [%s]" % e)

        # Instantiate Synthetics Class
        try:
            elog.debug( 'synty_obj.Synthetics(%s,%s)' % (options.synth_db_folder, options.tmp_folder) )
            self.my_synth = synth.Synthetics( options.synth_db_folder, options.tmp_folder)
        except Exception,e:
            elog.error("Synthetics module Init Error: [%s]" % e)

        # Instantiate Inversion Classes. Dreger's code wrapper.
        try:
            elog.debug( 'inv_obj.MomentTensor()' )
            self.my_inv = inv.MomentTensor()
            pass
        except Exception,e:
            elog.error("Inversion Module Init Error: [%s]" % e)


        self.orid = orid
        self.model = None

        self.stations = {}
        self.verbose = options.verbose
        self.debug = options.debug

        self.debug_synth = options.debug_synth
        self.debug_real = options.debug_real

        self.tmp_folder = options.tmp_folder
        self.pf = open_verify_pf(options.pf)

        self.select = options.select
        self.reject = options.reject

        self.sta_min = int(options.sta_min)
        self.sta_max = int(options.sta_max)
        self.filter = options.filter
        self.depth_min = options.depth_min
        self.depth_max = options.depth_max
        self.recursive = options.recursive
        self.timewindow = int(options.timewindow)
        self.chan_to_use = options.chan_to_use
        self.min_quality = int(options.min_quality)
        self.arrivals_only = options.arrivals_only

        # Overwrites to PF files
        self.mindistance = options.mindistance
        self.maxdistance = options.maxdistance

        if options.min_variance:
            self.min_variance = float( options.min_variance )
        else:
            self.min_variance = 0.0


        # Extract event information from database
        elog.notify( "Get orid [%s]" % orid )
        self.my_event.get_origin( orid , self.select, self.reject)

        self.my_inv.set_depth( self.my_event.depth )
        self.my_inv.set_folder( self.tmp_folder )



    def mt(self,  model):
        '''
        Main method to control the inversion.
        Need to run with any origin from the database and will return all results
        for that inversion.
        '''

        self.model = model

        elog.debug( "mt(model=%s)" % (self.model) )

        self.my_synth.set_model( self.model )

        self.my_event.set_distance_step( self.model['distance_step'] )

        # Save short name of model in object
        self.my_event.model = self.my_synth.model_short_name


        ## Verify we have a valid event
        if self.my_event.depth >= self.depth_min and self.my_event.depth <= self.depth_max:
            elog.info("Event depth [%s] within our limits." % (self.my_event.depth))
        else:
            elog.error("Event depth [%s] out of limits.[%s,%s]" % \
                    (self.my_event.depth,self.depth_min,self.depth_max), 5)


        # sorted by distance
        total_stations = self.my_event.station_list()
        elog.info( 'Total list of stations: %s' % total_stations )

        for sta in total_stations:

            elog.debug('Test individual station [%s]' % sta)

            distance = self.my_event.distance(sta)

            if self.mindistance and distance < self.mindistance:
                elog.warning( '%s => %s too close for this config [min:%s]' % \
                        (sta, distance, self.mindistance)  )
                continue

            elif distance < int(self.model['distance_min']):
                elog.warning( '%s => %s too close for this model [min:%s]' % \
                        (sta, distance, self.model['distance_min'])  )
                continue

            if self.maxdistance and distance > self.maxdistance:
                elog.warning( '%s => %s too far for this config [max:%s]' % \
                        (sta, distance, self.maxdistance)  )
                continue
            elif distance > int(self.model['distance_max']):
                elog.warning( '%s => %s too far for this model [max:%s]' % \
                        (sta, distance, self.model['distance_max'])  )
                continue

            # Test different filters here.
            test_site = self.add_filtered_station( sta )

            if test_site:
                # Need to put the data into disk in the temporary folder
                #   ****** FILES ARE SAVED TO DISK ON THESE 2 LINES. ********
                elog.debug('Real traces saved to %s' % test_site.to_file('real') )
                elog.debug('Synthetic traces saved to %s' % test_site.to_file('synth') )

                # add to list of stations that we want to use
                self.stations[sta] = test_site

            else:
                elog.warning('No data for station %s' % sta )
                continue

        elog.info( 'Valid stations for inversion: %s ' % ', '.join(sorted(self.stations.keys())  ) )

        # Verify that we have min number of stations
        if len(self.stations.keys()) < self.sta_min:
            elog.error('NOT enough stations [%s] for this event. Need [%s]' % \
                    (len(self.stations), self.sta_min))


        # Maybe we have too many sites. Let's keep the best ones.
        while len(self.stations) > ( self.sta_max * 2 ):
            worst = sorted(self.stations.keys(),
                    key=lambda x: float( self.stations[x].vr ) )[0]
            elog.info('Too many stations. To be removed: [%s] [%s]' % \
                    (worst, self.stations[worst].vr) )

            del( self.stations[worst] )
            elog.info('Now we have [%s] sites' % len(self.stations) )


        '''
        We have list of all possible sites. Now invert.
        '''

        elog.debug('Loop for inversion and removal of stations')
        while True:


            elog.debug('determine_mt_solution()')
            self.results = self.my_inv.invert(self.stations)
            elog.notify('INVERSION: Quality:[%s] VR:[%0.1f]' % \
                    ( self.results['Quality'], self.results['VarRed']) )


            ## If recursive not set then stop here
            if not self.recursive:
                elog.debug('Not running recursive. Stop.')
                break

            # Verify number or sites in inversion
            if len(self.stations) <= self.sta_min:
                elog.debug('Minimum number of stations [%s]. Stop.' % len(self.stations) )
                break


            # jackknife variance calculation
            best_vr = None
            avoid_site = None
            keep_results = None
            worst_vr = None
            keep_site = None

            elog.debug('RecursiveTest: original station list: %s)' % self.stations.keys() )
            for test in sorted(self.stations.keys()):

                elog.debug('\tRecursiveTest: TEST ON REJECTING: %s)' % test )

                elog.debug('\tdetermine_mt_solution(RecursiveTest:%s)' % test)
                results = self.my_inv.invert(self.stations, ignore_sta=test )

                if not results:
                    worst_vr = 0.0
                    avoid_site = test
                    elog.info('\tRecursiveTest [%s] Q:[-] VR:[-]' % test )
                    continue

                elog.info('\tRecursiveTest [%s] Q:[%s] VR:[%0.1f]' % \
                        ( test, results['Quality'], results['VarRed']) )

                if not worst_vr or results['VarRed'] < worst_vr:
                    worst_vr = results['VarRed']
                    keep_site = test

                if not best_vr or results['VarRed'] > best_vr:
                    best_vr = results['VarRed']
                    avoid_site = test
                    keep_results = results

                elog.info('\tTEMP: Best contributor  [%s] ' % keep_site )
                elog.info('\tTEMP: Worst contributor [%s] ' % avoid_site)


            elog.info('\tBest Contributor  [%s] ' % keep_site )
            elog.info('\tWorst Contributor [%s] ' % avoid_site)

            elog.info('\tRecursiveTest Best option is avoiding [%s] for Q:[%s] VR:[%0.1f]' % \
                    ( avoid_site, keep_results['Quality'], keep_results['VarRed'] ) )


            # IF WE CAN INCREASE QUALITY SIGNIFICANTLY THEN DO IT!!!!

            # VERIFY IF THE VR IS BETTER THAN 1.2 TIMES THE ORIGINAL!!!!
            if self._good_fit(keep_results['VarRed'], self.min_variance) and \
                keep_results['VarRed'] > self.results['VarRed'] * 1.2:

                elog.notify('Remove %s to improve VR %s=>%0.1f' % \
                        (avoid_site, self.results['VarRed'], keep_results['VarRed']) )

                self.results = keep_results
                self.stations[avoid_site].clean()
                del( self.stations[ avoid_site ] )
                continue

            elif len( self.stations.keys() ) > self.sta_max:
                elog.notify('Remove %s. Too many sites. VR %s=>%0.1f' % \
                        (avoid_site, self.results['VarRed'], keep_results['VarRed']) )

                self.results = keep_results
                self.stations[avoid_site].clean()
                del( self.stations[ avoid_site ] )
                continue

            else:
                elog.notify( 'No significant improvement from RecursiveTest. Continue.' )



            # Might need to review final solution for individual VarRed
            # VERIFY INDIVIDUAL SITES IN THIS POSSIBLE SOLUTION
            if len( self.stations.keys() ) > self.sta_min:
                # only get here if no station was removed on last loop
                worst = sorted(self.results['variance'],
                        key=lambda x: float(self.results['variance'][x]) )[0]

                if self.results['variance'][ worst ] < self.min_variance:
                    elog.notify('Remove %s for bad VR:%0.1f' % (worst, self.results['variance'][ worst ]) )

                    self.results = keep_results
                    self.stations[worst].clean()
                    del self.stations[worst]
                    continue
                else:
                    elog.info('All stations above minimum variance reduction [%s]' % self.min_variance)



            # If nothing changed then stop here.
            break


        # Verify that we have minimum quality
        if int(self.results['Quality']) < int(self.min_quality):
            elog.warning('Quality[%s] Minimum required [%s]' % \
                    (self.results['Quality'], self.min_quality))
            elog.warning('NO RESULTS PRODUCED WITH VALID PARAMETERS.')
            return {}


        self.results['event'] = self.my_event
        self.results['stations'] = self.stations

        elog.debug(self.results)

        return self.results



    def _good_fit(self, test, min_val=0.0):
        # Need to run several tests on the correlation value

        if test != test: return False
        if float(test) == float('Inf'): return False
        if float(test) == float('-Inf'): return False

        if float(test) > 150.0: return False
        if float(test) < float(min_val): return False

        return True


    def add_filtered_station(self, sta):
        '''
        Method that will try to bring a new station to the process.
        We take care of pulling the data from the archive and the
        matching synthetics.
        Also verify which is the best filter for the site.
        '''

        results  = None

        # Skip this station if we want arrivals only
        if self.arrivals_only and not self.my_event.has_arrival(sta):
            elog.warning('%s No arrivals on database. Skip station.' % sta)
            return None


        # Calculate some values for station.
        # Then it's simpler to handle in the rest of the loop.
        distance = self.my_event.distance(sta)
        seaz = self.my_event.seaz(sta)
        esaz = self.my_event.esaz(sta)
        time = self.my_event.time
        depth = self.my_event.depth

        delay = 0.0
        gf_delay = delay


        # Get the waveforms for this station
        elog.debug( 'Get Waveforms for %s' % sta )
        real = self.my_data.get_waveforms( sta, self.chan_to_use, time, delay=delay, esaz=esaz,
                            seaz=seaz, tw=self.timewindow, filters=self.filter,
                            bw_filter=self.my_event.filter,
                            debug_plot=self.debug_real)

        # Verify that we have good information for this site
        if not real:
            elog.warning('Problems during get_waveforms() for %s' % sta )
            return None

        synth = {}
        for fltr in self.filter:
            # Get synthetics for this station
            elog.debug( '%s Get SYNTH at %s km and %s filter' % \
                    (sta, distance, fltr) )
            synth[ fltr ] = self.my_synth.get_synth(depth=depth,
                    distance=distance, delay=gf_delay, tw=self.timewindow,
                    filter=fltr, response=real[fltr].response,
                    debug_plot=self.debug_synth)


            # Verify that we have good synthetics for this site
            if not fltr in synth or not synth[ fltr ]:
                elog.warning('%s Problems during get_synth(depth=%s,distance=%s)' % \
                        (sta, depth, distance) )
                return None



        # Save station-event metadata to my object
        results = Station(sta,self.tmp_folder)
        results.depth = self.my_event.depth
        results.time = self.my_event.time
        results.distance = self.my_event.distance(sta)
        results.realdistance = self.my_event.realdistance(sta)
        results.azimuth = self.my_event.esaz(sta)
        results.timewindow = self.timewindow


        # Verify which filter provides the best
        # variance reduction
        variance_results = {}
        zcor_results = {}
        for fltr in self.filter:

            elog.debug('%s Test VR for [%s]' % (sta,fltr) )

            # Save temp data to object
            results.real_data( real[fltr] )
            results.synth_data( synth[fltr] )

            # Save traces to tmp files
            file_for_real = results.to_file('real')
            file_for_synth = results.to_file('synth')

            elog.debug('TEMP: Real traces saved to %s' % file_for_real )
            elog.debug('TEMP: Synthetic traces saved to %s' % file_for_synth )


            # We have the information for this station. Let's try the fit alone.
            try:
                temp = self.my_inv.invert( {sta:results} )
                variance_results[ fltr ] = temp['variance'][sta]
                zcor_results[ fltr ] = temp['zcor'][sta]

                elog.debug( 'TEMP: VR:%s ZCOR:%s Filter:[%s]' % \
                        (variance_results[ fltr ], zcor_results[ fltr ], fltr) )
            except Exception,e:
                elog.warning('%s %s' % (Exception,e) )
                elog.error('Invertion on {0} failed!'.format(sta) )

            results.clean()

        elog.debug( variance_results )
        elog.debug( zcor_results )

        if not variance_results:
                elog.warning('No VR results for station {0}!'.format(sta) )
                return None


        best = sorted(variance_results.items(), key=operator.itemgetter(1))[-1]
        best_filter = best[0]
        best_vr = best[1]
        best_zcor = zcor_results[ best_filter]

        elog.info( '%s Best VR is [%0.1f] for [%s]' % ( sta, best_vr, best_filter)  )

        if best_vr < self.min_variance:
            elog.warning( '%s Best VR [%0.1f] lower than allowed minimal [%s]' % \
                    (sta, best_vr, self.min_variance) )
            return None

        # Save data to object
        results.real_data( real[best_filter] )
        results.synth_data( synth[best_filter] )
        results.filter = best_filter
        results.zcor = best_zcor
        results.vr = best_vr

        return results





if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
