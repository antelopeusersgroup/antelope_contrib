from __main__ import *      # Get all the libraries from parent

class MomentTensor():
    """
    Class for building moment tensors and doing the inversion

    """

    def __init__(self, depth, tmp_folder='/tmp/dbmoment'):
        """Initialize"""
        self.station_cache = {}
        self.event_cache = None
        self.tmp_folder = tmp_folder
        self.depth = depth
        self.results = {
            'Depth':None,
            'zcor':{},
            'acc':{},
            'Mxx':None,
            'Mxy':None,
            'Mxz':None,
            'Myy':None,
            'Myz':None,
            'Mzz':None,
            'Mo':None,
            'Mw':None,
            'Strike':[],
            'Rake':[],
            'Dip':[],
            'Pdc':None,
            'Pclvd':None,
            'Piso':None,
            'Variance':None,
            'VarRed':None,
            'Var/Pdc':None,
            'Quality':None
        }

    def invert(self, sta_cache, event_cache):
        """
        To run the original code from Dreger's we need to
        prepare a file like this...

        3 8 1 1
        testdata1 100. 10. 0 120
        testdata2 100. 40. 0 120
        testdata3 100. 50. 0 120
        socal100d8 0 120
        socal100d8 0 120
        socal100d8 0 120

        <number of 3-component stations, source depth, distance weighting flag, plotting flag>
        <data_filename, distance (km), azimuth (deg from north), sample-offset (Zcor), number_of_samples>
        <ditto for station 2>
        <ditto for station 3>
        <filtered GF_filename, zero-offset (always zero), number_of_samples (same as corresponding data>
        <ditto for station 2>
        <ditto for station 3>
        """

        self.sta_cache = sta_cache
        self.event_cache = event_cache
        destination = '%s/mt_inv.in' % self.tmp_folder
        green = ''
        data = ''
        total = 0

        for sta in self.sta_cache:
            self.station_cache["%s" % total] = sta
            total += 1

            log('Work on station: %s' % sta)
            log('\tData: %s' % self.sta_cache[sta].real_file )
            log('\tSynth: %s' % self.sta_cache[sta].synth_file )
            real_file = self.sta_cache[sta].real_file
            synth_file = self.sta_cache[sta].synth_file
            distance = int(float(self.sta_cache[sta].distance))
            azimuth = int(float(self.sta_cache[sta].azimuth))

            data += "%s %s %s 0 120\n" % ( real_file, distance, azimuth)
            green += "%s 0 120\n" % ( synth_file)

        output = '%s %s 1 0\n' % ( total, self.depth )
        output += data
        output += green

        try:
            f = open(destination, 'w')
            f.write(output)
            f.close()
        except Exception,e:
            error('Cannot create new file %s %s'% (destination,e))

        log('new file %s' % destination)


        #try:
        #    os.chdir(self.tmp_folder)
        #except Exception,e:
        #    error('Cannot change directory to [%s]' % self.tmp_folder)

        try:
            os.remove( "%s/mt_inv_redi.out" % self.tmp_folder )
        except Exception,e:
            pass

        cmd = 'tdmt_invc'
        for line in run(cmd, self.tmp_folder):
            debug(line)
            match_acc = re.match("^Station\((.+)\)=(\S+) +(\S+)$",line)
            if match_acc:
                sta = self.station_cache[match_acc.group(1)]
                log('Got station %s =%s %s' % \
                        (sta,match_acc.group(2),match_acc.group(3)) )
                self.results['acc'][sta] = match_acc.group(2)

        outputfile = open( "%s/mt_inv_redi.out" % self.tmp_folder )
        self._parse_output(outputfile.read())
        outputfile.close()

        return self.results

    def _parse_output(self, string ):
        """ After running the command-line
        code we need to extract the results
        from the log.

        Exampel:
        Depth=8
        Station Information
        Station(0): datasta2  R=100.0km  AZI=0.0  W=1.000  Zcor=5
        Station(1): datasta3  R=100.0km  AZI=0.0  W=1.000  Zcor=0
        Station(2): datasta1  R=100.0km  AZI=0.0  W=1.000  Zcor=5
        Mo=7.56446e+19
        Mw=2.6
        Strike=352 ; 258
        Rake=58 ; 175
        Dip=88; 33
        Pdc=93
        Pclvd=7
        Piso=0
        Station(0)=44.824974  1.7449e-11
        Station(1)=-44.040012  1.35435e-11
        Station(2)=63.666492  2.93047e-11
        VAR=3.70419e-14
        VR=34.02  (UNWEIGHTED)
        VR=34.02  (WEIGHTED)
        Var/Pdc=3.981e-16
        Quality=1

        """


        for line in string.split('\n'):

            line = line.strip()
            debug('line: [%s]' % line )

            match_zcor = re.match("^Station\((.+)\):.*Zcor=(.+)$",line)
            #match_acc = re.match("^Station\((.+)\)=(\d+) +(\d+)$",line)
            strike = re.match("^Strike=(-?\d+) +; +(-?\d+)$",line)
            rake = re.match("^Rake=(-?\d+) +; +(-?\d+)$",line)
            dip = re.match("^Dip=(-?\d+) +; +(-?\d+)$",line)
            if match_zcor :
                sta = self.station_cache[match_zcor.group(1)]
                log('Got station %s zcor=%s' % \
                        (sta,match_zcor.group(2)) )
                self.results['zcor'][sta] = match_zcor.group(2)
            #elif match_acc:
            #    sta = self.station_cache[match_acc.group(1)]
            #    log('Got station %s =%s %s' % \
            #            (sta,match_acc.group(2),match_acc.group(3)) )
            #    self.results['acc'][sta] = match_acc.group(2)
            elif strike:
                log('Got strike %s %s' % (strike.group(1),strike.group(2) ))
                self.results['Strike'] = [ strike.group(1), strike.group(2) ]
            elif rake:
                log('Got rake %s %s' % (rake.group(1),rake.group(2) ))
                self.results['Rake'] = [ rake.group(1), rake.group(2) ]
            elif dip:
                log('Got dip %s %s' % (dip.group(1),dip.group(2) ))
                self.results['Dip'] = [ dip.group(1), dip.group(2) ]
            else:
                parts = line.split('=')
                if len(parts) == 2:
                    key = parts[0].strip()
                    value = parts[1].strip()
                    if key in self.results:
                        log('New value: [%s]:[%s]' % (key,value) )
                        self.results[ key ] = value
                    else:
                        warning('UNKNOWN value: [%s]:[%s]' % (key,value) )

        # Source parameters and moment-tensor solutions
        #Author: Gunter Bock, Formerly GeoForschungsZentrum Potsdam, Germany
        #Version: September 2002; DOI: 10.2312/GFZ.NMSOP-2_IS_3.8
        #
        # The 6 independent moment-tensor elements in
        # the (x, y, z) = (north, east, down) coordinate
        # system are related to the components in (r;Theta;Phi) by
        #       Mrr = Mzz
        #       Mtt = Mxx
        #       Mpp = Myy
        #       Mrt = Mzx
        #       Mrp = -Mzy
        #       Mtp = -Mxy
        self.results['Mtt'] = float(self.results['Mxx'])
        self.results['Mtp'] = -1 * float(self.results['Mxy'])
        self.results['Mrt'] = float(self.results['Mxz'])
        self.results['Mpp'] = float(self.results['Myy'])
        self.results['Mrp'] = -1 * float(self.results['Myz'])
        self.results['Mrr'] = float(self.results['Mzz'])


        # Fix Pdc to 0-1 range
        self.results['Pdc'] = float(self.results['Pdc']) / 100
        self.results['Pclvd'] = float(self.results['Pclvd']) / 100

        # The output parameters VAR, VR, Var/Pdc and Quality are used
        # to gauge the success of the inversion. VAR is the overall
        # variance estimate, VR is the variance reduction (both unweighted
        # and distance weighted estimates), Var/Pdc is the ratio of the
        # variance to the percent double couple, quality is a subjective
        # measure where 4 is the best and 1 is the worst. The higher the
        # value of VR the better the solution. The Var/Pdc measure can also
        # be very useful for areas where non-double-couple solutions are not expected.


        # Fields on Antelope MT table
        # fields = [ "pubid", "qmlid", "tmpp", "tmrp", "tmrr", "tmrt", "tmtp",
        #     "tmtt", "taxlength", "taxplg", "taxazm", "paxlength", "paxplg", "paxazm",
        #     "naxlength", "naxplg", "naxazm", "scm", "pdc", "str1", "dip1", "rake1", "str2",
        #     "dip2", "rake2", "drdepth", "drtime", "drlat", "drlon", "drmag", "drmagt",
        #     "estatus", "rstatus", "utime", "auth"]

        self.results['drlat'] = self.event_cache.lat
        self.results['drlon'] = self.event_cache.lon
        self.results['drtime'] = self.event_cache.time

        # mt table apparently uses N-m whereas Dregers code really
        # looks like dyne-cm so you will have to divide it by 10^7
        #self.results['Mo'] = float(self.results['Mo']) / 10**7
        # Update scm instead
        self.results['scm'] = self.results['Mo']
        self.results['scm'] = float(self.results['scm']) / 10**7


        #self.results['auth'] = self.results['Pdc']
        self.results['pdc'] = self.results['Pdc']
        self.results['tmtt'] = self.results['Mtt'] * self.results['scm']
        self.results['tmtp'] = self.results['Mtp'] * self.results['scm']
        self.results['tmrt'] = self.results['Mrt'] * self.results['scm']
        self.results['tmpp'] = self.results['Mpp'] * self.results['scm']
        self.results['tmrp'] = self.results['Mrp'] * self.results['scm']
        self.results['tmrr'] = self.results['Mrr'] * self.results['scm']
        self.results['dip1'] = self.results['Dip'][0]
        self.results['dip2'] = self.results['Dip'][1]
        self.results['str1'] = self.results['Strike'][0]
        self.results['str2'] = self.results['Strike'][1]
        self.results['rake1'] = self.results['Rake'][0]
        self.results['rake2'] = self.results['Rake'][1]
        self.results['drmag'] = self.results['Mw']
        self.results['drmagt'] = 'Mw'
        self.results['drdepth'] = self.results['Depth']
        self.results['estatus'] = "Quality: %s" % self.results['Quality']




if __name__ == "__main__":
    """ If we call this script directly, then output error  """

    print "\n\t** Not to run directly!!!! **\n"
    print "\n\nNo BRTT support."
    print "Juan Reyes <reyes@ucsd.edu>\n\n"
