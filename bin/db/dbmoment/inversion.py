#   Copyright (c) 2016 Boulder Real Time Technologies, Inc.
#
#   Written by Juan Reyes
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.


from __main__ import *      # Get all the libraries from parent

class MomentTensor():
    """
    Class for building moment tensors and doing the inversion

    """

    def __init__(self, depth=0, tmp_folder='/tmp/dbmoment'):
        """Initialize"""

        self.station_cache = {}
        self.tmp_folder = tmp_folder
        self.depth = depth
        self.results = {
            'zcor':{},
            'variance':{},
            'Strike':[],
            'Rake':[],
            'Dip':[]
        }

    def set_depth(self, depth):
        self.depth = depth

    def set_folder(self, folder):
        self.tmp_folder = folder

    def invert(self, sta_cache, ignore_sta=None):
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
        destination = '%s/mt_inv.in' % self.tmp_folder
        green = ''
        data = ''
        total = 0
        valid = False

        for sta in self.sta_cache:
            if not self.sta_cache[sta]: continue
            if ignore_sta and sta == ignore_sta : continue

            self.station_cache["%s" % total] = sta
            total += 1

            elog.debug('Work on station: %s' % sta)
            elog.debug('\tData: %s' % self.sta_cache[sta].real_file)
            elog.debug('\tSynth: %s' % self.sta_cache[sta].synth_file)
            real_file = self.sta_cache[sta].real_file_name
            synth_file = self.sta_cache[sta].synth_file_name

            real_samples = self.sta_cache[sta].real_samples
            synth_samples = self.sta_cache[sta].synth_samples

            distance = float(self.sta_cache[sta].realdistance)
            azimuth = float(self.sta_cache[sta].azimuth)
            zcor = int(float(self.sta_cache[sta].zcor))

            data += "%s %s %s %s %s\n" % ( real_file, distance, azimuth, zcor, real_samples)
            green += "%s 0 %s\n" % ( synth_file, synth_samples)

        output = '%s %s 1 6 0\n' % ( total, self.depth )
        output += data
        output += green

        try:
            # Remove file if present
            if os.path.exists(destination):
                    os.remove(destination)
        except:
            elog.error('Cannot remove previous copy of %s %s'% (destination,e))

        try:
            f = open(destination, 'w')
            f.write(output)
            f.close()
        except Exception,e:
            elog.error('Cannot create new file %s %s'% (destination,e))

        elog.debug('new file %s' % destination)

        #elog.debug(output)

        try:
            os.remove( "%s/mt_inv.out" % self.tmp_folder )
        except Exception,e:
            pass

        try:
            os.remove( "%s/tdmt.results" % self.tmp_folder )
        except Exception,e:
            pass

        # Clean variables
        self.results = {
            'zcor':{},
            'variance':{},
            'Strike':[],
            'Rake':[],
            'Dip':[]
        }

        for line in run('tdmt_inv', self.tmp_folder):
            valid = True
            elog.debug(line)
            match_variance = re.match("^Station\((.+)\)=(\S+) +(\S+)$",line)
            if match_variance:
                sta = self.station_cache[match_variance.group(1)]
                elog.debug('Got station %s =%s %s' % \
                        (sta,match_variance.group(2),match_variance.group(3)) )
                self.results['variance'][sta] = float(match_variance.group(2))
                elog.debug('station variance reduction %s' % \
                                    self.results['variance'][sta])


        if not valid:
            return False

        outputfile = open( "%s/mt_inv.out" % self.tmp_folder )
        self._parse_output(outputfile.read())
        outputfile.close()

        return self.results

    def _parse_output(self, string ):
        """ After running the command-line
        code we need to extract the results
        from the log.

        """
        elog.debug('Raw inversion output:')
        #elog.debug( '\n' + string )

        elog.debug('Parse output:')

        for line in string.split('\n'):

            line = line.strip()
            #elog.debug(line )

            match_zcor = re.match("^Station\((.+)\):.*Zcor=(.+)$",line)
            #match_variance = re.match("^Station\((.+)\)=(\d+) +(\d+)$",line)
            strike = re.match("^Strike=(-?\d+) +; +(-?\d+)$",line)
            rake = re.match("^Rake=(-?\d+) +; +(-?\d+)$",line)
            dip = re.match("^Dip=(-?\d+) +; +(-?\d+)$",line)
            varred = re.match("^VarRed=(.+)$",line)
            quality = re.match("^Quality=(.+)$",line)
            mw = re.match("^Mw=(.+)$",line)
            if match_zcor :
                sta = self.station_cache[match_zcor.group(1)]
                elog.debug('Got station %s zcor=%s' % \
                        (sta,match_zcor.group(2)) )
                self.results['zcor'][sta] = int(float(match_zcor.group(2)))
            #elif match_variance:
            #    sta = self.station_cache[match_variance.group(1)]
            #    elog.debug('Got station %s =%s %s' % \
            #            (sta,match_variance.group(2),match_variance.group(3)) )
            #    self.results['variance'][sta] = match_variance.group(2)
            elif strike:
                elog.debug('Got strike %s %s' % (strike.group(1),strike.group(2) ))
                self.results['Strike'] = [ int(strike.group(1)), int(strike.group(2)) ]
            elif rake:
                elog.debug('Got rake %s %s' % (rake.group(1),rake.group(2) ))
                self.results['Rake'] = [ int(rake.group(1)), int(rake.group(2)) ]
            elif dip:
                elog.debug('Got dip %s %s' % (dip.group(1),dip.group(2) ))
                self.results['Dip'] = [ int(dip.group(1)), int(dip.group(2)) ]
            elif varred:
                elog.debug('Got VarRed %s' % varred.group(1))
                self.results['VarRed'] = float(varred.group(1))
            elif quality:
                elog.debug('Got Quality %s' % quality.group(1))
                self.results['Quality'] = int(quality.group(1))
            elif mw:
                elog.debug('Got Mw %s' % mw.group(1))
                self.results['Mw'] = float(mw.group(1))
            else:
                parts = line.split('=')
                if len(parts) == 2:
                    key = parts[0].strip()
                    value = parts[1].strip()
                    self.results[ key ] = value
                    elog.debug('New value: [%s]:[%s]' % (key,value) )
                    #if key in self.results:
                    #    elog.debug('New value: [%s]:[%s]' % (key,value) )
                    #    self.results[ key ] = value
                    #else:
                    #    elog.warning('UNKNOWN value: [%s]:[%s]' % (key,value) )

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
        #self.results['Mtp'] =  float(self.results['Mxy'])
        self.results['Mrt'] = float(self.results['Mxz'])
        self.results['Mpp'] = float(self.results['Myy'])
        self.results['Mrp'] = -1 * float(self.results['Myz'])
        #self.results['Mrp'] =  float(self.results['Myz'])
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


        # mt table apparently uses N-m whereas Dregers code really
        # looks like dyne-cm so we will have to divide it by 10^7
        #self.results['Mo'] = float(self.results['Mo']) / 10**7
        # Update scm instead
        self.results['scm'] = self.results['Mo']
        self.results['scm'] = float(self.results['scm']) / 10**7


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
        self.results['estatus'] = "Quality: %s" % self.results['Quality']


if __name__ == "__main__": raise ImportError( "\n\n\tAntelope's dbmoment module. Not to run directly!!!! **\n" )
