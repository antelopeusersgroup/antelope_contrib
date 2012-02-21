#
#   Copyright (c) 2007-2010 Lindquist Consulting, Inc.
#   All rights reserved. 
#                                                                     
#   Written by Dr. Kent Lindquist, Lindquist Consulting, Inc. 
#
#   This software is licensed under the New BSD license: 
#
#   Redistribution and use in source and binary forms,
#   with or without modification, are permitted provided
#   that the following conditions are met:
#   
#   * Redistributions of source code must retain the above
#   copyright notice, this list of conditions and the
#   following disclaimer.
#   
#   * Redistributions in binary form must reproduce the
#   above copyright notice, this list of conditions and
#   the following disclaimer in the documentation and/or
#   other materials provided with the distribution.
#   
#   * Neither the name of Lindquist Consulting, Inc. nor
#   the names of its contributors may be used to endorse
#   or promote products derived from this software without
#   specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
#   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
#   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
#   THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
#   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#   IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#

import _orb
import stock

from stock import ElogException, ElogLog, ElogNotify, ElogComplain, ElogDie


for _key in _orb._constants:
    exec( "%s = _orb._constants['%s']" % (_key, _key) )


class Orb(object):
    """Create an Antelope Orb connection
        
        Orb(orbname)
        Orb(orbname, perm)
        Orb(orbname=address)
        Orb(orbname=address, perm='r')
    """
    
    def __init__(self, *args, **kwargs):

        self._orbname = None
        self._orbfd = -1
        self._perm = 'r'
    
        if(kwargs.has_key('orbname')):

            self._orbname = kwargs['orbname']

        if(kwargs.has_key('perm')):

            self._perm = kwargs['perm']

        if(len(args) >= 1):

            if(isinstance(args[0], str)):

                self._orbname = args[0]

            else:

                stock.elog_complain('Orb constructor arguments not understood')

        if(len(args) >= 2):

            if(isinstance(args[1], str)):

                self._perm = args[1]

            else:

                stock.elog_complain('Orb constructor arguments not understood')
        
        if(self._orbname and not isinstance(self._orbname, str)):
            
            raise TypeError, 'dbname must be a string'

        if(not isinstance(self._perm, str)):
            
            raise TypeError, 'perm must be a string'

        if(self._orbname):

	    try:

                self._orbfd = _orb._orbopen(self._orbname, self._perm)
            
	    except _orb._ElogException, _e: 

                stock._raise_elog(_e)

            if(self._orbfd < 0):

                stock.elog_complain('Failure opening orbserver "%s"' % self._orbname)

    def __str__(self):
        
        return ("\n[Orb:\n" +
            "\torbfd = %d\n" % self._orbfd +
            "\torbname = %s\n" % self._orbname +
            "]\n")

    def close(self):
        """Close an Antelope orb connection"""

	try:

            _orb._orbclose(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

    def ping(self):
        """Query orbserver version"""

	try:

            ret = _orb._orbping(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def tell(self):
        """Query orb read-head position"""

	try:

            ret = _orb._orbtell(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def select(self, match):
        """Match orb source-names"""

	try:

            ret = _orb._orbselect(self._orbfd, match)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def reject(self, reject):
        """Reject orb source names"""

	try:

            ret = _orb._orbreject(self._orbfd, reject)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def position(self, where):
        """Position orb connection packet pointer by time or code"""

	try:

            ret = _orb._orbposition(self._orbfd, where)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def seek(self, whichpkt):
        """Position orb connection packet pointer by pktid or code"""

	try:

            ret = _orb._orbseek(self._orbfd, whichpkt)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def after(self, time):
        """Position orb connection packet pointer by epoch time"""

	try:

            ret = _orb._orbafter(self._orbfd, time)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def reap(self):
        """Get the next packet from an orb"""

	try:

            ret = _orb._orbreap(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def reap_timeout(self, maxseconds):
        """Get the next packet from an orb, waiting a maximum number of seconds"""

	try:

            ret = _orb._orbreap_timeout(self._orbfd, maxseconds)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def get(self, whichpkt):
        """Get a specified packet from an orb"""

	try:

            ret = _orb._orbget(self._orbfd, whichpkt)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def put(self, srcname, time, packet, nbytes):
        """Put a packet on an orb"""

	try:

            ret = _orb._orbput(self._orbfd, srcname, time, packet, nbytes)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def lag(self, match = None, reject = None):
        """"Return parameters indicating degree to which clients are behind"""

	try:

            ret = _orb._orblag(self._orbfd, match, reject)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def stat(self):
        """"Return parameters about orb status"""

	try:

            ret = _orb._orbstat(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def sources(self):
        """"Return information on orb data-streams (source names)"""

	try:

            ret = _orb._orbsources(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def clients(self):
        """"Return information on orb clients"""

	try:

            ret = _orb._orbclients(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def putx(self, srcname, time, packet, nbytes):
        """Put a packet on an orb, returning the pktid of the output packet"""

	try:

            ret = _orb._orbputx(self._orbfd, srcname, time, packet, nbytes)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def resurrect(self):
        """restores previous orb position variables"""

	try:

            ret = _orb._orbresurrect(self._orbfd)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret

    def bury(self, pktid, pkttime):
        """Save orb position variables"""

	try:

            ret = _orb._orbbury(self._orbfd, pktid, pkttime)

	except _orb._ElogException, _e: 

            stock._raise_elog(_e)

        return ret


def orbopen(orbname, perm = 'r'):
    """Open an Antelope orb connection"""

    return Orb(orbname, perm)


def orbclose(orb):
    """Close an Antelope orb connection"""

    orb.close()

    return 


def orbping(orb):
    """Query orbserver version"""

    return orb.ping()


def orbtell(orb):
    """Query current connection read-head position"""

    return orb.tell()


def orbselect(orb, match):
    """Match orb source names"""

    return orb.select( match )


def orbreject(orb, reject):
    """Reject orb source names"""

    return orb.reject( reject )


def orbposition(orb, where):
    """Position orb connection packet pointer by time or code"""

    return orb.position( where )


def orbseek(orb, whichpkt):
    """Position orb connection packet pointer by pktid or code"""

    return orb.seek( whichpkt )


def orbafter(orb, time):
    """Position orb connection packet pointer by epoch time"""

    return orb.after( time )


def orbreap(orb):
    """Get the next packet from an orb"""

    return orb.reap()


def orbreap_timeout(orb, maxseconds):
    """Get the next packet from an orb, waiting a maximum number of seconds"""

    return orb.reap_timeout(maxseconds)


def orbget(orb, whichpkt):
    """Get a specified packet from an orb"""

    return orb.get(whichpkt)


def orbput(orb, srcname, time, packet, nbytes):
    """Put a packet on an orb"""

    return orb.put(srcname, time, packet, nbytes)


def orbputx(orb, srcname, time, packet, nbytes):
    """Put a packet on an orb, returning the pktid of the output packet"""

    return orb.putx(srcname, time, packet, nbytes)


def orblag(orb, match = None, reject = None):
    """"Return parameters indicating degree to which clients are behind"""

    return orb.lag(match, reject)


def orbstat(orb):
    """"Return various status parameters about the orbserver"""

    return orb.stat()


def orbsources(orb):
    """"Return information on orb data streams (source-names)"""

    return orb.sources()


def orbclients(orb):
    """"Return information on orb clients""" 

    return orb.clients()


def orbresurrect(orb):
    """Restore previous orb position variables"""

    return orb.resurrect()


def orbbury(orb, pktid, pkttime):
    """Save orb tracking variables in state file"""

    return orb.bury(pktid, pkttime)


def orbexhume(filename):
    """Read and initiate a statefile for orb tracking"""

    try:

        ret = _orb._orbexhume(filename)

    except _orb._ElogException, _e: 

        stock._raise_elog(_e)

    return ret


def orbpkt_string(srcname, time, packet, nbytes):
    """Convert an orb packet to string representation"""

    try:

        ret = _orb._orbpkt_string(srcname, time, packet, nbytes)

    except _orb._ElogException, _e: 

        stock._raise_elog(_e)

    return ret


if __name__ == '__main__':
    import unittest
    import os
    orbname = ':dq'

    class Testorb_fixture(object):

        tempdir = '/tmp/python_orbtest_' + str(os.getuid()) + str(os.getpid())

        def start(self):

            os.mkdir(self.tempdir)
            os.chdir(self.tempdir)
            os.system("pfcp orbserver .")
            os.system("orbserver -p " + orbname + " orbserver &")
            os.system("sleep 3")
            os.system( "pf2orb rtexec " + orbname )

        def stop(self):

            os.system("echo halt | orbstat -i " + orbname)
            os.system("sleep 5")
            os.system("/bin/rm -f " + self.tempdir + "/orb/*")
            os.rmdir(self.tempdir + "/orb")
            os.system("/bin/rm -f " + self.tempdir + "/*")
            os.rmdir(self.tempdir)

    class Testorb(unittest.TestCase):

        def test_Orb_constructor(self):

            orb = Orb(orbname)

            self.assertRaises(ElogComplain, Orb, 'not an orb')
            
        def test_procedure_orbopen(self):

            orb = orbopen(orbname, 'r')

        def test_procedure_orbclose(self):

            orb = orbopen(orbname, 'r')

            orbclose(orb)

        def test_procedure_orbping(self):

            orb = orbopen(orbname, 'r')

            version = orbping(orb)

            self.assertTrue(version > 0)

            orbclose(orb)

        def test_procedure_orbtell(self):

            orb = orbopen(orbname, 'r')

            pktid = orbtell(orb)

            self.assertTrue(isinstance(pktid,int))

            orbclose(orb)

        def test_procedure_orbselect(self):

            orb = orbopen(orbname, 'r')

            n = orbselect(orb, ".*")

            self.assertTrue(n >= 0)

            orbclose(orb)

        def test_procedure_orbreject(self):

            orb = orbopen(orbname, 'r')

            n = orbreject(orb, ".*")

            self.assertTrue(n >= 0)

            orbclose(orb)

        def test_procedure_orbposition(self):

            orb = orbopen(orbname, 'r')

            pktid = orbposition(orb, "oldest")

            self.assertTrue(pktid >= 0)

            orbclose(orb)

        def test_procedure_orbreap(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbreap(orb)

            self.assertTrue(isinstance(pktid, int))
            self.assertTrue(isinstance(srcname, str))
            self.assertTrue(isinstance(time, float))
            self.assertTrue(isinstance(packet, str))
            self.assertTrue(isinstance(nbytes, int))

            orbclose(orb)

        def test_procedure_orbreap_timeout(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbreap_timeout(orb, 1)

            if( isinstance(pktid, int) ):
                self.assertTrue(isinstance(srcname, str))
                self.assertTrue(isinstance(time, float))
                self.assertTrue(isinstance(packet, str))
                self.assertTrue(isinstance(nbytes, int))

            orbclose(orb)

        def test_procedure_orbget(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbget(orb, ORBNEWEST)

            self.assertTrue(isinstance(pktid, int))
            self.assertTrue(isinstance(srcname, str))
            self.assertTrue(isinstance(time, float))
            self.assertTrue(isinstance(packet, str))
            self.assertTrue(isinstance(nbytes, int))

            orbclose(orb)

        def test_procedure_orbput(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbget(orb, ORBNEWEST)

            time += 1

            rc = orbput(orb, srcname, time, packet, nbytes )

            self.assertTrue(rc == 0)

            orbclose(orb)

        def test_procedure_orbputx(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbget(orb, ORBNEWEST)

            time += 1

            rc = orbputx(orb, srcname, time, packet, nbytes )

            self.assertTrue(rc > 0)

            orbclose(orb)

        def test_procedure_orbseek(self):

            orb = orbopen(orbname, 'r')

            pktid = orbseek(orb, ORBOLDEST)

            self.assertTrue(pktid >= 0)

            orbclose(orb)

        def test_procedure_orbafter(self):

            orb = orbopen(orbname, 'r')

            pktid = orbafter(orb, 631152000)

            self.assertTrue(pktid >= 0)

            orbclose(orb)

        def test_procedure_orbpkt_string(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orbget(orb, ORBNEWEST)

            packet_string = orbpkt_string(srcname, time, packet, nbytes)

            self.assertTrue(isinstance(packet_string, str))

            orbclose(orb)

        def test_procedure_orbstat(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            stat = orbstat( orb )

            self.assertTrue(isinstance(stat['when'], float))
            self.assertTrue(isinstance(stat['started'], float))
            self.assertTrue(isinstance(stat['orb_start'], float))
            self.assertTrue(isinstance(stat['connections'], int))
            self.assertTrue(isinstance(stat['messages'], int))
            self.assertTrue(isinstance(stat['maxdata'], int))
            self.assertTrue(isinstance(stat['errors'], int))
            self.assertTrue(isinstance(stat['rejected'], int))
            self.assertTrue(isinstance(stat['closes'], int))
            self.assertTrue(isinstance(stat['opens'], int))
            self.assertTrue(isinstance(stat['port'], int))
            self.assertTrue(isinstance(stat['address'], str))
            self.assertTrue(isinstance(stat['pid'], int))
            self.assertTrue(isinstance(stat['nsources'], int))
            self.assertTrue(isinstance(stat['nclients'], int))
            self.assertTrue(isinstance(stat['maxsrc'], int))
            self.assertTrue(isinstance(stat['maxpkts'], int))
            self.assertTrue(isinstance(stat['version'], str))
            self.assertTrue(isinstance(stat['who'], str))
            self.assertTrue(isinstance(stat['host'], str))

            orbclose(orb)

        def test_procedure_orbsources(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            (when, sources) = orbsources( orb )

            self.assertTrue(isinstance(when, float))

            if( len(sources) > 0 ):

                self.assertTrue(isinstance(sources[0]['srcname'], str))
                self.assertTrue(isinstance(sources[0]['active'], int))
                self.assertTrue(isinstance(sources[0]['soldest'], int))
                self.assertTrue(isinstance(sources[0]['slatest'], int))
                self.assertTrue(isinstance(sources[0]['npkts'], int))
                self.assertTrue(isinstance(sources[0]['nbytes'], int))
                self.assertTrue(isinstance(sources[0]['soldest_time'], float))
                self.assertTrue(isinstance(sources[0]['slatest_time'], float))

            orbclose(orb)

        def test_procedure_orbclients(self):

            orb = orbopen(orbname, 'r')

            os.system( "orbtail " + orbname + " now 2 &" )

            os.system( "pf2orb rtexec " + orbname )

            (when, clients) = orbclients( orb )

            self.assertTrue(isinstance(when, float))

            if( len(clients) > 0 ):

                self.assertTrue(isinstance(clients[0]['lastpkt'], float))
                self.assertTrue(isinstance(clients[0]['started'], float))
                self.assertTrue(isinstance(clients[0]['read'], int))
                self.assertTrue(isinstance(clients[0]['pid'], int))
                self.assertTrue(isinstance(clients[0]['bytes'], int))
                self.assertTrue(isinstance(clients[0]['packets'], int))
                self.assertTrue(isinstance(clients[0]['pktid'], int))
                self.assertTrue(isinstance(clients[0]['port'], int))
                self.assertTrue(isinstance(clients[0]['address'], str))
                self.assertTrue(isinstance(clients[0]['thread'], int))
                self.assertTrue(isinstance(clients[0]['fd'], int))
                self.assertTrue(isinstance(clients[0]['nreject'], int))
                self.assertTrue(isinstance(clients[0]['nselect'], int))
                self.assertTrue(isinstance(clients[0]['errors'], int))
                self.assertTrue(isinstance(clients[0]['priority'], int))
                self.assertTrue(isinstance(clients[0]['lastrequest'], int))
                self.assertTrue(isinstance(clients[0]['mymessages'], int))
                self.assertTrue(isinstance(clients[0]['nrequests'], int))
                self.assertTrue(isinstance(clients[0]['nwrites'], int))
                self.assertTrue(isinstance(clients[0]['nreads'], int))
                self.assertTrue(isinstance(clients[0]['written'], int))
                self.assertTrue(isinstance(clients[0]['perm'], str))
                self.assertTrue(isinstance(clients[0]['what'], str))
                self.assertTrue(isinstance(clients[0]['host'], str))
                self.assertTrue(isinstance(clients[0]['who'], str))
                self.assertTrue(isinstance(clients[0]['select'], str))
                self.assertTrue(isinstance(clients[0]['reject'], str))

            os.system( "pf2orb rtexec " + orbname )

            os.system( "sleep 2" )
            os.system( "pf2orb rtexec " + orbname )

            orbclose(orb)

        def test_procedure_orblag(self):

            orb = orbopen(orbname, 'r')

            os.system( "orbtail " + orbname + " now 2 &" )

            os.system( "pf2orb rtexec " + orbname )

            os.system( "sleep 2" )

            ( oldest, newest, maxpktid, range, clients ) = orblag( orb )

            os.system( "pf2orb rtexec " + orbname )

            self.assertTrue(isinstance(oldest, int))
            self.assertTrue(newest > 0)
            self.assertTrue(maxpktid > 0)
            self.assertTrue(range > 0)
            self.assertTrue(isinstance(clients, tuple))

            os.system( "sleep 2" )
            os.system( "pf2orb rtexec " + orbname )

            orbclose(orb)

        def test_procedure_orbresurrect(self):

            tempstate = '/tmp/python_orbtest_state_' + str(os.getuid()) + str(os.getpid())

            rc = orbexhume( tempstate )

            self.assertEqual(rc, 0)

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            pktid, time = orbresurrect( orb )

            self.assertEqual(pktid, -1)
            self.assertEqual(time, -9999999999.999)

            ( pktid, srcname, time, packet, nbytes ) = orbget(orb, ORBNEWEST)

            rc = orbbury(orb, pktid, time)

            self.assertEqual(rc, 0)

            orbclose(orb)

            os.system("/bin/rm -f " + tempstate)

    server = Testorb_fixture()
    server.start()
    suite = unittest.makeSuite(Testorb)
    runner = unittest.TextTestRunner()
    runner.run(suite)
    server.stop()
