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

import _Pkt
import datascope
import stock

from stock import ElogException, ElogLog, ElogNotify, ElogComplain, ElogDie


for _key in _Pkt._constants:
    exec( "%s = _Pkt._constants['%s']" % (_key, _key) )


class PktChannel(object):
    """Create an Antelope PktChannel object. 

       This constructor is used internally to Pkt.py and generally should not be invoked
       directly by end-user code. 

       PktChannel( _pktchannel )
    """

    def __init__(self, *args, **kwargs):

        self._pktchannel = None

        if(len(args) != 1):

            raise TypeError, 'PktChannel constructor requires a _pktchannel object'

        self._pktchannel = args[0]

    def time(self):
       
       try:

           ret = self._pktchannel.time()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def net(self):
       
       try:

           ret = self._pktchannel.net()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def sta(self):
       
       try:

           ret = self._pktchannel.sta()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def chan(self):
       
       try:

           ret = self._pktchannel.chan()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def loc(self):
       
       try:

           ret = self._pktchannel.loc()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def nsamp(self):
       
       try:

           ret = self._pktchannel.nsamp()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def samprate(self):
       
       try:

           ret = self._pktchannel.samprate()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def calib(self):
       
       try:

           ret = self._pktchannel.calib()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def calper(self):
       
       try:

           ret = self._pktchannel.calper()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def segtype(self):
       
       try:

           ret = self._pktchannel.segtype()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def data(self):
       
       try:

           ret = self._pktchannel.data()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def duser1(self):
       
       try:

           ret = self._pktchannel.duser1()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def duser2(self):
       
       try:

           ret = self._pktchannel.duser2()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def iuser1(self):
       
       try:

           ret = self._pktchannel.iuser1()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def iuser2(self):
       
       try:

           ret = self._pktchannel.iuser2()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def iuser3(self):
       
       try:

           ret = self._pktchannel.iuser3()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def cuser1(self):
       
       try:

           ret = self._pktchannel.cuser1()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret

    def cuser2(self):
       
       try:

           ret = self._pktchannel.cuser2()

       except _Pkt._ElogException, _e:

           stock._raise_elog(_e)

       return ret


class Pkt(object):
    """Create an Antelope Packet object
        
        Pkt( srcname, time, packet, nbytes, pktid = -1 )
    """
    
    def __init__(self, *args, **kwargs):

        self._pkt = None

        if(len(args) == 4 or len(args) == 5 ):

            if(not isinstance(args[0], str)):

                raise TypeError, 'first argument to Pkt() (srcname) must be a string'

            elif(not isinstance(args[1], float)):

                raise TypeError, 'second argument to Pkt() (time) must be a floating-point value'

            elif(not isinstance(args[2], str)):

                raise TypeError, 'third argument to Pkt() (packet) must be a string'

            elif(not isinstance(args[3], int)):

                raise TypeError, 'fourth argument to Pkt() (nbytes) must be an integer'

            elif(len(args) == 5 and not isinstance(args[4], int)):

                raise TypeError, 'fifth argument to Pkt() (pktid) must be an integer'

            else:

                pass

        else:

                stock.elog_complain('Pkt constructor arguments not understood')

        srcname = args[0]
        time    = args[1]
        packet  = args[2]
        nbytes  = args[3]

        if( len(args) == 5 ):
            pktid = args[4]
        else:
            pktid = -1

	try:

            self._pkt = _Pkt._pkt( srcname, time, packet, nbytes, pktid )

        except _Pkt._ElogException, _e:

            stock._raise_elog(_e)

    def __str__(self):
        
        return ("\n[Pkt:\n" +
            "\tsrcname  = %s\n" % self.srcname() +
            "\ttime  = %s\n" % self.time() +
            "]\n")

    def type(self):

	try:

            ret = self._pkt.type()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def PacketType(self):

	try:

            ret = self._pkt.PacketType()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def show(self, mode = PKT_TERSE):

	try:

            ret = self._pkt.show(mode)
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def srcname(self):

	try:

            ret = self._pkt.srcname()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def time(self):

	try:

            ret = self._pkt.time()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def parts(self):

	try:

            ret = self._pkt.parts()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def nchannels(self):

	try:

            ret = self._pkt.nchannels()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def channels(self, ichannel):

	try:

            pci = self._pkt.channels(ichannel)
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return PktChannel(pci)

    def string(self):

	try:

            ret = self._pkt.string()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def version(self):

	try:

            ret = self._pkt.version()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def dfile(self):

	try:

            ret = self._pkt.dfile()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def pf(self):

	try:

            ret = self._pkt.pf()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        return ret

    def db(self):

	try:

            db = self._pkt.db()
        
	except _Pkt._ElogException, _e:

	    stock._raise_elog(_e)

        if( isinstance(db, list)):
            
            db = datascope.Dbptr(db)

        return db


def unstuffPkt(srcname, time, packet, nbytes, pktid = -1):
    """Unpack an Antelope packet"""

    pkt = Pkt(srcname, time, packet, nbytes, pktid)

    return pkt.type(), pkt


def showPkt(pktid, srcname, time, packet, nbytes, mode = PKT_TERSE):
    """Print out an Antelope packet"""

    pkt = Pkt(srcname, time, packet, nbytes, pktid)

    pkt.show(mode)

    return


def join_srcname(net, sta, chan, loc, suffix, subcode):
    """Combine component parts into an Antelope srcname"""

    try:

        ret = _Pkt._join_srcname(net, sta, chan, loc, suffix, subcode)

    except _Pkt._ElogException, _e:

        stock._raise_elog(_e)

    return ret


def split_srcname(srcname):
    """Render an Antelope srcname into its component parts"""

    try:

        ret = _Pkt._split_srcname(srcname)

    except _Pkt._ElogException, _e:

        stock._raise_elog(_e)

    return ret


if __name__ == '__main__':
    import unittest
    import os
    from orb import *
    from stock import *
    orbname = ':dq'

    class TestPkt_fixture(object):

        tempdir = '/tmp/python_Pkttest_' + str(os.getuid()) + str(os.getpid())

        def start(self):

            os.mkdir(self.tempdir)
            os.chdir(self.tempdir)
            os.system("pfcp orbserver .")
            os.system("orbserver -p " + orbname + " orbserver &")
            os.system("sleep 3")

        def stop(self):

            os.system("echo halt | orbstat -i " + orbname)
            os.system("sleep 5")
            os.system("/bin/rm -f " + self.tempdir + "/orb/*")
            os.rmdir(self.tempdir + "/orb")
            os.system("/bin/rm -f " + self.tempdir + "/*")
            os.rmdir(self.tempdir)

    class TestPkt(unittest.TestCase):

        def test_Pkt_constructor(self):

            self.assertRaises(ElogComplain, Pkt, 'Pkt constructor arguments not understood')
            
        def test_procedure_unstuffPkt(self):

            orb = orbopen(orbname, 'r')

            os.system( "pf2orb rtexec " + orbname )

            ( pktid, srcname, time, packet, nbytes ) = orb.reap()

            ( type, pkt ) = unstuffPkt( srcname, time, packet, nbytes )

            self.assertTrue(isinstance(packet, str))
            self.assertTrue(isinstance(type, int))
            self.assertTrue(isinstance(pkt, Pkt))
            self.assertTrue(isinstance(pkt.srcname(), str))
            self.assertTrue(isinstance(pkt.time(), float))
            self.assertTrue(isinstance(pkt.version(), int))

            (net, sta, chan, loc, suffix, subcode) = pkt.parts()
 
            self.assertTrue(isinstance(net, str))
            self.assertTrue(isinstance(sta, str))
            self.assertTrue(isinstance(chan, str))
            self.assertTrue(isinstance(loc, str))
            self.assertTrue(isinstance(suffix, str))
            self.assertTrue(isinstance(subcode, str))

            self.assertTrue(isinstance(pkt.nchannels(), int))

            (name, strtype, desc) = pkt.PacketType()

            self.assertTrue(isinstance(name, str))
            self.assertTrue(isinstance(strtype, str))
            self.assertTrue(isinstance(desc, str))

            pf = pkt.pf()

            ft = pfget(pf, "Failure_threshold")

            self.assertTrue(isinstance(ft, int))

        def test_procedure_split_srcname(self):

            (net, sta, chan, loc, suffix, subcode) = split_srcname("AZ_PFO_BHZ_00/BBA/BS")

            self.assertEqual(net, "AZ")
            self.assertEqual(sta, "PFO")
            self.assertEqual(chan, "BHZ")
            self.assertEqual(loc, "00")
            self.assertEqual(suffix, "BBA")
            self.assertEqual(subcode, "BS")

        def test_procedure_join_srcname(self):

            srcname = join_srcname("AZ", "PFO", "BHZ", "00", "BBA", "BS")

            self.assertEqual(srcname, "AZ_PFO_BHZ_00/BBA/BS")

    server = TestPkt_fixture()
    server.start()
    suite = unittest.makeSuite(TestPkt)
    runner = unittest.TextTestRunner()
    runner.run(suite)
    server.stop()
