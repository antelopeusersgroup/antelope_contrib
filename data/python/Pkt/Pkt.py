import _Pkt

from _Pkt import *
from datascope import *

class PktChannel():
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
       
       return self._pktchannel.time()

    def net(self):
       
       return self._pktchannel.net()

    def sta(self):
       
       return self._pktchannel.sta()

    def chan(self):
       
       return self._pktchannel.chan()

    def loc(self):
       
       return self._pktchannel.loc()

    def nsamp(self):
       
       return self._pktchannel.nsamp()

    def samprate(self):
       
       return self._pktchannel.samprate()

    def calib(self):
       
       return self._pktchannel.calib()

    def calper(self):
       
       return self._pktchannel.calper()

    def segtype(self):
       
       return self._pktchannel.segtype()

    def data(self):
       
       return self._pktchannel.data()

    def duser1(self):
       
       return self._pktchannel.duser1()

    def duser2(self):
       
       return self._pktchannel.duser2()

    def iuser1(self):
       
       return self._pktchannel.iuser1()

    def iuser2(self):
       
       return self._pktchannel.iuser2()

    def iuser3(self):
       
       return self._pktchannel.iuser3()

    def cuser1(self):
       
       return self._pktchannel.cuser1()

    def cuser2(self):
       
       return self._pktchannel.cuser2()


class Pkt():
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

                raise TypeError, 'Pkt constructor arguments not understood'

        srcname = args[0]
	time    = args[1]
	packet  = args[2]
	nbytes  = args[3]

	if( len(args) == 5 ):
	    pktid = args[4]
        else:
	    pktid = -1

        self._pkt = _Pkt._pkt( srcname, time, packet, nbytes, pktid )

    def __str__(self):
        
        return ("\n[Pkt:\n" +
            "\tsrcname  = %s\n" % self.srcname() +
            "\ttime  = %s\n" % self.time() +
            "]\n")

    def type(self):

        return self._pkt.type()

    def PacketType(self):

        return self._pkt.PacketType()

    def show(self, mode = PKT_TERSE):

        return self._pkt.show(mode)

    def srcname(self):

        return self._pkt.srcname()

    def time(self):

        return self._pkt.time()

    def parts(self):

        return self._pkt.parts()

    def nchannels(self):

        return self._pkt.nchannels()

    def channels(self, ichannel):

        return PktChannel(self._pkt.channels(ichannel))

    def string(self):

        return self._pkt.string()

    def version(self):

        return self._pkt.version()

    def dfile(self):

        return self._pkt.dfile()

    def pf(self):

        return self._pkt.pf()

    def db(self):

	db = self._pkt.db()

	if( isinstance(db, list)):
	    
	    db = Dbptr(db)

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

    return _Pkt._join_srcname(net, sta, chan, loc, suffix, subcode)


def split_srcname(srcname):
    """Render an Antelope srcname into its component parts"""

    return _Pkt._split_srcname(srcname)


if __name__ == '__main__':
    import unittest
    import os
    from orb import *
    from stock import *
    orbname = ':dq'

    class TestPkt_fixture():

	tempdir = '/tmp/python_Pkttest_' + os.environ["USER"] + str(os.getpid())

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

	    self.assertRaises(TypeError, Pkt, 'Pkt constructor arguments not understood')
	    
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
