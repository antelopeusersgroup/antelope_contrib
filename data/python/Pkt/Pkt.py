import _Pkt

from _Pkt import *

class Pkt():
    """Create an Antelope Packet object
        
        Pkt( srcname, time, packet, nbytes )
    """
    
    def __init__(self, *args, **kwargs):

        self._srcname = ''
	self._time = -9999999999.999
	self._packet = None
	self._nbytes = 0

        if(len(args) == 4):

            if(not isinstance(args[0], str)):

                raise TypeError, 'first argument to Pkt() must be a string'

            elif(not isinstance(args[1], float)):

                raise TypeError, 'second argument to Pkt() must be a floating-point value'

            elif(not isinstance(args[2], str)):

                raise TypeError, 'third argument to Pkt() must be a string'

            elif(not isinstance(args[3], int)):

                raise TypeError, 'fourth argument to Pkt() must be an integer'

            else:

                pass

        else:

                raise TypeError, 'Pkt constructor arguments not understood'


    def __str__(self):
        
        return ("\n[Pkt:\n" +
            "\tsrcname  = %s\n" % self._srcname +
            "\ttime  = %s\n" % self._time +
            "]\n")

    def unstuff(self):
        """Unpack an Antelope packet"""

        return _Pkt._unstuffPkt(self._srcname, self._time, self._packet, self._nbytes)


def unstuffPkt(srcname, time, packet, nbytes):
    """Unpack an Antelope packet"""

    return _Pkt._unstuffPkt(srcname, time, packet, nbytes)


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

	    type = unstuffPkt( srcname, time, packet, nbytes )

	    self.assertTrue(isinstance(packet, str))
	    self.assertTrue(isinstance(type, int))
 
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
