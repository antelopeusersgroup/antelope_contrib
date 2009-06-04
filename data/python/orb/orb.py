import _orb

from _orb import *

class Orb():
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

                raise TypeError, 'Orb constructor arguments not understood'

        if(len(args) >= 2):

            if(isinstance(args[1], str)):

                self._perm = args[1]

            else:

                raise TypeError, 'Orb constructor arguments not understood'
        
        if(self._orbname and not isinstance(self._orbname, str)):
            
            raise TypeError, 'dbname must be a string'

        if(not isinstance(self._perm, str)):
            
            raise TypeError, 'perm must be a string'

        if(self._orbname):

            self._orbfd = _orb._orbopen(self._orbname, self._perm)

	    if(self._orbfd < 0):

	        raise RuntimeError, 'Failure opening orbserver %s' % self._orbname

    def __str__(self):
        
        return ("\n[Orb:\n" +
            "\torbfd = %d\n" % self._orbfd +
            "\torbname  = %d\n" % self._orbname +
            "]\n")

    def close(self):
        """Close an Antelope orb connection"""

        _orb._orbclose(self._orbfd)

    def ping(self):
        """Query orbserver version"""

        return _orb._orbping(self._orbfd)

    def tell(self):
        """Query orb read-head position"""

        return _orb._orbtell(self._orbfd)

    def select(self, match):
        """Match orb source-names"""

        return _orb._orbselect(self._orbfd, match)

    def reject(self, reject):
        """Reject orb source names"""

        return _orb._orbreject(self._orbfd, reject)

    def position(self, where):
        """Position orb connection packet pointer by time or code"""

        return _orb._orbposition(self._orbfd, where)

    def seek(self, whichpkt):
        """Position orb connection packet pointer by pktid or code"""

        return _orb._orbseek(self._orbfd, whichpkt)

    def after(self, time):
        """Position orb connection packet pointer by epoch time"""

        return _orb._orbafter(self._orbfd, time)

    def reap(self):
        """Get the next packet from an orb"""

        return _orb._orbreap(self._orbfd)

    def reap_timeout(self, maxseconds):
        """Get the next packet from an orb, waiting a maximum number of seconds"""

        return _orb._orbreap_timeout(self._orbfd, maxseconds)

    def get(self, whichpkt):
        """Get a specified packet from an orb"""

        return _orb._orbget(self._orbfd, whichpkt)

    def put(self, srcname, time, packet, nbytes):
        """Put a packet on an orb"""

        return _orb._orbput(self._orbfd, srcname, time, packet, nbytes)

    def putx(self, srcname, time, packet, nbytes):
        """Put a packet on an orb, returning the pktid of the output packet"""

        return _orb._orbputx(self._orbfd, srcname, time, packet, nbytes)


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


def orbpkt_string(srcname, time, packet, nbytes):
    """Convert an orb packet to string representation"""

    return _orb._orbpkt_string(srcname, time, packet, nbytes)


if __name__ == '__main__':
    import unittest
    import os
    orbname = ':dq'

    class Testorb_fixture():

	tempdir = '/tmp/python_orbtest_' + os.environ["USER"] + str(os.getpid())

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

	    self.assertRaises(RuntimeError, Orb, 'not an orb')
	    
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

    server = Testorb_fixture()
    server.start()
    suite = unittest.makeSuite(Testorb)
    runner = unittest.TextTestRunner()
    runner.run(suite)
    server.stop()
