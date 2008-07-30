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


def orbopen(orbname, perm = 'r'):
    """Open an Antelope orb connection"""

    return Orb(orbname, perm)


def orbclose(orb):
    """Close an Antelope orb connection"""

    orb.close()

    return 


if __name__ == '__main__':
    import unittest
    import os

    class Testdatascope(unittest.TestCase):
        orbname = ':dq'

        def setUp(self):
 
            pass

        def tearDown(self):

            pass

        def test_Orb_constructor(self):

	    orb = Orb(self.orbname)

	    self.assertRaises(RuntimeError, Orb, 'not an orb')

        def test_procedure_orbopen(self):

	    orb = orbopen(self.orbname, 'r')

        def test_procedure_orbclose(self):

	    orb = orbopen(self.orbname, 'r')

	    orbclose(orb)

    unittest.main()
