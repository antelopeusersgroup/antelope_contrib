import _stock

def pfget_string(pfname, pfkey):
    """Retrieve a string value from a parameter file"""

    return _stock._pfget_string(pfname, pfkey)


def pfget_int(pfname, pfkey):
    """Retrieve an integer value from a parameter file"""

    return _stock._pfget_int(pfname, pfkey)


def pfget_double(pfname, pfkey):
    """Retrieve a double-precision value from a parameter file"""

    return _stock._pfget_double(pfname, pfkey)


def pfget_size(pfname, pfkey):
    """Retrieve a size value from a parameter file"""

    return _stock._pfget_size(pfname, pfkey)


def pfget_time(pfname, pfkey):
    """Retrieve a time value from a parameter file"""

    return _stock._pfget_time(pfname, pfkey)


def pfget_boolean(pfname, pfkey):
    """Retrieve a boolean value from a parameter file"""

    return _stock._pfget_boolean(pfname, pfkey)


def strtime(epoch):
    """Convert an epoch time to a standard string"""

    return _stock._strtime(epoch)


def str2epoch(astring):
    """Convert a string to an epoch time"""

    return _stock._str2epoch(astring)


if __name__ == '__main__':
    import unittest
    import operator

    class Teststock(unittest.TestCase):

        def setUp(self):
            pass

        def tearDown(self):
            pass

        def test_pfget_string(self):
           
	    val = pfget_string('trdefaults', 'default_trace_schema')

	    self.assertTrue(isinstance(val, str))

        def test_pfget_int(self):
           
	    val = pfget_int('trdefaults', 'miniseed_record_size')

	    self.assertTrue(isinstance(val, int))

        def test_pfget_double(self):
           
	    val = pfget_double('trdefaults', 'samprate_tolerance')

	    self.assertTrue(isinstance(val, float))

        def test_pfget_size(self):
           
	    val = pfget_size('orbserver', 'ringsize')

	    self.assertTrue(isinstance(val, float))

        def test_pfget_time(self):
           
	    val = pfget_time('trdefaults', 'pf_revision_time')

	    self.assertTrue(isinstance(val, float))

        def test_pfget_boolean(self):
           
	    val = pfget_boolean('trdefaults', 'verbose_splicing')

	    self.assertTrue(isinstance(val, bool))

        def test_strtime(self):

            e = 0
            t = strtime(e)

            self.assertEqual(t,' 1/01/1970   0:00:00.000')

        def test_str2epoch(self):

            s = '1/1/2000 14:00'

            e = str2epoch(s)

            self.assertEqual(e, 946735200)

    unittest.main()
