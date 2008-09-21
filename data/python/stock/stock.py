import _stock

def pfget_string(pfname, pfkey):
    """Retrieve a string value from a parameter file"""

    return _stock._pfget_string(pfname, pfkey)


def pfget_int(pfname, pfkey):
    """Retrieve an integer value from a parameter file"""

    return _stock._pfget_int(pfname, pfkey)


def pfget_double(pfname, pfkey):
    """Retrieve a floating-point value from a parameter file"""

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


def pfget_tbl(pfname, pfkey):
    """Retrieve a table value from a parameter file"""

    return _stock._pfget_tbl(pfname, pfkey)


def pfget_arr(pfname, pfkey):
    """Retrieve an array value from a parameter file"""

    return _stock._pfget_arr(pfname, pfkey)


def pfget(pfname, pfkey):
    """Retrieve an arbitrary value from a parameter file"""

    return _stock._pfget(pfname, pfkey)


def pfupdate(pfname):
    """Re-read and update a parameter-file"""

    return _stock._pfupdate(pfname)


def pffiles(pfname, all = False):
    """Return a list of parameter-file path names"""

    return _stock._pffiles(pfname, all)


def pf2string(pfname):
    """Convert a parameter-file to a string representation"""

    return _stock._pf2string(pfname)


def pf2xml(pfname, flags = None, prolog = None, name = None ):
    """Convert a parameter-file to an XML string representation"""

    return _stock._pf2xml(pfname)


def strtime(epoch):
    """Convert an epoch time to a standard string"""

    return _stock._strtime(epoch)


def strtdelta(epoch):
    """Convert an epoch time difference to a string representation"""

    return _stock._strtdelta(epoch)


def strydtime(epoch):
    """Convert an epoch time to a string date and time, including julian day"""

    return _stock._strydtime(epoch)


def strdate(epoch):
    """Convert an epoch time to a string date"""

    return _stock._strdate(epoch)


def strlocaltime(epoch):
    """Convert an epoch time to a string date and time in local time zone"""

    return _stock._strlocaltime(epoch)


def strlocalydtime(epoch):
    """Convert an epoch time to a string date and time in local time zone, with julian day"""

    return _stock._strlocalydtime(epoch)


def strlocaldate(epoch):
    """Convert an epoch time to a string date in local time zone"""

    return _stock._strlocaldate(epoch)


def str2epoch(astring):
    """Convert a string to an epoch time"""

    return _stock._str2epoch(astring)


def now():
    """Return epoch time for local system clock"""

    return _stock._now()


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

        def test_pfget_tbl(self):
           
	    val = pfget_tbl('trdefaults', 'waveform_types')

	    self.assertTrue(isinstance(val, tuple))

        def test_pfget_arr(self):
           
	    val = pfget_arr('rtexec', 'Run')

	    self.assertTrue(isinstance(val, dict))

        def test_pfupdate(self):
           
	    rc = pfupdate('rtexec')

	    self.assertTrue(rc == 0 or rc == 1)

        def test_pfget(self):
           
	    val = pfget('trdefaults', 'default_trace_schema')
	    self.assertTrue(isinstance(val, str))

	    val = pfget('trdefaults', 'miniseed_record_size')
	    self.assertTrue(isinstance(val, int))

	    val = pfget('trdefaults', 'samprate_tolerance')
	    self.assertTrue(isinstance(val, float))

	    val = pfget('orbserver', 'ringsize')
	    self.assertTrue(isinstance(val, float))

	    val = pfget('trdefaults', 'waveform_types')
	    self.assertTrue(isinstance(val, tuple))

	    val = pfget('rtexec', 'Run')
	    self.assertTrue(isinstance(val, dict))

        def test_pffiles(self):

	    files = pffiles('rtexec')

	    self.assertTrue(isinstance(files,tuple))

        def test_pf2string(self):
           
	    val = pf2string('trdefaults')

	    self.assertTrue(isinstance(val, str))

        def test_pf2xml(self):
           
	    val = pf2string('trdefaults')

	    self.assertTrue(isinstance(val, str))

        def test_strtime(self):

            e = 0
            t = strtime(e)

            self.assertEqual(t,' 1/01/1970   0:00:00.000')

        def test_strydtime(self):

            e = 0
            t = strydtime(e)

            self.assertEqual(t,' 1/01/1970 (001)  0:00:00.000')

        def test_strdate(self):

            e = 0
            t = strdate(e)

            self.assertEqual(t,' 1/01/1970')

        def test_strlocaldate(self):

            e = 0
            t = strlocaldate(e)

            self.assertTrue(isinstance(t, str))

        def test_strlocaltime(self):

            e = 0
            t = strlocaltime(e)

            self.assertTrue(isinstance(t, str))

        def test_strlocalydtime(self):

            e = 0
            t = strlocalydtime(e)

            self.assertTrue(isinstance(t, str))

        def test_strtdelta(self):

            e = 0
            t = strtdelta(e)

            self.assertEqual(t,' 0 microseconds      ')

        def test_str2epoch(self):

            s = '1/1/2000 14:00'

            e = str2epoch(s)

            self.assertEqual(e, 946735200)

        def test_now(self):

            e = now()

	    self.assertTrue(isinstance(e,float))

    unittest.main()
