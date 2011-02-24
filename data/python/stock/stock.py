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

import exceptions
import _stock

for key in _stock._constants:
    exec( "%s = _stock._constants['%s']" % (key, key) )


class ElogException(exceptions.Exception):
    """Raise an ElogException object of specified severity and string message

       This constructor is used internally to stock.py and generally should not be invoked
       directly by end-user code. 

       ElogException( severity, string )
    """

    def __init__(self, severity, string):
        self.args = (severity, string)
	self.severity = severity
	self.string = string


class ElogLog(ElogException):
    """Raise an ElogLog exception with specified string message

       ElogLog( string )

       This constructor is used internally to stock.py. To generate ElogLog exceptions, 
       consider using the standard command

           elog_log( string )
    """

    def __init__(self, string):
        ElogException.__init__(self, ELOG_LOG, string)
        self.args = (string,)


class ElogNotify(ElogException):
    """Raise an ElogNotify exception with specified string message

       ElogNotify( string )

       This constructor is used internally to stock.py. To generate ElogNotify exceptions, 
       consider using the standard command

           elog_notify( string )
    """

    def __init__(self, string):
        ElogException.__init__(self, ELOG_NOTIFY, string)
        self.args = (string,)


class ElogComplain(ElogException):
    """Raise an ElogComplain exception with specified string message

       ElogComplain( string )

       This constructor is used internally to stock.py. To generate ElogComplain exceptions, 
       consider using the standard command

           elog_complain( string )
    """

    def __init__(self, string):
        ElogException.__init__(self, ELOG_COMPLAIN, string)
        self.args = (string,)


class ElogDie(ElogException,SystemError):
    """Raise an ElogDie exception with specified string message

       ElogDie( string )

       This constructor is used internally to stock.py. To generate ElogDie exceptions, 
       consider using the standard command

           elog_die( string )
    """

    def __init__(self, string):
        ElogException.__init__(self, ELOG_DIE, string)
        self.args = (string,)


def _raise_elog(e):
    """Factory function to raise subclasses of ElogException.

       This factory function is used internally to the Antelope Python modules and is not 
       intended to be called external to them.
    """

    if( e.severity == ELOG_LOG ):

        raise ElogLog(e.string)

    elif( e.severity == ELOG_NOTIFY ):

        raise ElogNotify(e.string)

    elif( e.severity == ELOG_COMPLAIN ):

        raise ElogComplain(e.string)

    elif( e.severity == ELOG_DIE ):

        raise ElogDie(e.string)

    else:

        raise ElogException(e.severity, e.string)


def elog_init(argv):
    """Initialize the Antelope error log"""

    try:

        ret = _stock._elog_init(argv)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def elog_log(msg):
    """Put a log message on the Antelope error log"""

    try:

        ret = _stock._elog_log(msg)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def elog_notify(msg):
    """Put a notification message on the Antelope error log"""

    try:

        ret = _stock._elog_notify(msg)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def elog_complain(msg):
    """Put a complaint message on the Antelope error log"""

    try:

        ret = _stock._elog_complain(msg)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def elog_die(msg):
    """Put a fatal message on the Antelope error log and exit"""

    try:

        ret = _stock._elog_die(msg)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_string(pfname, pfkey):
    """Retrieve a string value from a parameter file"""

    try:

        ret = _stock._pfget_string(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_int(pfname, pfkey):
    """Retrieve an integer value from a parameter file"""

    try:
    
        ret = _stock._pfget_int(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_double(pfname, pfkey):
    """Retrieve a floating-point value from a parameter file"""

    try:
    
        ret = _stock._pfget_double(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_size(pfname, pfkey):
    """Retrieve a size value from a parameter file"""

    try:
    
        ret = _stock._pfget_size(pfname, pfkey)

    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret

def pfget_time(pfname, pfkey):
    """Retrieve a time value from a parameter file"""

    try:
    
        ret = _stock._pfget_time(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_boolean(pfname, pfkey):
    """Retrieve a boolean value from a parameter file"""

    try:
    
        ret = _stock._pfget_boolean(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_tbl(pfname, pfkey):
    """Retrieve a table value from a parameter file"""

    try:
    
        ret = _stock._pfget_tbl(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget_arr(pfname, pfkey):
    """Retrieve an array value from a parameter file"""

    try:
    
        ret = _stock._pfget_arr(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfget(pfname, pfkey = None):
    """Retrieve an arbitrary value from a parameter file, or retrieve the whole parameter file"""

    try:
    
        ret = _stock._pfget(pfname, pfkey)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfupdate(pfname):
    """Re-read and update a parameter-file"""

    try:
    
        ret = _stock._pfupdate(pfname)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pffiles(pfname, all = False):
    """Return a list of parameter-file path names"""

    try:
    
        ret = _stock._pffiles(pfname, all)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pfout(pfname, file):
    """Output a parameter file to a file object"""

    return _stock._pfout(pfname, file)


def pfwrite(pfname, filename):
    """Output a parameter file to a named file"""

    return _stock._pfwrite(pfname, filename)


def pf2string(pfname):
    """Convert a parameter-file to a string representation"""

    try:
    
        ret = _stock._pf2string(pfname)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def pf2xml(pfname, flags = None, prolog = None, name = None ):
    """Convert a parameter-file to an XML string representation"""

    try:
    
        ret = _stock._pf2xml(pfname, flags, prolog, name)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strtime(epoch):
    """Convert an epoch time to a standard string"""

    try:
    
        ret = _stock._strtime(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strtdelta(epoch):
    """Convert an epoch time difference to a string representation"""

    try:
    
        ret = _stock._strtdelta(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strydtime(epoch):
    """Convert an epoch time to a string date and time, including julian day"""

    try:
    
        ret = _stock._strydtime(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strdate(epoch):
    """Convert an epoch time to a string date"""

    try:
    
        ret = _stock._strdate(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strlocaltime(epoch):
    """Convert an epoch time to a string date and time in local time zone"""

    try:
    
        ret = _stock._strlocaltime(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strlocalydtime(epoch):
    """Convert an epoch time to a string date and time in local time zone, with julian day"""

    try:
    
        ret = _stock._strlocalydtime(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def strlocaldate(epoch):
    """Convert an epoch time to a string date in local time zone"""

    try:
    
        ret = _stock._strlocaldate(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def str2epoch(astring):
    """Convert a string to an epoch time"""

    try:
    
        ret = _stock._str2epoch(astring)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def epoch2str(epoch, fmt, tz = None):
    """Convert an epoch time to a string"""

    try:
    
        ret = _stock._epoch2str(epoch, fmt, tz)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def epoch(yearday):
    """Convert a yearday value to an epoch time"""

    try:
    
        ret = _stock._epoch(yearday)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def yearday(epoch):
    """Convert an epoch time to a yearday value """

    try:
    
        ret = _stock._yearday(epoch)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def now():
    """Return epoch time for local system clock"""

    try:
    
        ret = _stock._now()
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def grn(lat, lon):
    """Return geographic region number for coordinates"""

    try:
    
        ret = _stock._grn(lat, lon)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def srn(lat, lon):
    """Return seismic region number for coordinates"""

    try:
    
        ret = _stock._srn(lat, lon)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def grname(lat, lon):
    """Return geographic region name for coordinates"""

    try:
    
        ret = _stock._grname(lat, lon)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


def srname(lat, lon):
    """Return seismic region name for coordinates"""

    try:
    
        ret = _stock._srname(lat, lon)
    
    except _stock._ElogException, _e:

        _raise_elog(_e)

    return ret


if __name__ == '__main__':
    import unittest
    import operator
    import sys
    import warnings

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

            val = pfget('orbserver_names')
            self.assertTrue(isinstance(val, dict))

        def test_pffiles(self):

            files = pffiles('rtexec')

            self.assertTrue(isinstance(files,tuple))

	    warnings.filterwarnings('ignore', 'Attempt to coerce non-Boolean.*', RuntimeWarning)

	    self.assertRaises(ElogComplain, pffiles, 'rtexec', 'moo')

	    warnings.resetwarnings()

        def test_pfout(self):

            fp = open('/tmp/trdefaults_test.pf', 'w')

            rc = pfout('trdefaults', fp)

	    fp.close()

	    self.assertEqual(rc, 0)

        def test_pfwrite(self):

            rc = pfwrite('trdefaults', '/tmp/trdefaults_test.pf')

	    self.assertEqual(rc, 0)

        def test_pf2string(self):
           
            val = pf2string('trdefaults')

            self.assertTrue(isinstance(val, str))

        def test_pf2xml(self):
           
            val = pf2xml('trdefaults', PFXML_STRONG)

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

            self.assertTrue(t == ' 0 microseconds      ' or t == '  0 seconds          ')

        def test_str2epoch(self):

            s = '1/1/2000 14:00'

            e = str2epoch(s)

            self.assertEqual(e, 946735200)

        def test_epoch2str(self):

            e = 946735200

            s = epoch2str(e, "%m/%d/%Y %H:%M")
            self.assertEqual(s, '01/01/2000 14:00')

            z = epoch2str(e, "%m/%d/%Y %H:%M %Z", "US/Alaska")
            self.assertEqual(z, '01/01/2000 05:00 AKST')

        def test_epoch(self):

            yd = 2000001
            e = epoch(yd)

            self.assertEqual(e, 946684800)

        def test_yearday(self):

            e = 946684800
            yd = yearday(e)

            self.assertEqual(yd, 2000001)

        def test_now(self):

            e = now()

            self.assertTrue(isinstance(e,float))

        def test_grname(self):

	    name = grname(65, -150)

	    self.assertEqual(name, "NORTHERN ALASKA" )

        def test_srname(self):

	    name = srname(65, -150)

	    self.assertEqual(name, "NORTHEASTERN ASIA, NORTHERN ALASKA TO GREENLAND" )

        def test_grn(self):

	    num = grn(65, -150)

	    self.assertEqual(num, 676)

        def test_srn(self):

	    num = srn(65, -150)

	    self.assertEqual(num, 42)

        def test_elog_init(self):

            elog_init( sys.argv )

	    warnings.filterwarnings('ignore', 'Attempt to convert sequence.*', RuntimeWarning)

            self.assertRaises(ElogComplain, elog_init, {"a":1, "b":2})

            warnings.resetwarnings()

        def test_elog_log(self):

            self.assertRaises(ElogLog, elog_log, "Test notification message")

        def test_elog_notify(self):

            self.assertRaises(ElogNotify, elog_notify, "Test notification message")

        def test_elog_complain(self):

            self.assertRaises(ElogComplain, elog_complain, "Test complaint message")

        def test_elog_die(self):

            self.assertRaises(SystemError, elog_die, "Test fatal message")
            self.assertRaises(ElogDie, elog_die, "Test fatal message")

    unittest.main()
