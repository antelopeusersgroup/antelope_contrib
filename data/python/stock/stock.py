import _stock

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

        def test_strtime(self):

            e = 0
            t = strtime(e)

            self.assertEqual(t,' 1/01/1970   0:00:00.000')

        def test_str2epoch(self):

            s = '1/1/2000 14:00'

            e = str2epoch(s)

            self.assertEqual(e, 946735200)

    unittest.main()
