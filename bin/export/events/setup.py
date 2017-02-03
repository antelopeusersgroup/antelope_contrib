# -*- coding: utf-8 -*-
'''
Installer for export_events in antelope_contrib
'''
from __future__ import print_function

import os
import re
from setuptools import setup, find_packages

VERSION = '1.0.2'


def convert_xpy_to_py(xpy_file_name):
    '''
    Convert .xpy to a working .py file, for installation using setup.py during
    development.
    '''
    py_file_name = os.path.splitext(xpy_file_name)[0] + '.py'
    print('Converting %s to %s.' % (xpy_file_name, py_file_name))

    with open(xpy_file_name) as input_file:
        read_xpy_lines = input_file.read().splitlines()

    with open(py_file_name, 'w') as output_file:
        comment_follows = False
        header_written = False
        for line in read_xpy_lines:
            # skip comments
            if line.startswith('#'):
                output_file.write(line + '\n')
                comment_follows = False
            if line.startswith("'''"):
                output_file.write(line + '\n')
                comment_follows = not comment_follows
                continue

            # write Antelope import lines just after initial comments
            if not comment_follows and not header_written:

                output_file.write(ANTELOPE_IMPORT)
                header_written = True
            else:  # write rest of input, skipping lines in Antelope import
                if line == '' or line not in ANTELOPE_IMPORT.splitlines():
                    output_file.write(line + '\n')

ANTELOPE_IMPORT = '''\
from __future__ import print_function
import os
import sys

sys.path.append(os.environ['ANTELOPE'] + "/data/python")
'''

convert_xpy_to_py('event2qml.xpy')


def read_me():
    '''Returns the first readme.* found in this directory as a string'''
    files = [item for item in os.listdir('.') if os.path.isfile(item)]
    readme_regex = re.compile(r'^readme.*$', re.IGNORECASE)
    read_me_file = next((item for item in files
                         if readme_regex.match(item)), None)
    return open(read_me_file).read()

setup(
    name='export_events',
    version=VERSION,
    description='Python utilities export events via Antelope datascope',
    packages=find_packages(),
    long_description=read_me(),
)
