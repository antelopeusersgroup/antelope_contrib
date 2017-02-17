# -*- coding: utf-8 -*-
"""
Installer for export_events in antelope_contrib
"""
import os
from setuptools import setup, find_packages


def convert_xpy_to_py(xpy_file_name):
    '''
    Convert .xpy to a working .py file, for installation using setup.py during
    development.
    '''
    with open(xpy_file_name) as file:
        read_xpy = file.read()

    py_file_name = os.path.splitext(xpy_file_name)[0] + '.py'

    with open(py_file_name, 'w') as file:
        file.write(ANTELOPE_HEADER)
        file.write(read_xpy)

ANTELOPE_HEADER = '''\
import os
import sys

sys.path.append(os.environ['ANTELOPE'] + "/data/python")
'''


def read_description(file_name):
    return open(os.path.join(os.path.dirname(__file__), file_name)).read()

convert_xpy_to_py('event2qml.xpy')

setup(
    name='export_events',
    version='0.0.2',
    description='Python utilities export events via Antelope datascope',
    packages=find_packages(),
    long_description=read_description('README.md'),
)
