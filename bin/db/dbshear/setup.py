#!/usr/bin/env python
# -*- coding: UTF-8 -*-

from distutils.core import setup, Extension
import numpy as np
import os

os.environ['CC'] = "g++"
os.environ['CXX'] = "g++"

eigen_path = os.getcwd()
ext_modules = [Extension('dbshear',
                         sources=['dbshear_py.cc',
                                  'picker.cc'],
                         extra_compile_args=['-O3'],
                         include_dirs=[np.get_include(), eigen_path])
              ]
setup(
        name='dbshear',
        version='1.0',
        #include_dirs = [np.get_include(), eigen_path], #Add Include path of numpy
        ext_modules=ext_modules
      )
