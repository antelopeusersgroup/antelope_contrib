from __future__ import division, absolute_import

from numpy import get_include
from numpy.distutils.core import Extension
from numpy.distutils.system_info import get_info

import os
import sys

os.environ['CC'] = "gcc"
os.environ['CXX'] = "gcc"

blas_opt = get_info('blas',notfound_action=2)
lapack_opt = get_info('lapack',notfound_action=2)

if not blas_opt:
    raise NotFoundError,'no BLAS resources found'
if not lapack_opt:
    raise NotFoundError,'no LAPACK resources found'

config_path = "%s/lib/python%d.%d/config" % (sys.prefix,
                                             sys.version_info.major,
                                             sys.version_info.minor)

libs = [blas_opt['libraries'][0], lapack_opt['libraries'][0]]
lib_dirs = [blas_opt['library_dirs'][0],
            lapack_opt['library_dirs'][0],
            config_path]
ext_modules = [Extension('algorithm',
                         ['src/algorithm.f90'],
                         libraries=libs,
                         library_dirs=lib_dirs,
                         extra_link_args=['-llapack', '-lblas'])]

eigen_path = os.getcwd()

if __name__ == "__main__":
    from numpy.distutils.core import setup
    from numpy.distutils.core import Extension
    from numpy.distutils.system_info import get_info

    setup(name='phase',
        version='2.1',
        description='Earthquake phase picking and detection algorithms',
        author='Zachary E. Ross',
        author_email='zross@usc.edu',
        url='http://earth.usc.edu/~zross/',
        #packages=['phase'],
        requires=['numpy', 'scipy', 'matplotlib'],
        #scripts=['dbrepick'],
        include_dirs=[get_include(), eigen_path], #Add Include path of numpy
        #ext_package='phase',
        ext_modules=ext_modules
)
