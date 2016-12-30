import sys
from numpy.distutils.core import setup,\
                                 Extension

if __name__ == "__main__":
    lib_dirs = ["%s/lib/python%d.%d/config" % (sys.prefix,
                                               sys.version_info.major,
                                               sys.version_info.minor)]
    setup(name="moment",
          version="1.0beta",
          author="Zachary Ross",
          maintainer="Malcolm White",
          maintainer_email="malcolcw@usc.edu",
          description="library to estimate seismic moment magnitude",
          ext_modules = [Extension(name = 'moment',
                                   sources = ['moment.f90'],
                                   library_dirs=lib_dirs)])
