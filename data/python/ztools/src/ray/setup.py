if __name__ == "__main__":
    from distutils.core import setup, Extension
    import os
    import sys
    import numpy as np
    os.environ['CC'] = "g++"
    os.environ['CXX'] = "g++"

    lib_dirs = ["%s/lib/python%d.%d/config" % (sys.prefix,
                                                sys.version_info.major,
                                                sys.version_info.minor)]

    setup(name="raytracer",
          version="1.0beta",
          author="Zachary Ross",
          maintainer="Malcolm White",
          maintainer_email="malcolcw@usc.edu",
          descriptiont="ray tracing library",
          py_modules=['raytracer'],
          include_dirs=[np.get_include()], #Add Include path of numpy
          ext_modules=[Extension('ray_tracer_cc',
                                 sources=['ray_tracer_cc.cc'],
                                 extra_compile_args=['-O3'],
                                 library_dirs=lib_dirs)
                      ]
           )
