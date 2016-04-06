from distutils.core import setup, Extension

core_tools_c = Extension('core_tools_c',
        sources = ['core_tools_c.c'])

setup (name = 'PackageName',
        version = '1.0',
        description = 'This is a demo package',
        ext_modules = [core_tools_c])
