if __name__ == "__main__":
    from distutils.core import setup, Extension
    import os
    setup(name="response",
        version="1.0",
        ext_modules=[Extension("response",
                                ["responsemodule.c"],
                                include_dirs=['%s/include'\
                                              % os.environ['ANTELOPE']],
                                libraries=['coords',
                                            'alk',
                                            'stock',
                                            'deviants',
                                            'brttpool',
                                            'response'],
                                library_dirs=['%s/lib' % os.environ['ANTELOPE']]
                                )
                    ]
        )
