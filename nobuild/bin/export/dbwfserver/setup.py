"""Install this package via setuptools."""

from setuptools import find_namespace_packages, setup

setup(
    name="pydbwfserver",
    version="0.1",
    packages=find_namespace_packages(include=["pydbwfserver.*"]),
    author="UCSD ANF",
    author_email="support@anf.ucsd.edu",
    url="http://github.com/UCSD-ANF/anfsrc/anf/bin/web/usarray_deploy_map",
    install_requires=["six", "antelope", "twisted"],
)
