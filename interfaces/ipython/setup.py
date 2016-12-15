# -*- coding: utf-8

from setuptools import setup, find_packages
from setuptools.command.install import install

setup(name = "pint",
    version = "0.1",
    author = "Loïc Paulevé",
    author_email = "loic.pauleve@ens-cachan.org",
    url = "http://loicpauleve.name/pint",
    licence = "CeCILL",
    classifiers=[
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
    ],
    keywords='',
    description = "Python interface to Pint",
    long_description=open('README.rst').read(),
    install_requires = ["networkx"],
    packages = find_packages()
)

