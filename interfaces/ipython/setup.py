#!/usr/bin/env python
# -*- coding: utf-8

import os
import re
import sys

if sys.version_info[0] < 3:
    raise Exception("python >= 3 is required")

from setuptools import setup, find_packages
from setuptools.command.install import install

NAME = "pypint"

META = {}
META_FILE = os.path.join(NAME, "__init__.py")
with open(META_FILE) as f:
    __data = f.read()
for key in ["version"]:
    match = re.search(r"^__{0}__ = ['\"]([^'\"]*)['\"]".format(key), __data, re.M)
    if not match:
        raise RuntimeError("Unable to find __{meta}__ string.".format(meta=key))
    META[key] = match.group(1)

setup(name = NAME,
    author = "Loïc Paulevé",
    author_email = "loic.pauleve@ens-cachan.org",
    url = "http://loicpauleve.name/pint",
    license = "CeCILL",
    classifiers=[
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        "Topic :: Scientific/Engineering :: Bio-Informatics",
    ],
    keywords='',
    description = "Python interface to Pint",
    long_description=open('README.rst').read(),
    python_requires=">=3.4",
    install_requires = [
        "colomoto_jupyter[networkx] >=0.4.1, <0.5",
        "networkx >= 2.0",
    ],
    extras_require = {
        "cadbiom": ["boolean.py"],
    },
    entry_points = {
        "console_scripts": [
            "pint-import=pypint.console:import_model",
        ],
    },
    include_package_data = True,
    packages = find_packages(exclude=["tests"]),
    **META
)

