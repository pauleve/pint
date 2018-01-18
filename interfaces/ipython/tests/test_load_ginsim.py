
import os
import unittest

import pypint

__data_dir__ = os.path.join(os.path.dirname(__file__), "data")

class LoadGINsimTest(unittest.TestCase):
    def test_zginml(self):
        assert pypint.load(os.path.join(__data_dir__, "phageLambda4.zginml"))
    def test_sbml(self):
        assert pypint.load(os.path.join(__data_dir__, "phageLambda4.sbml"))

