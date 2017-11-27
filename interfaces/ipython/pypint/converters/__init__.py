
from .biocham import import_biocham
from .cadbiom import import_cadbiom
from .sbgnpd import import_sbgnpd

CONVERTERS = {
    "biocham": import_biocham,
    "cadbiom": import_cadbiom,
    "sbgnpd": import_sbgnpd,
}


