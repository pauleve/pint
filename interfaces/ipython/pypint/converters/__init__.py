
from .biocham import import_biocham
from .cadbiom import import_cadbiom
from .sbgnpd import import_sbgnpd
from .minibn import import_minibn

CONVERTERS = {
    "biocham": import_biocham,
    "cadbiom": import_cadbiom,
    "sbgnpd": import_sbgnpd,
}


