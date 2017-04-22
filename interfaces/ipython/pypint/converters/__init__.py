
from .biocham import import_biocham
from .cadbiom import import_cadbiom

CONVERTERS = {
    "biocham": import_biocham,
    "cadbiom": import_cadbiom,
}


