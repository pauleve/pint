## Pint - Static analyzer for dynamics of Automata Networks

[![PyPI version](https://badge.fury.io/py/pypint.svg)](https://badge.fury.io/py/pypint)

Distributed under [CeCCIL licence](http://cecill.info/licences/Licence_CeCILL_V2-en.html).

See https://loicpauleve.name/pint/doc/index.html#Installation for installation
instructions from binary packages and docker image.

## Installation from sources

### Runtime requirements

- [clingo](http://sourceforge.net/projects/potassco/files/clingo/) is required
  by `pint-reach` and `ph2thomas`.
- [mole](http://www.lsv.ens-cachan.fr/~schwoon/tools/mole) is required by
  `pint-mole`.

You can use the script `pint_install_deps` to install runtime dependencies.


### Compilation requirements

- [OCaml](http://caml.inria.fr) >= 4.02
- [OCaml FindLib](http://projects.camlcity.org/projects/findlib.html)
- [CamlIDL](http://caml.inria.fr/pub/old_caml_site/camlidl/)
* [Extlib](https://github.com/ygrek/ocaml-extlib)
- [Facile](http://www.recherche.enac.fr/opti/facile/distrib)
- [Zarith](https://github.com/ocaml/Zarith)
- [Python](http://python.org)
- (optional) [libRmath](http://www.r-project.org) from R - debian/ubuntu: `r-mathlib`


#### Notes on libRmath requirement

Pint requires the libRmath standalone library for the stochastic simulation and parameters
inference from time interval specifications.
You can disable this functionnality with the following command:

	$ python setup.py --disable-R

Most distributions provide a libRmath package (or include the libRmath.so library in the R package).

If you compile R from sources, after the configure step:

	(from R source directory)
	$ cd src/nmath/standalone
	$ make shared
	$ sudo make install


### Compilation

In the root directory of sources:

	$ python setup.py --enable-R

or

	$ python setup.py --disable-R

then

	$ make

If you installed libRmath in a custom location, use `CFLAGS` and `LDFLAGS` environment variables to indicate it:

	LDFLAGS=-L/usr/local/lib CFLAGS=-I/usr/local/include make

### Installation

Add `<Pint source directory>/bin` to your `$PATH` environment variable.


