
:: REQUIREMENT

- OCaml (http://caml.inria.fr)
- CamlIDL (http://caml.inria.fr/pub/old_caml_site/camlidl/)
- libRmath (from R - http://www.r-project.org)
- Facile (http://www.recherche.enac.fr/opti/facile/distrib)

Notes on libRmath requirement:
- Pint requires the libRmath standalone library for the stochastic parameters inference from time interval specifications.
- You can disable this functionnality by applying the patch 'patches/disable-R-bindings.patch':
	$ patch -p0 patches/disable-R-bindings.patch
- Most distributions provide a libRmath package (or include the libRmath.so library in the R package).
- If you compile R from sources, after the configure step:
	(from R source directory)
	$ cd src/nmath/standalone
	$ make shared
	$ sudo make install



:: COMPILATION 

$ python setup.py
$ make

IMPORTANT: if you installed libRmath in a custom location, use CFLAGS and LDFLAGS environment
variables to indicate it:
  LDFLAGS=-L/usr/local/lib CFLAGS=-I/usr/local/include make


:: INSTALLATION 

- Add PINT_PATH/bin to your $PATH.

