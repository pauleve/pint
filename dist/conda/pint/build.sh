
## build from source (requires OCaml outside of conda)
#python setup.py --disable-R
#make


## Extract from the deb package
#ar x pint_2017-04-13_amd64.deb
#tar axf data.tar.xz

#cp -a usr/bin/* "$PREFIX/bin/"
#cp -a usr/lib/ocaml "$PREFIX/lib/"


echo "Installing pint in $PREFIX"
set -e

mkdir -p "${PREFIX}/bin"
cp -v -p bin/* "${PREFIX}/bin/"

mkdir -p "${PREFIX}/share/pint"
cp -r share/* "${PREFIX}/share/pint"


# Set the pint share path in the conda env
mkdir -p "${PREFIX}/etc/conda/activate.d"
cat >"${PREFIX}/etc/conda/activate.d/pint.sh"<<EOF
#!/bin/sh
export PINT_SHARE_PATH="$CONDA_PREFIX/share/pint/"
EOF


