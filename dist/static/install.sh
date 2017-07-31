#!/bin/bash
if [ -z "$1" ]; then
    PREFIX=/usr/local
else
    PREFIX=$1
fi
echo "Installing pint in $PREFIX"
set -e
mkdir -p "${PREFIX}/bin"
mkdir -p "${PREFIX}/share"
for x in bin/*; do
	cp -v "$x" "${PREFIX}/bin/"
done
cat >"${PREFIX}/bin/pint-env"<<EOF
#!/bin/sh
echo "PINT_SHARE_PATH=${PREFIX}/share/pint"
EOF
cp -rv share "${PREFIX}/share/pint"
pint_install_deps
