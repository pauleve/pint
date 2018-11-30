
echo "Installing pint in $PREFIX"
set -e

tar xvJf *.txz --strip-components=1

mkdir -p "${PREFIX}/bin"
cp -v -p bin/* "${PREFIX}/bin/"

mkdir -p "${PREFIX}/share/pint"
cp -r share/* "${PREFIX}/share/pint"

