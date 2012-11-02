#!/bin/sh
cat <<EOF
#!/bin/sh
cd "\`dirname "\$0"\`"
mkdir -p /usr/local/bin
mkdir -p /usr/local/lib
mkdir -p /usr/local/share/pint
for x in $MISC_TOOLS; do
	cp -v \`basename "\$x"\` /usr/local/bin/
done
for x in $OSX_BINS; do
	cp -v \$x.mac /usr/local/bin/\$x
done
mkdir -p /usr/local/share/pint
cp -rv share/* /usr/local/share/pint
cp -v *.dylib /usr/local/lib/
EOF
