#!/bin/sh
cat <<EOF
#!/bin/sh
cd "\`dirname "\$0"\`"
for x in $MISC_TOOLS; do
	cp -v \`basename "\$x"\` /usr/local/bin/
done
for x in $OSX_BINS; do
	cp -v \$x.mac /usr/local/bin/\$x
done
cp -v *.dylib /usr/local/lib/
EOF
