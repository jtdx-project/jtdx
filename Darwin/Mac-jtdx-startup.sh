#!/bin/sh
JTDX_BUNDLE="`echo "$0" | sed -e 's/\/Contents\/MacOS\/.*//'`"
JTDX_RESOURCES="$JTDX_BUNDLE/Contents/Resources"
JTDX_TEMP="/tmp/jtdx/$UID"

echo "running $0"
echo "JTDX_BUNDLE: $JTDX_BUNDLE"

# Setup temporary runtime files
rm -rf "$JTDX_TEMP"

export "DYLD_LIBRARY_PATH=$JTDX_RESOURCES/lib"
export "PATH=$JTDX_RESOURCES/bin:$PATH"

#export
exec "$JTDX_RESOURCES/bin/jtdx"
