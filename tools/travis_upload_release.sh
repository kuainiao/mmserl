#!/bin/bash

echo upload-release $SPEC
if [ "$SPEC" = "embedded" ]; then
    ./rebar generate
    cd rel
    tar zcvf $TARBALL_NAME mms
    curl --ftp-create-dirs -T ${TARBALL_NAME} -u ${FTP_USER}:${FTP_PASSWORD} ftp://${FTP_SERVER}/mmserl/${TARBALL_NAME}
    echo "\n${TARBALL_NAME} uploaded"
fi
