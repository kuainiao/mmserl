#!/bin/bash

echo upload-release $SPEC
if [ "$SPEC" = "embedded" ]; then
    tar cvzf $TARBALL_NAME -C rel/mms/ .
    curl --ftp-create-dirs -T ${TARBALL_NAME} -u ${FTP_USER}:${FTP_PASSWORD} ftp://${FTP_SERVER}/mms/${TARBALL_NAME}
    curl -d "release=${TARBALL_NAME}" "${PUSH_SERVER}"
    echo $TARBALL_NAME uploaded
fi
