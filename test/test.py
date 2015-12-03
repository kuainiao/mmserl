#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import requests

ip = '127.0.0.1'
filename = 'test.tar.gz'

config = {
    'fileid': 'ae3c5a1d7b4b42c5a19cca02588e0cea',
    'token': '296d4f485769433e32ea7b7159ceb886',
    'type': '1',
    'filesize': os.path.getsize(filename),
    'filename': filename,
    'owner': '4d3b9a253dfb4bf4ac316e08f76b08ff@localhost',
    'expiration': 1449462519,
    'mimeType': 'image/jpeg',
    'uploadid': 'E5C79AE6C1EA494FADD9BA82DB5C1E53',
    'partNumber': 1
}


def post_file(ip, config):

    url = 'http://%s:8080/upload' % ip
    files = {'inputfile': open(config['filename'], 'rb')}
    headers = config
    r = requests.post(url, headers=headers, files=files)

    print r.status_code
    print r.text

if __name__ == '__main__':
    post_file(ip, config)
