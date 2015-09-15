#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import requests

ip = '127.0.0.1'
filename = 'linux.zip'

config = {
    'fileid': '4c1e07084ae1464d8eeb0786e82328e0',
    'range': 'bytes=0-%s' % (os.path.getsize(filename) - 1),
    'token': '132d909d62d476b176cd5558e1c93a',
    'type': '1',
    'filesize': os.path.getsize(filename),
    'filename': filename,
    'owner': '4d3b9a253dfb4bf4ac316e08f76b08ff@localhost',
    'expiration': 1442316380
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
