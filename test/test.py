#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import requests

ip = '127.0.0.1'
filename = 'abc.jpg'

config = {
    'fileid': 'e8db188db96f4f3ba5f9b6e09bbecbfc',
    'range': 'bytes=0-%s' % (os.path.getsize(filename) - 1),
    'token': 'ac88f5dca0d9d3055a370c99374413f',
    'type': '1',
    'filesize': os.path.getsize(filename),
    'filename': filename,
    'owner': '4d3b9a253dfb4bf4ac316e08f76b08ff@localhost',
    'expiration': 1442393886,
    'mimeType': 'image/jpeg'
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
