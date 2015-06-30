#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import requests

ip = '127.0.0.1'

config = {
    'uid': 'e9d3c28bd8fb4c0dae95be508945d3d1',
    'range': 'bytes=0-%s' % os.path.getsize('test.dat'),
    'token': 'eed9c49b3245e4ee55d0c9bca565f44c',
    'private': '0',
    'filesize': os.path.getsize('test.dat'),
    'filename': 'test.dat',
    'owner': 'adsfadsfad@localhost',
    'expiration': 1435658452
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

