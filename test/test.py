#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests

ip = '127.0.0.1'

config = {
    'uid': 'dd28e2ca-c263-4600-937f-0a18bddca054',
    'range': 'bytes=0-600',
    'token': 'dd28e2cdd28e2cdd28e2cdd28e2cdd28e2c',
    'filesize': 600,
    'filename': 'test.dat',
    'owner': 'adsfadsfad@localhost',
    'expiration': 1429001366
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
