#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import requests

ip = '127.0.0.1'

config = {
    'fileid': '619a255d88d94431b5b17e32d36fd564',
    'range': 'bytes=0-%s' % (os.path.getsize('test.dat') - 1),
    'token': '344118a4d4893153b04618e67918c239',
    'type': '2',
    'filesize': os.path.getsize('test.dat'),
    'filename': 'test.dat',
    'owner': '4d3b9a253dfb4bf4ac316e08f76b08ff@localhost',
    'expiration': 1441788868
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
