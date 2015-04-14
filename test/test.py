#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests


def post_file(ip, token):

    url = 'http://%s:8080/upload' % ip
    files = {'inputfile': open('test.dat', 'rb')}
    headers = {
        'uid': 'dd28e2ca-c263-4600-937f-0a18bddca054',
        'range': 'bytes=0-600',
        'token': token,
        'filesize': 600,
        'filename': 'test1.txt',
        'owner': 'adsfadsfad@localhost',
        'expiration': 1429001366
    }
    r = requests.post(url, headers=headers, files=files)

    print r.status_code
    print r.text

if __name__ == '__main__':
    post_file('127.0.0.1', '552730b34e2a692d1dafa0764d132782')
