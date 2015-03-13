#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests


def post_file():

    url = 'http://127.0.0.1:8080/upload'
    files = {'inputfile': open('test.dat', 'rb')}
    headers = {
        'range': 'bytes=0-600',
        'token': 'abcde',
        'filesize': 600,
        'filename': 'test1',
        'owner': '12345adfadfadfadfadfadfadfadfadsfadfadsf@localhost'
    }
    r = requests.post(url, headers=headers, files=files)

    print r.status_code
    print r.text

if __name__ == '__main__':
    post_file()
