#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests


def post_file(ip, token):

    url = 'http://%s:8080/upload' % ip
    files = {'inputfile': open('test.dat', 'rb')}
    headers = {
        'range': 'bytes=0-600',
        'token': token,
        'filesize': 600,
        'filename': 'test2',
        'owner': 'adsfadsfad@localhost',
    }
    r = requests.post(url, headers=headers, files=files)

    print r.status_code
    print r.text

if __name__ == '__main__':
    post_file('127.0.0.1', '8b76c441-2ae5-4676-97c4-721c45895543')
