#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import shutil
import datetime

config = {
    'repo_name': 'mmserl',
    'repo_url': 'https://github.com/caoyue/mmserl.git',
    'config_dir': 'rel',
    'config_file': 'vars.config',
    'release_name': 'mms'
}


class Release(object):

    def __init__(self, config):
        super(Release, self).__init__()
        self.repo_name = config['repo_name']
        self.repo_url = config['repo_url']
        self.config_dir = config['config_dir']
        self.config_file = config['config_file']
        self.release_name = config['release_name']

        self.pwd = os.getcwd()
        self.repo = os.path.join(self.pwd, self.repo_name)
        self.zip_name = "%s%s.tar.gz" % (self.repo_name, self.now())

    def clone(self):
        if os.path.exists(self.repo):
            os.chdir(self.repo)
            self.success(True, "checkout and pull repository ...")
            if self.cmd("git checkout -f"):
                return self.cmd("git pull")
            else:
                return False

        self.success(True, "clone repository ...")
        return self.cmd("git clone %s" % self.repo_url)

    def config(self):
        file = os.path.join(
            self.repo, self.config_dir, self.config_file)
        if os.path.exists(file):
            os.remove(file)
            file2 = os.path.join(self.pwd, self.config_file)
            self.success(True, "replace config ...")
            if os.path.exists(file2):
                shutil.copy(file2, file)
                return True
            else:
                self.success(False, "no such file: %s" % file2)
        else:
            self.success(False, "no such file: %s" % file)

        return False

    def generate(self):
        self.success(True, "generate release ...")
        os.chdir(self.repo)
        return self.cmd("./rebar get-deps compile generate")

    def tar(self):
        self.success(True, "compress release ...")
        release_dir = os.path.join(self.repo, 'rel')
        os.chdir(release_dir)
        return self.cmd("tar -zcvf %s %s" %
                        (os.path.join(self.pwd,  self.zip_name),
                         self.release_name))

    def cmd(self, sh):
        if not os.system(sh):
            return True
        return False

    def now(self):
        return datetime.datetime.now().strftime('%Y%m%d%H%M%S')

    def log(self, text):
        print ">> [release] %s" % text

    def success(self, success, text):
        OKGREEN = '\033[92m'
        FAIL = '\033[91m'
        ENDC = '\033[0m'
        print OKGREEN if success else FAIL
        self.log(text)
        print ENDC

    def go(self):
        flag = False
        if self.clone():
            if self.config():
                if self.generate():
                    if self.tar():
                        flag = True

        self.success(flag, "Success!\n release file: %s" %
                     self.zip_name if flag else "Fail!")

if __name__ == '__main__':
    Release(config).go()
