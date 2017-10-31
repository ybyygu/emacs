#! /usr/bin/env python
# [[file:~/Install/configs/spacemacs/config.note::0c47c380-c352-46e3-adf7-4dcc4ff91461][0c47c380-c352-46e3-adf7-4dcc4ff91461]]
# -*- coding: utf-8 -*-
#==============================================================================#
#   DESCRIPTION:  open text file with emacsclient and register the it into the
#                 recently-used-files list (~/.recently-used.xbel)
#
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#         NOTES:
#        AUTHOR:  Wenping Guo (ybyygu)
#         EMAIL:  winpng@gmail.com
#       LICENCE:  GPL version 2 or upper
#       CREATED:  <2010-09-01 Wed 16:04>
#       UPDATED:  <2017-10-27 Fri 20:00>
#==============================================================================#
#
# basically, to make emacsclient perfect, there are three things need be done:
# 1. use parameter "-a ''", that is, use a void alternate-editor to automatically start
#    emacs daemon
# 2. open the file without "-n" option to make sure the file buffer will be killed when frame
#    is closed
# 3. if the file has been opened, use "-n" option.
# 0c47c380-c352-46e3-adf7-4dcc4ff91461 ends here

# [[file:~/Install/configs/spacemacs/config.note::3f61daf6-b502-45be-b24b-cbf63b31fed4][3f61daf6-b502-45be-b24b-cbf63b31fed4]]
import os
import sys
import subprocess

def check_emacs_client(path):
    """check if the file has been opened by emacsclient"""
    cmdlines = ["/usr/bin/pgrep", "-fa", "emacsclient -c"]
    output = subprocess.Popen(cmdlines,
                              universal_newlines=True,  # output is a str not bytes
                              stdout=subprocess.PIPE).stdout.read()
    print(output)
    if path in output:
        return True
    return False

if len(sys.argv) == 1:
    print("%s: open file using emacsclient." % (__file__))
    sys.exit(0)

path = os.path.abspath(sys.argv[1])
if check_emacs_client(path):
    print("{:} is opened.".format(path))
    args = ["emacsclient", "-n", path]
    subprocess.call(args)
else:
    args = ["emacsclient", "-c", "-a", "", path]
    subprocess.Popen(args)
# 3f61daf6-b502-45be-b24b-cbf63b31fed4 ends here
