#! /usr/bin/env python2
# [[file:~/Install/configs/spacemacs/config.note::41b21fcc-9294-4fac-b9c5-0039ebc9921d][41b21fcc-9294-4fac-b9c5-0039ebc9921d]]
# -*- coding: utf-8 -*-
#====================================================================#
#   DESCRIPTION:  create tar.xz archive of org-mode file including
#                 all files under current directory
#
#       OPTIONS:  ---
#  REQUIREMENTS:  ---
#         NOTES:  ---
#        AUTHOR:  Wenping Guo (ybyygu)
#         EMAIL:  winpng@gmail.com
#       LICENCE:  GPL version 2 or upper
#       CREATED:  <2011-11-18 Fri 13:29>
#       UPDATED:  <>
#====================================================================#
# 41b21fcc-9294-4fac-b9c5-0039ebc9921d ends here

# [[file:~/Install/configs/spacemacs/config.note::53bf16e1-b093-45ae-8f22-22c27f7447f2][53bf16e1-b093-45ae-8f22-22c27f7447f2]]
__VERSION__ = '0.1'
__UPDATED__ = '2013-01-16 14:13:56 ybyygu'

import sys
import os
import re
import urllib

rex_file = re.compile(r'.*\[\[file:([^\]]+)\]')
rex_css = re.compile(r'.*href="([^"]+)".*')
# 53bf16e1-b093-45ae-8f22-22c27f7447f2 ends here

# [[file:~/Install/configs/spacemacs/config.note::6f7a73df-dc80-42a4-bdd8-adebd1546683][6f7a73df-dc80-42a4-bdd8-adebd1546683]]
def find_linked_files(filename):
    """
    find linked files (image or css style etc) from filename in
    org-mode syntax
    """
    files = []

    with open(filename) as fp:
        for line in fp:
            if rex_file.match(line):
                afile = rex_file.match(line).groups()[0]
                if afile:
                    files.append(afile)
            elif rex_css.match(line):
                afile = rex_css.match(line).groups()[0]
                if afile and not afile.startswith("http"):
                    files.append(afile)

    return files

def make_tar_archive(files, archive_name):
    import tarfile

    tar = tarfile.open(archive_name, mode="w", dereference=True)
    lists = set([])
    for afile in files:
        afile = urllib.unquote(afile)
        if os.path.exists(afile):
            print("included {}".format(afile))
            lists.add(afile)
        else:
            print("{} is not in current directory!".format(afile))
    for afile in lists:
        tar.add(afile)
    tar.close()


def main(argv=None):
    import optparse

    if argv == None: argv = sys.argv

    # parsing cmdline
    cmdl_usage = 'usage: %prog [options]...[queue_id]'
    cmdl_version = "%prog " + __VERSION__
    cmdl_parser = optparse.OptionParser(usage=cmdl_usage, \
                                        version=cmdl_version, \
                                        conflict_handler='resolve')
    cmdl_parser.add_option('-h', '--help',
                           action='help',
                           help='print this help text and exit')
    cmdl_parser.add_option('-v', '--version',
                           action='version',
                           help='print program version and exit')
    # cmdl_parser.add_option('-f', '--file', dest='file',
    #                         help='start from this gjf file')
    # cmdl_parser.add_option('-p', '--pause', dest='pause',
    #                         action="store_true", default=False,
    #                         help='pause running job.')
    (cmdl_opts, cmdl_args) = cmdl_parser.parse_args()

    filename = cmdl_args[0]

    files = find_linked_files(filename)
    files.append(filename)
    make_tar_archive(files, archive_name="{}.tar".format(filename))

if __name__ == '__main__':
    main()
# 6f7a73df-dc80-42a4-bdd8-adebd1546683 ends here
