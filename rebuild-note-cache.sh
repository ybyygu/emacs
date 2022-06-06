#! /usr/bin/env bash

set -u 
set -e

cd ~/.cache
rm -rf notes
notesearch.py -r notes
