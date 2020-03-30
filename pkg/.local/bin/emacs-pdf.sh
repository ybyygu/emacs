#! /usr/bin/env bash
# [[file:~/Workspace/Programming/emacs/doom.note::*pdf-view][pdf-view:1]]
emacs --maximized --file "$*" --eval "(pdf-view-fit-width-to-window)"
# pdf-view:1 ends here
