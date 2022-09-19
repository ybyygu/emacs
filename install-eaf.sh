#! /usr/bin/bash
# [[file:gwp.note::73bab0df][73bab0df]]
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/

cd ~/.emacs.d/site-lisp/emacs-application-framework
chmod +x ./install-eaf.py
./install-eaf.py
# 73bab0df ends here
