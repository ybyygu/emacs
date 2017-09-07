#! /bin/sh


# 3. 命令行启动 emacs
#    #+header: :tangle start.sh
#    #+header: :shebang #! /bin/sh

# HOME=`pwd` emacs
# emacs -q -l ...
emacs --no-init-file --load ~/Install/configs/spacemacs/init.el
