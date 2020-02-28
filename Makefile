# makefile
# 使用gnu stow来将pkg目录下的文件安装symlink到$HOME下对应的目录

# #+header: :tangle Makefile

# [[file:~/Workspace/Programming/emacs/doom.note::*makefile][makefile:1]]
default: doom-sync

install: stow
	stow --verbose --adopt --no-folding --target ~/ pkg
uninstall:
	stow --verbose --target ~/ --delete pkg

# doom 相关操作
doom-sync: pkg/.doom.d/init.el pkg/.doom.d/packages.el	# 修改doom配置后执行
	doom-emacs/bin/doom sync
doom-build:											                        # 重建.local下build目录
	doom-emacs/bin/doom build
doom-clean:											                        # 清理过期的.elc文件
	doom-emacs/bin/doom clean
doom-upgrade:										                        # 升级doom及packages
	doom-emacs/bin/doom upgrade
stow:
	which stow
# makefile:1 ends here
