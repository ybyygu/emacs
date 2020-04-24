# [[file:~/Workspace/Programming/emacs/doom.note::*Makefile][Makefile:1]]
# https://www.gnu.org/software/make/manual/html_node/One-Shell.html
.ONESHELL:

default: doom-sync

install: stow
	stow --verbose --adopt --no-folding --target ~/ pkg
	rsync -rav ./doom-hacks/modules/tools/magit ~/.doom.d/modules/gwp/
	rsync -rav ./doom-hacks/modules/lang/org ~/.doom.d/modules/gwp/
	rsync -rav ./doom-hacks/modules/lang/rust ~/.doom.d/modules/gwp/
uninstall:
	stow --verbose --target ~/ --delete pkg

# doom 相关操作
doom-sync: pkg/.doom.d/init.el pkg/.doom.d/packages.el # 修改doom配置后执行
	doom-emacs/bin/doom sync
doom-build:                                            # 重建.local下build目录
	doom-emacs/bin/doom build
doom-clean:                                            # 清理过期的.elc文件
	doom-emacs/bin/doom clean
doom-upgrade:                                          # 升级doom及packages
	doom-emacs/bin/doom upgrade
start:
	emacs --with-profile default
stow:
	which stow
# 合并doom develop分支修改
gwp-merge:
	cd doom-hacks
	git checkout develop
	git pull
	git checkout gwp
	git merge -e develop
# Makefile:1 ends here
