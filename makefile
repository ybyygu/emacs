install: stow
	stow --verbose --adopt --no-folding --target ~/ pkg
uninstall:
	stow --verbose --target ~/ --delete pkg
stow:
	which stow
