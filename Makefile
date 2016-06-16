LINK_FILES_Linux  = conkyrc
LINK_FILES_Darwin = 
LINK_FILES	  = bash_profile bashrc dircolors gitignore_global emacs id-lang.map $(LINK_FILES_$(shell uname -s))
DOTHOME		  = $(PWD)


help:
	@echo "This repository contains the following configuration files: $(LINK_FILES)"
	@echo "\nRun 'make install' to create symlinks to the configuration files in your home directory. \
		\nExisting files will be backed up in ~/.dotfiles_bak.tar. \
		\nRun 'make restore' to replace the symbolic links by the files backed up in ~/.dotfiles_bak.tar. \
		\n\nYou can also run 'make dotfile' to install only a specific dotfile."

install: $(LINK_FILES)

restore:
	tar -Pxf ~/.dotfiles_bak.tar

$(LINK_FILES):
	test -f ~/.$@ && (test -L ~/.$@ || tar -Prf ~/.dotfiles_bak.tar ~/.$@ ) || true
	ln -sf $(DOTHOME)/src/$@ ~/.$@

texshop:
	./bin/texshop-colorscheme.sh -c my_solarized_light

solarized:
	git clone "git://github.com/altercation/solarized.git"

