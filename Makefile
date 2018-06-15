DOTHOME		  = $(PWD)
OS                = $(shell uname -s)
LINK_FILES_Linux  = conkyrc
LINK_FILES_Darwin =
LINK_FILES	  = bash_profile bashrc dircolors gitignore_global emacs id-lang.map $(LINK_FILES_$(OS))
PKGFILE           = packages_$(OS)
PKGMGR_Darwin     = port
PKGMGR_Linux      = apt-get
PKGMGR            = $(PKGMGR_$(OS))
MPLHOME_Linux     = $(HOME)/.config/matplotlib
MPLHOME_Darwin    = $(HOME)/.matplotlib
MPLHOME           = $(MPLHOME_$(OS))
DOT_PRIVATE       = git@framagit.org:cherbert/dotfiles_private.git

help:
	@echo "This repository contains the following configuration files: $(LINK_FILES)"
	@echo "\nRun 'make install' to create symlinks to the configuration files in your home directory. \
		\nExisting files will be backed up in ~/.dotfiles_bak.tar. \
		\nRun 'make restore' to replace the symbolic links by the files backed up in ~/.dotfiles_bak.tar. \
		\n\nYou can also run 'make dotfile' to install only a specific dotfile."
	@echo "\nTo install configuration files specific to the user (e.g. containing private information) \
	\nrun 'make private' after editing the Makefile to set the variable DOT_PRIVATE to point to the right repository"

install: $(LINK_FILES) matplotlibrc

restore:
	tar -Pxf ~/.dotfiles_bak.tar

$(LINK_FILES):
	test -f ~/.$@ && (test -L ~/.$@ || tar -Prf ~/.dotfiles_bak.tar ~/.$@ ) || true
	ln -sf $(DOTHOME)/src/$@ ~/.$@

packages:
	@while IFS='' read -r pkg; do \
		if [ ! -z "$${pkg}" ]; then sudo $(PKGMGR) install "$${pkg}"; fi; \
	done < $(PKGFILE)

matplotlibrc:
	test -f $(MPLHOME)/$@ && (test -L $(MPLHOME)/$@ || tar -Prf ~/.dotfiles_bak.tar $(MPLHOME)/$@ ) || true
	ln -sf $(DOTHOME)/src/$@_$(OS) $(MPLHOME)/$@

texshop:
	./bin/texshop-colorscheme.sh -c my_solarized_light

solarized:
	git clone "git://github.com/altercation/solarized.git"

private:
	git clone --origin framagit $(DOT_PRIVATE) $@
	cd private && $(MAKE)
