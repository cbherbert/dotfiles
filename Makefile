DOTHOME		  = $(PWD)
OS                = $(shell uname -s)
LINK_FILES_Linux  = conkyrc
LINK_FILES_Darwin =
LINK_FILES	  = bash_profile bashrc dircolors emacs.d id-lang.map condarc $(LINK_FILES_$(OS))
XDG_FILES	  = git/ignore git/config
ifeq ($(XDG_CONFIG_HOME),)
XDG_CONFIG_HOME := $(HOME)/.config
endif
PKGFILE           = packages_$(OS)
PKGMGR_Darwin     = port
PKGMGR_Linux      = apt-get -y
PKGMGR            = $(PKGMGR_$(OS))
MPLHOME_Linux     = $(HOME)/.config/matplotlib
MPLHOME_Darwin    = $(HOME)/.matplotlib
MPLHOME           = $(MPLHOME_$(OS))
DOT_PRIVATE       = git@framagit.org:cherbert/dotfiles_private.git
ORG               = $(HOME)/owncloud/org

help:
	@echo "This repository contains the following configuration files: $(LINK_FILES)"
	@echo "\nRun 'make install' to create symlinks to the configuration files in your home directory. \
		\nExisting files will be backed up in ~/.dotfiles_bak.tar. \
		\nRun 'make restore' to replace the symbolic links by the files backed up in ~/.dotfiles_bak.tar. \
		\n\nYou can also run 'make dotfile' to install only a specific dotfile."
	@echo "\nTo install configuration files specific to the user (e.g. containing private information) \
	\nrun 'make private' after editing the Makefile to set the variable DOT_PRIVATE to point to the right repository"

# Note: We create the links by hand rather than using a program such as GNU stow for portability reasons.
#       Those programs often have dependencies which may not be installed on some machines.
#       Even stow for instance is typically not available on supercomputers.

install: $(LINK_FILES) matplotlibrc

restore:
	tar -Pxf ~/.dotfiles_bak.tar

$(LINK_FILES):
	test -f ~/.$@ && (test -L ~/.$@ || tar -Prf ~/.dotfiles_bak.tar ~/.$@ ) || true
	ln -sf $(DOTHOME)/src/$@ ~/.$@

$(XDG_FILES):
	mkdir -p $(XDG_CONFIG_HOME)/$(@D)
	ln -sf $(DOTHOME)/src/$@ $(XDG_CONFIG_HOME)/$@

packages:
	@while IFS='' read -r pkg; do \
		if [ ! -z "$${pkg}" ]; then sudo $(PKGMGR) install "$${pkg}"; fi; \
	done < $(PKGFILE)

bat:
	mkdir -p "$(shell bat --config-dir)/themes"
	cd "$(shell bat --config-dir)/themes" && git clone "https://github.com/paulcpederson/solarized-sublime"
	bat cache --build

$(ORG)/external:
	mkdir -p $@
	mkdir -p $(HOME)/bin
	cp -t $(HOME)/bin/ bin/ical2org.awk bin/sync_lab_calendars.sh
	(crontab -l; echo "*/30 * * * * $$HOME/bin/sync_lab_calendars.sh &> /dev/null") | sort -u | crontab -

python:
# This installs a list of python packages in the user repository using pip.
# This should be reserved for packages that you want to add to the system python,
# use virtual environments for fine-grained package management.
# In the future I should implement an automatic install of my typical virtual
# environments with conda.
	@while IFS='' read -r pkg; do \
		if [ -z "$${pkg}" ]; then pip install "$${pkg}" --user; fi; \
	done < packages_python

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
