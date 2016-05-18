LINK_FILES	= bash_profile bashrc dircolors
DOTHOME		= $(PWD)

install: $(LINK_FILES)


$(LINK_FILES):
	test -f ~/.$@ && (test -L ~/.$@ || tar -Prf ~/.dotfiles_bak.tar ~/.$@ ) || true
	ln -sf $(DOTHOME)/src/$@ ~/.$@

