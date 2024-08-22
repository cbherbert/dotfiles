# dotfiles

This repository is maintained mostly for my personal convenience; it contains the configuration files of my usual setup, and in particular:

	* bash
	* emacs
	* python

This configuration is used in a scientific computing context (numerical simulations and data analysis using C, C++, Fortran and Python), and is designed to work, as much as possible, on different platforms, from the workstation to the cluster. It is tested under both macOS and several Linux distributions (Debian, Ubuntu,...)

There is probably nothing particularly rare here, and many things are actually inspired from other publicly available repositories, but feel free to use anything you might find interesting.
Of course these files are not intended for general use but are only tailored to fit my particular needs and use case, so they should not be expected to work out-of-the-box.

## repository structure

- `src` contains the configuration files, managed by the `Makefile`
- `bin` contains a few useful scripts. Some of them are called from other configuration files, like the ones I used to synchronize lab calendars with my agenda in `org-mode`.
- `contrib` contains a number of external, publicly available repositories, as git submodules. They are not strictly necessary but they correspond to repositories I have used over the years and may be potentially useful to keep around. For instance it contains a few well-known `emacs` distributions.
- personal stuff is contained in a separate `private` repository that I maintain elsewhere. Because it is not publicly available I do not include it as a git submodule, but I use the Makefile to clone it.
