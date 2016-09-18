#!/usr/bin/env bash
#
# This script is for automatically setting the preferences for a number of programs
#

##
#  Default ports
##

sudo port select --set python python27
sudo port select --set python2 python27
sudo port select --set ipython py27-ipython
sudo port select --set ipython2 py27-ipython
sudo port select --set cython cython27
sudo port select --set mpi openmpi-mp-fortran

##
#  Skim
##

defaults write net.sourceforge.skim-app.skim "SKAutoCheckFileUpdate" -bool YES
