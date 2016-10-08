#!/usr/bin/env bash
#
# This script is for automatically setting the preferences for a number of programs
#

##
#  OS X El Capitan
##

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
# defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
# defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1


##
#  Finder
##
defaults write com.apple.finder "ShowHardDrivesOnDesktop" -bool true
defaults write com.apple.finder "ShowExternalHardDrivesOnDesktop" -bool true
defaults write com.apple.finder "ShowMountedServersOnDesktop" -bool true
defaults write com.apple.finder "ShowRemovableMediaOnDesktop" -bool true
defaults write com.apple.finder "NewWindowTargetPath" -string "file://$HOME"
defaults write com.apple.finder "NewWindowTarget" -string "PfHm"
defaults write com.apple.finder "AppleShowAllExtensions" -bool true
defaults write com.apple.finder "ShowStatusBar" -bool true
defaults write com.apple.finder "ShowPathbar" -bool false
defaults write com.apple.finder "FXEnableExtensionChangeWarning" -bool false
chflags nohidden "$HOME/Library"

##
#  TextEdit
##
defaults write com.apple.TextEdit RichText -int 0

##
#  Firefox
##

# Set default search engine to DuckDuckGo

##
#  Default ports
##
if command -v port > /dev/null 2>&1; then
    sudo port select --set python python27
    sudo port select --set python2 python27
    sudo port select --set ipython py27-ipython
    sudo port select --set ipython2 py27-ipython
    sudo port select --set cython cython27
    sudo port select --set mpi openmpi-mp-fortran
    sudo port select --set llvm mp-llvm-3.8
fi
if [ -e "/opt/local/bin/bash" ] && [ "$SHELL" != "/opt/local/bin/bash" ]; then
    sudo -s 'grep -Fxq "/opt/local/bin/bash" /etc/shells || echo "/opt/local/bin/bash" >> /etc/shells'
    chsh -s /opt/local/bin/bash
    # Should I also change the root shell ?
fi

##
#  iTerm2
##

# Import profiles automatically:
for profile in $(dirname $0)/../src/iterm-profiles/*.plist; do
    /usr/libexec/PlistBuddy -c "Merge $profile 'New Bookmarks'" "$HOME/Library/Preferences/com.googlecode.iterm2.plist"
done
# Preferences>Keys opt+left arrow send escape code b; opt+right arrow send escape code f; remove corresponding keymappings from individual profiles
# in org-mode alt+up/down arrows to move headlines: alt behaves as esc?

##
#  Skim
##

defaults write net.sourceforge.skim-app.skim "SKAutoCheckFileUpdate" -bool true
# set Skim as the default application to open pdf files
PLpath="$HOME/Library/Preferences/com.apple.LaunchServices/com.apple.launchservices.secure.plist"
nb=$(/usr/libexec/PlistBuddy -c "Print :LSHandlers" "$PLpath" | grep -x '\s*Dict {' | wc -l)
for i in $(seq 0 $((nb-1))); do
    type=$(/usr/libexec/PlistBuddy -c "Print :LSHandlers:$i:LSHandlerContentType" "$PLpath" 2> /dev/null)
    if [ "$type" == "com.adobe.pdf" ]; then
	/usr/libexec/PlistBuddy -c "Set :LSHandlers:$i:LSHandlerRoleAll net.sourceforge.skim-app.skim" "$PLpath"
	status=1
    fi
done
if [ "$status" != 1 ]; then
    defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add '{ LSHandlerContentType = "com.adobe.pdf"; LSHandlerRoleAll = "net.sourceforge.skim-app.skim"; }'    
fi
/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local
