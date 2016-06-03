#!/bin/bash
# not using associative arrays for compatibility with Bash < 4, although they would be useful here.

# getsolarizedcolor solarizedcolor
function getsolarizedcolor {
    case "$1" in
	"base03")  rgb=(0.0000 0.1686 0.2118);;
	"base02")  rgb=(0.0275 0.2118 0.2588);;
	"base01")  rgb=(0.3451 0.4314 0.4588);;
        "base00")  rgb=(0.3961 0.4824 0.5137);;
        "base0")   rgb=(0.5137 0.5804 0.5882);;
        "base1")   rgb=(0.5765 0.6314 0.6314);;
        "base2")   rgb=(0.9333 0.9098 0.8353);;
        "base3")   rgb=(0.9922 0.9647 0.8902);;
        "yellow")  rgb=(0.7098 0.5373 0.0000);;
        "orange")  rgb=(0.7961 0.2941 0.0863);;
        "red")     rgb=(0.8275 0.0039 0.0078);;
	"magenta") rgb=(0.8275 0.2118 0.5098);;
        "violet")  rgb=(0.4235 0.4431 0.7686);;
        "blue")    rgb=(0.1490 0.5451 0.8235);;
        "cyan")    rgb=(0.1647 0.6314 0.5961);;
        "green")   rgb=(0.5216 0.6000 0.0000);;
    esac
    echo "${rgb[*]}"
}

# setcolor context red green blue
function setcolor {
    local context="$1"
    local rgb=("$2" "$3" "$4")
    case "$context" in
	"background"|"foreground"|"insertionpoint"|"ConsoleBackgroundColor"|"ConsoleForegroundColor")
	    contextRGB=("${context}_R" "${context}_G" "${context}_B");;
	"command"|"comment"|"index"|"marker")
	    contextRGB=("${context}red" "${context}green" "${context}blue");;
	highlight*)
	    contextRGB=("${context}Red" "${context}Green" "${context}Blue");;
    esac
    for index in ${!contextRGB[*]}; do
	defaults write TeXShop "${contextRGB[$index]}" "${rgb[$index]}"
    done
}

# applyscheme scheme type
function applyscheme {
    if [ "$1" == "default" ]; then
	# restore the default settings that come with TeXShop
	if [ "$2" == "main" ]; then
	    cat <<EOF
Switching TeXShop color theme back to default.
EOF
	    setcolor background 1.0 1.0 1.0
	    setcolor command 0.0 0.0 1.0
	    setcolor comment 1.0 0.0 0.0
	    setcolor foreground 0.0 0.0 0.0
	    setcolor index 1.0 1.0 0.0
	    setcolor insertionpoint 0.0 0.0 0.0
	    setcolor marker 0.02 0.51 0.13
	    #missing: also reset highlightContent and highlightBraces !!!
	    setcolor highlightContent 1.0 1.0 0.0
	    setcolor highlightBraces 1.0 0.0 1.0
	elif [ "$2" == "console" ]; then
	    cat <<EOF
Switching TeXShop console color theme back to default.
EOF
	    setcolor ConsoleBackgroundColor 0.9961 0.9765 0.9176
	    setcolor ConsoleForegroundColor 0.0 0.0 0.0
	fi
    elif [ "$1" == "solarized_light" ]; then
	cat <<EOF 
Switching TeXShop color theme to Solarized light theme
-- see http://ethanschoonover.com/solarized
-- and https://github.com/altercation/solarized/issues/167
EOF
	setcolor background $(getsolarizedcolor base3)
	setcolor command 0.86 0.196 0.184 # this is not exactly solarized red
	setcolor comment $(getsolarizedcolor base1)
	setcolor foreground $(getsolarizedcolor base00)
	setcolor index $(getsolarizedcolor magenta)
	setcolor marker $(getsolarizedcolor cyan)
	setcolor insertionpoint $(getsolarizedcolor base00)
    elif [ "$1" == "solarized_dark" ]; then
	cat <<EOF 
Switching TeXShop color theme to Solarized dark theme
-- see http://ethanschoonover.com/solarized
-- and https://github.com/altercation/solarized/issues/167
EOF
	setcolor background $(getsolarizedcolor base03)
	setcolor command 0.86 0.196 0.184 # this is not exactly solarized red
	setcolor comment $(getsolarizedcolor base01)
	setcolor foreground $(getsolarizedcolor base0)
	setcolor index $(getsolarizedcolor magenta)
	setcolor marker $(getsolarizedcolor cyan)
	setcolor insertionpoint $(getsolarizedcolor base0)
    elif [ "$1" == "my_solarized_light" ]; then
	if [ "$2" == "main" ]; then
	    cat <<EOF
Switching to my custom TeXShop color theme based on Solarized light theme
-- see http://ethanschoonover.com/solarized
EOF
	    setcolor background $(getsolarizedcolor base3)
	    setcolor command $(getsolarizedcolor blue) # not sure...
	    setcolor comment $(getsolarizedcolor yellow)
	    setcolor foreground $(getsolarizedcolor base01)
	    setcolor index $(getsolarizedcolor green)
	    setcolor marker $(getsolarizedcolor orange)
	    setcolor insertionpoint $(getsolarizedcolor base02)
	    setcolor highlightContent $(getsolarizedcolor base03)
	    setcolor highlightBraces $(getsolarizedcolor red)
	elif [ "$2" == "console" ]; then
	    setcolor ConsoleBackgroundColor $(getsolarizedcolor base03)
	    setcolor ConsoleForegroundColor $(getsolarizedcolor base0)
	fi
    else
	echo "Unknown color scheme"
	exit 1
    fi
}

function filter { echo "$1" | tr '-' '_'; } 

# Get the arguments:
while getopts c: opt; do
    case "$opt" in
	c) scheme=$(filter "$OPTARG"); applyscheme "$scheme" "console";;	
	[?]) print >&2 "Usage: $0 [-c [console_colorscheme]] colorscheme"
	     exit 1;;
    esac
done
shift $(($OPTIND-1))
if [ "$1" != "" ]; then
    scheme=$(filter "$1")
elif [ "$1" == "nil" ]; then
    exit 0
fi
# Apply the color scheme:
applyscheme "$scheme" "main"

