#!/bin/bash
PROBCOMMAND=prob

# Shell wrapper for PROBCOMMAND
# Run this script to start ProB Tcl/Tk

export TRAILSTKSIZE=1M

# comment in and adapt the next line to point to the Tcl/Tk dynamic library
# if you get a message "Unable to start Tcl/Tk" when trying to start ProB
# export SP_TCL_DSO=/opt/homebrew/Cellar/tcl-tk/8.6.13_1/lib/libtcl8.6.dylib
# The shell script below will try and auto-detect Homebrew and MacPorts 8.6

echo "Running ProB Tcl/Tk"
#echo `readlink "$0"` 

# dirname
if [ -h "$0" ]
then
    realname=`readlink "$0"`
    dirname=`dirname "$realname"`
else
    dirname=`dirname "$0"`
fi

ulimit -d unlimited

echo "Operating System: $OSTYPE, Directory: $dirname"
#cd $dirname
#ls

if [[ "$OSTYPE" == "darwin"* ]]; then
    if [[ -z "${SP_TCL_DSO}" ]]; then
        echo "Trying to set SP_TCL_DSO for Homebrew (ARM)"
        for f in /opt/homebrew/Cellar/tcl-tk/8.6*/lib/libtcl8.6.dylib; do
            ## Check if the glob gets expanded to an existing file
            ## If not, f here will be exactly the pattern above
            ## and the exists test will evaluate to false.
            [ -e "$f" ] && export SP_TCL_DSO="$f" || echo "Homebrew (ARM) Tcl/Tk does not exist"
            ## break after the first iteration
            break
        done
    fi
    if [[ -z "${SP_TCL_DSO}" ]]; then
        echo "Trying to set SP_TCL_DSO for Homebrew (x86_64)"
        for f in /usr/local/Cellar/tcl-tk/8.6*/lib/libtcl8.6.dylib; do
            [ -e "$f" ] && export SP_TCL_DSO="$f" || echo "Homebrew (x86_64) Tcl/Tk does not exist"
            break
        done
    fi
    if [[ -z "${SP_TCL_DSO}" ]]; then
        echo "Trying to set SP_TCL_DSO for MacPorts"
        f=/opt/local/lib/libtcl8.6.dylib
        [ -e "$f" ] && export SP_TCL_DSO="$f" || echo "MacPorts Tcl/Tk does not exist"
    fi
fi
echo "$dirname/$PROBCOMMAND" "$@"
"$dirname/$PROBCOMMAND" "$@"
