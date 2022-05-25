#!/bin/sh

# Regression demo script for Handl (interpreted from MicroC and CFlat)


# LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# LLVM compiler
LLC="llc"

# C compiler
CC="cc"

# Path to Handl compiler
HANDL="./handl.native"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: demo.sh [.hdl file]"
    exit 1
}

Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}


basename=`echo $1 | sed 's/.*\\///
                             s/.hdl//'`
Run "$CC -c handlmusic.c"
Run "$HANDL" "$1" ">" "${basename}.ll" &&
Run "$LLC" "-relocation-model=pic" "${basename}.ll" ">" "${basename}.s" &&
Run "$CC" "-o" "${basename}.exe" "${basename}.s" "handlmusic.o" &&
Run "./${basename}.exe" > "${basename}.out" &&

exit