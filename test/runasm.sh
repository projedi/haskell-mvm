#!/bin/sh

if [ "$#" -ne 1 ]; then
    echo "Needs file name"
    exit 1
fi

DIRNAME=$(mktemp -d)
ASMNAME=$DIRNAME/out.s
BINNAME=$DIRNAME/out
mvm-haskell-exe --dump-asm $1 > $ASMNAME
clang $ASMNAME -masm=intel -o $BINNAME
$BINNAME
rm -rf $DIRNAME
