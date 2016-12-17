#!/bin/sh

COMMAND="stack exec hindent -- "

find src/ -name '*.hs' -exec ${COMMAND} {} \;
find test/ -name '*.hs' -exec ${COMMAND} {} \;
find app/ -name '*.hs' -exec ${COMMAND} {} \;
