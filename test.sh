#!/bin/sh

find examples -name '*.mvm' | xargs stack exec hs-jit-playground-exe
