#!/bin/sh -e

case $0 in
    /*) LOADPATH=${0%/*};;
    */*) LOADPATH=./${0%/*};;
    *) LOADPATH=.;;
esac

run_guile () {
    guile --fresh-auto-compile -L "$LOADPATH/" "$LOADPATH/main.scm" "$@"
}

run_guile test.scm
