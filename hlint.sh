#!/bin/sh

EXECUTABLE=$0

function usage() {
  echo "Usage:"
  echo "  $EXECUTABLE"
  echo "    list all errors in src/"
  echo "  $EXECUTABLE report"
  echo "    list all errors in src/ and generate report.html"
  echo "  $EXECUTABLE refactor FILE"
  echo "    interactively apply hints to a specific file"
}

if [ "$#" -eq 0 ]; then
  stack exec hlint -- src/ test/ app/
  exit $?
fi

case "$1" in
  report)
    if [ "$#" -ne 1 ]; then
      usage
      exit 1
    fi
    stack exec hlint -- src/ test/ app/ --report
  ;;
  refactor)
    if [ "$#" -ne 2 ]; then
      usage
      exit 1
    fi
    stack exec hlint -- "$2" --refactor --refactor-options="-i -s"
  ;;
  *)
    usage
    exit 1
  ;;
esac
