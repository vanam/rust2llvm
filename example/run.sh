#!/bin/bash

################################################################################
# VARIABLES                                                                    #
################################################################################

BIN=tmp
FILE=""

################################################################################
# FUNCTIONS                                                                    #
################################################################################

function help {
    echo "Falsum LLVM example test tool"
    echo "-----------------------------"
    echo "Copyright (c) Martin Váňa, 2016"
    echo "Program needs 'clang' dependencies."
    echo ""
    echo "Usage: $1 <LLVM_FILE_NAME | OPTION>"
    echo "Available options"
    echo "    -h                           displays help"
}

################################################################################
# CODE                                                                         #
################################################################################

if [ $# -eq 0 ]; then
  echo "No arguments provided."
  echo "Use '$0 -h' for help."
  exit 1
fi

if [ $# -gt 1 ]; then
  echo "Too many arguments provided."
  exit 1
fi

# Load arguments
case $1 in
  -h) help $0
      exit 0
      ;;

  *)  FILE=$1
      ;;
esac

# Compile
echo "Compiling '$FILE'"
clang $FILE -o $BIN

# Check if compilation failed
if [ ! $? -eq 0 ]; then
  exit 1;
fi

# Run and print exit status
./$BIN ; echo "Exit status: $?"

# Clean
if [ -f $BIN ]; then
    rm $BIN
fi
