#!/bin/bash

FILES=./examples/*.rs
BIN=tmp

GREEN='\033[0;32m'
NC='\033[0m' # No Color

for f in $FILES
do
  echo -e "${GREEN}falsum: Processing '$f' file...${NC}"

  if [ ! -f "$f" ]
  then
    continue
  fi

  ./falsum -o "$BIN" "$f"

  if [ $? -eq 0 ]
  then
    ./"$BIN"
  fi

done

if [ -f "$BIN" ]
then
  rm "$BIN"
fi
