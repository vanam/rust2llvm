#!/bin/bash

FILES=./examples/*.rs
BIN=tmp

for f in $FILES
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
  if [ ! -f "$f" ]; then
    continue
  fi

  ./falsum -o "$BIN" "$f"

  if [ $? -eq 0 ]; then
    ./"$BIN"
  fi

done

if [ -f "$BIN" ]; then
  rm "$BIN"
fi
