#!/bin/bash

# This script translates the Soda source files into Lean.
#
# 2024-02-18

oldIFS="$IFS"
IFS=$'\n'
# $IFS is the Internal Field Separator.
# Its default value is <space><tab><newline>.
# This is changed to support spaces in file names.


pathToSodaDir="examples/src/main/scala/soda/example/forlean"
pathToLeanDir="Soda/example/forlean"

files=$(find "${pathToSodaDir}" -type f -name "*.soda")

for file in ${files}; do
  relPath="${file#${pathToSodaDir}/}"
  dirPath="$(dirname "${relPath}")"
  fileName="$(basename "${file}" .soda)"

  mkdir -p "${pathToLeanDir}/${dirPath}"
  soda lean "${file}" "${pathToLeanDir}/${dirPath}/${fileName}.lean"
done

# This updates elan
elan self update

lake clean

# This updates lake
lake update

# lake build

IFS="$oldIFS"


