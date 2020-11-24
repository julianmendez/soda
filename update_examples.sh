#!/bin/bash

#
# This script copies the traduced Scala files into the src/test/scala directory.
# The Scala files that are traduced in the src/test/resource folder need to be compiled, to test its correctness.
#

origin="src\/test\/resources"
target="src\/test\/scala"

files=$(find . | grep ${origin} | grep "\.scala")

for file in ${files}; do
  newFile=$(echo ${file} | sed "s/${origin}/${target}/g")
  cp ${file} ${newFile}
done
