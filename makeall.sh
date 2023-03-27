#/bin/bash

#
# This script builds the binary file.
# It requires `sbt` installed.
#

sbt scalaVersion sbtVersion version clean compile test package assembly

scalaVersion="3.2.2"
binaryFile="soda"
executableStub="exec java -jar \$0 \"\$@\" ; exit"
jarFile="target/scala-${scalaVersion}/${binaryFile}-*.jar"

echo ${executableStub} >${binaryFile}
cat ${jarFile} >>${binaryFile}
chmod u+x ${binaryFile}

