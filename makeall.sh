#/bin/bash

#
# This script builds the binary file.
# It requires `sbt` [https://www.scala-sbt.org/].
#

sbt scalaVersion sbtVersion version clean compile test package assembly

scalaVersion="3.3.1"
binaryFile="soda"
executableStub="exec java -jar \$0 \"\$@\" ; exit"
jarFile="target/scala-${scalaVersion}/${binaryFile}-*.jar"

echo ${executableStub} >${binaryFile}
cat ${jarFile} >>${binaryFile}
chmod u+x ${binaryFile}

