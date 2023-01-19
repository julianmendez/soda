#/bin/bash

#
# This script builds the Soda binary file.
# It requires `sbt` installed.
#

sbt scalaVersion sbtVersion version clean compile test package assembly

executableStub="exec java -jar \$0 \"\$@\" ; exit"
sodaFile="soda"
scalaVersion="3.2.0"
jarFile="target/scala-${scalaVersion}/soda-*.jar"

echo ${executableStub} >${sodaFile}
cat ${jarFile} >>${sodaFile}
chmod u+x ${sodaFile}

