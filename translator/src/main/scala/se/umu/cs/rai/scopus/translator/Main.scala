package se.umu.cs.rai.scopus.translator

import java.io.FileWriter
import java.nio.file.{Files, Paths}

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

/**
 * This is the main entry point.
 */
case class Main() {

  val ScopusExtension: String = ".scopus"
  val ScalaExtension: String = ".scala"

  val Help: String = "\n" +
    "\nUsage:" +
    "\n  scopus SCOPUS_SCALA_INPUT" +
    "\nor" +
    "\n  scopus SCOPUS_INPUT SCALA_OUTPUT" +
    "\n" +
    "\nwhere" +
    "\n" +
    "\n  SCOPUS_SCALA_INPUT is used to create the Scopus input file and Scala output file. " +
    "\nIf the extension is " + ScopusExtension + ", the output file has extension " + ScalaExtension + "." +
    "\nOtherwise, the extension " + ScopusExtension + " and " + ScalaExtension + " are appended to create the input and output files respectively." +
    "\n" +
    "\n" +
    "\n  SCOPUS_INPUT is the Scopus input file, regardless of the extension" +
    "\n" +
    "\n  SCALA_OUTPUT is the Scala output file, regardless of the extension" +
    "\n" +
    "\n"


  val getTitleAndVersion: String = {
    val packg = this.getClass.getPackage
    val name = Option(packg.getImplementationTitle).getOrElse("")
    val version = Option(packg.getImplementationVersion).getOrElse("")
    (name + " " + version).trim
  }

  def main(args: Array[String]): Unit =
    if ( args.length == 1
    ) {
      val (inputFileName, outputFileName) = getInputOutputFileNames(args(0))
      translate(inputFileName, outputFileName)
    }
    else if ( args.length == 2
    ) {
      val inputFileName = args(0)
      val outputFileName = args(1)
      translate(inputFileName, outputFileName)
    }
    else println(getTitleAndVersion + Help)


  def getInputOutputFileNames(inputName: String): (String, String) =
    if ( inputName.endsWith(ScopusExtension)
    ) (inputName, inputName.substring(0, inputName.length - ScopusExtension.length) + ScalaExtension)
    else (inputName + ScopusExtension, inputName + ScalaExtension)

  def translate(inputFileName: String, outputFileName: String): Unit = {
    val input = readFile(inputFileName)
    val output = MicroTranslator().translateProgram(input)
    val writer = new FileWriter(outputFileName)
    writer.write(output)
    writer.flush()
  }

  def readFile(fileName: String): String =
    new String(Files.readAllBytes(Paths.get(fileName)))

}
