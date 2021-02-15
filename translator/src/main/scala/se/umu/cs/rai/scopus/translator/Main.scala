package se.umu.cs.rai.scopus.translator

import java.io.File
import java.io.FileWriter
import java.nio.file.Files
import java.nio.file.Paths

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
    "\nIf it is a directory, it scans recursively the directory to translate Scopus files." +
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
    if ( args.length == 1 ) process_directory(args(0))
    else if ( args.length == 2 ) translate(args(0), args(1))
    else println(getTitleAndVersion + Help)

  def process_directory(start: String) =
    DirectoryScanner()
      .get_scopus_files(new File(start))
      .map(file => {
        val file_name = file.getAbsolutePath
        val (inputFileName, outputFileName) = getInputOutputFileNames(file_name)
        translate(inputFileName, outputFileName)
      })

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
