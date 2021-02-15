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


  val get_title_and_version: String = {
    val packg = this.getClass.getPackage
    val name = Option(packg.getImplementationTitle).getOrElse("")
    val version = Option(packg.getImplementationVersion).getOrElse("")
    (name + " " + version).trim
  }

  def main(args: Array[String]): Unit =
    if ( args.length == 1 ) process_directory(args(0))
    else if ( args.length == 2 ) translate(args(0), args(1))
    else println(get_title_and_version + Help)

  def process_directory(start: String) =
    DirectoryScanner()
      .get_scopus_files(new File(start))
      .map(file => {
        val file_name = file.getAbsolutePath
        val (input_file_name, output_file_name) = get_input_output_file_names(file_name)
        translate(input_file_name, output_file_name)
      })

  def get_input_output_file_names(input_name: String): (String, String) =
    if ( input_name.endsWith(ScopusExtension)
    ) (input_name, input_name.substring(0, input_name.length - ScopusExtension.length) + ScalaExtension)
    else (input_name + ScopusExtension, input_name + ScalaExtension)

  def translate(input_file_name: String, output_file_name: String): Unit = {
    val input = read_file(input_file_name)
    val output = MicroTranslator().translate_program(input)
    val writer = new FileWriter(output_file_name)
    writer.write(output)
    writer.flush()
  }

  def read_file(fileName: String): String =
    new String(Files.readAllBytes(Paths.get(fileName)))

}
