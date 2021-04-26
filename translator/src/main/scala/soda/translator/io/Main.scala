package soda.translator.io

import soda.translator.language.MicroTranslator

import java.io.File

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

/**
 * This is the main entry point.
 */
case class Main () {

  lazy val SodaExtension: String = ".soda"
  lazy val ScalaExtension: String = ".scala"

  lazy val Library_marker_file = "lib.soda"
  lazy val Library_directory_in_jar = "/lib/soda/lib/"
  lazy val Library_content_files: Seq [String] = Seq ("Comb.soda", "EnumConstant.soda", "OptionSD.soda", "Rec.soda")
  lazy val Soda_suffix = ".soda"


  lazy val Help: String = "\n" +
    "\nUsage:" +
    "\n  soda SODA_SCALA_INPUT" +
    "\nor" +
    "\n  soda SODA_INPUT SCALA_OUTPUT" +
    "\n" +
    "\nwhere" +
    "\n" +
    "\n  SODA_SCALA_INPUT is used to create the Soda input file and Scala output file. " +
    "\nIf it is a directory, it scans recursively the directory to translate Soda files." +
    "\nIf the extension is " + SodaExtension + ", the output file has extension " + ScalaExtension + "." +
    "\nOtherwise, the extension " + SodaExtension + " and " + ScalaExtension + " are appended to create the input and output files respectively." +
    "\n" +
    "\n" +
    "\n  SODA_INPUT is the Soda input file, regardless of the extension" +
    "\n" +
    "\n  SCALA_OUTPUT is the Scala output file, regardless of the extension" +
    "\n" +
    "\n"


  lazy val Title_and_version: String = {
    lazy val packg = this.getClass.getPackage
    lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
    lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
    (name + " " + version ) .trim
  }

  def main (args: Array [String]  ): Unit =
    if (args.length == 1 ) process_directory (args (0 )  )
    else if (args.length == 2 ) translate (args (0 ), args (1 )  )
    else println (Title_and_version + Help )

  def process_directory (start: String ): Boolean = {

    lazy val result = expand_library (lib_files ) &&
      soda_files
        .map (file => process_soda_file (file )  )
        .forall (x => x )

    lazy val all_files = DirectoryScanner () .get_all_files (new File (start )  )
    lazy val soda_files = all_files
        .filter (x => x.isFile )
        .filter (x => x.getName.endsWith (Soda_suffix )  )
    lazy val lib_files = all_files
        .filter (x => x.isFile )
        .filter (file => file.getName == Library_marker_file )

    result
  }

  def expand_library (lib_files: Seq [File]  ): Boolean =
    lib_files
      .map (lib_file => lib_file.getParent )
      .map (parent_directory =>
        Library_content_files
          .map (lib_file_name =>
            SimpleIO () .write_file (
              file = SimpleIO () .create_file (parent_directory, lib_file_name ), content = SimpleIO () .read_resource (Library_directory_in_jar + lib_file_name )
            )
          ) .forall (x => x )
      ) .forall (x => x )

  def process_soda_file (file: File ): Boolean = {
    lazy val file_name = file.getAbsolutePath
    lazy val t = get_input_output_file_names (file_name )
    translate (t.input_file_name, t.output_file_name )
  }

  def get_input_output_file_names (input_name: String ): FileNamePair =
    if (input_name.endsWith (SodaExtension )
    ) FileNamePair (input_name, input_name.substring (0, input_name.length - SodaExtension.length ) + ScalaExtension )
    else FileNamePair (input_name + SodaExtension, input_name + ScalaExtension )

  def translate (input_file_name: String, output_file_name: String ): Boolean = {
    lazy val input = SimpleIO () .read_file (input_file_name )
    lazy val output = MicroTranslator () .translate_program (input )
    SimpleIO () .write_file (output_file_name, content = output )
  }

  case class FileNamePair (input_file_name: String, output_file_name: String )

}
