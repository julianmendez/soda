package soda.translator.io


object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

/**
 * This is the main entry point.
 */
trait MainClass {
  import soda.translator.language.MicroTranslatorImpl
  import java.io.File

  lazy val soda_extension: String = ".soda"

  lazy val scala_extension: String = ".scala"

  lazy val help: String =
    SimpleIOImpl () .read_resource ("/soda/translator/documentation/help.txt")

  lazy val title_and_version: String =
    {
      lazy val packg = this.getClass.getPackage
      lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
      lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
      (name + " " + version ) .trim }

  def main (arguments: Array [String]  ): Unit =
    if (arguments.length == 1 ) process_directory (arguments (0 )  )
    else if (arguments.length == 2 ) translate (arguments (0 ), arguments (1 )  )
    else println (title_and_version + "\n\n" + help )

  def process_directory (start: String ): Boolean =
    DirectoryProcessorImpl (start, process_soda_file ) .process ()

  def process_soda_file (file: File ): Boolean =
    {
      lazy val file_name = file.getAbsolutePath
      lazy val t = get_input_output_file_names (file_name )
      translate (t.input_file_name, t.output_file_name ) }

  def get_input_output_file_names (input_name: String ): FileNamePair =
    if (input_name.endsWith (soda_extension )
    ) FileNamePair (input_name, input_name.substring (0, input_name.length - soda_extension.length ) + scala_extension )
    else FileNamePair (input_name + soda_extension, input_name + scala_extension )

  def translate (input_file_name: String, output_file_name: String ): Boolean =
    {
      lazy val input = SimpleIOImpl () .read_file (input_file_name )
      lazy val output = MicroTranslatorImpl () .translate_program (input )
      SimpleIOImpl () .write_file (output_file_name, content = output ) }
}

case class Main () extends MainClass

case class FileNamePair (input_file_name: String, output_file_name: String )
