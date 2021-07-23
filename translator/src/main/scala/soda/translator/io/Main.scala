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

  lazy val SodaExtension: String = ".soda"
  lazy val ScalaExtension: String = ".scala"

  lazy val Help: String = SimpleIOImpl () .read_resource ("/soda/translator/documentation/help.txt")

  lazy val Title_and_version: String =
    {
      lazy val packg = this.getClass.getPackage
      lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
      lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
      (name + " " + version ) .trim }

  def main (args: Array [String]  ): Unit =
    if (args.length == 1 ) process_directory (args (0 )  )
    else if (args.length == 2 ) translate (args (0 ), args (1 )  )
    else println (Title_and_version + "\n\n" + Help )

  def process_directory (start: String ): Boolean =
    DirectoryProcessorImpl (start, process_soda_file ) .process ()

  def process_soda_file (file: File ): Boolean =
    {
      lazy val file_name = file.getAbsolutePath
      lazy val t = get_input_output_file_names (file_name )
      translate (t.input_file_name, t.output_file_name ) }

  def get_input_output_file_names (input_name: String ): FileNamePair =
    if (input_name.endsWith (SodaExtension )
    ) FileNamePair (input_name, input_name.substring (0, input_name.length - SodaExtension.length ) + ScalaExtension )
    else FileNamePair (input_name + SodaExtension, input_name + ScalaExtension )

  def translate (input_file_name: String, output_file_name: String ): Boolean =
    {
      lazy val input = SimpleIOImpl () .read_file (input_file_name )
      lazy val output = MicroTranslatorImpl () .translate_program (input )
      SimpleIOImpl () .write_file (output_file_name, content = output ) }
}

case class Main () extends MainClass

case class FileNamePair (input_file_name: String, output_file_name: String )
