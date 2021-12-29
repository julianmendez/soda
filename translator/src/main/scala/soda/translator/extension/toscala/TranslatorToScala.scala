package soda.translator.extension.toscala

/**
 * This translates Soda source code to Scala source code.
 */
trait TranslatorToScala  extends soda.translator.extension.common.Extension {

  import soda.translator.parser.BlockProcessor_
  import soda.translator.io.DirectoryProcessor_
  import soda.translator.io.SimpleFileReader_
  import soda.translator.io.SimpleFileWriter_
  import java.io.File

  lazy val soda_extension: String = ".soda"

  lazy val scala_extension: String = ".scala"

  lazy val default_argument = "."

  def execute (arguments: Seq [String]  ): Boolean =
    if (arguments.length == 0 ) process_directory (default_argument )
    else if (arguments.length == 1 ) process_directory (arguments (0 )  )
    else if (arguments.length == 2 ) translate (arguments (0 ), arguments (1 )  )
    else false

  def process_directory (start: String ): Boolean =
    DirectoryProcessor_ (start, process_soda_file ) .process ()

  def process_soda_file (file: File ): Boolean =
    {
      lazy val file_name = file.getAbsolutePath
      lazy val t = get_input_output_file_names (file_name )
      translate (t.input_file_name, t.output_file_name ) }

  def get_input_output_file_names (input_name: String ): FileNamePair =
    if (input_name.endsWith (soda_extension )
    ) FileNamePair_ (input_name, input_name.substring (0, input_name.length - soda_extension.length ) + scala_extension )
    else FileNamePair_ (input_name + soda_extension, input_name + scala_extension )

  def translate (input_file_name: String, output_file_name: String ): Boolean =
    {
      lazy val block_translator = MicroTranslatorToScala_ ()
      lazy val input = SimpleFileReader_ () .read_file (input_file_name )
      lazy val output = BlockProcessor_ (block_translator ) .translate (input )
      SimpleFileWriter_ () .write_file (output_file_name, content = output ) }

}

case class TranslatorToScala_ ()  extends TranslatorToScala

trait FileNamePair {

  def input_file_name: String

  def output_file_name: String

}

case class FileNamePair_ (input_file_name: String, output_file_name: String )  extends FileNamePair
