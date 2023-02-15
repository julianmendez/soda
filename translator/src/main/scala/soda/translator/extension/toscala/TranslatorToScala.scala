package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





/**
 * This translates Soda source code to Scala source code.
 */

trait TranslatorToScala
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.io.DirectoryProcessor_
  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   soda.translator.parser.BlockProcessor_
  import   java.io.File

  lazy val package_file_name : String = "Package.soda"

  lazy val prelude_file_name : String = package_file_name

  lazy val package_scala_file_name : String = "Package.scala"

  lazy val file_separator : String = File.separator

  lazy val default_prelude : String = ""

  lazy val new_line : String = "\n"

  lazy val prelude_separation : String = new_line + new_line

  lazy val prelude_file_body : String = new_line + "trait Package" + prelude_separation

  lazy val package_option_1 = "-p"

  lazy val package_option_2 = "--package"

  private lazy val _soda_extension : String = ".soda"

  private lazy val _scala_extension : String = ".scala"

  private lazy val _default_argument = "."

  private lazy val _translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments.length match  {
      case 0 => _process_directory (_default_argument)
      case 1 => _process_directory (arguments (0) )
      case 2 =>
        if ( _is_package_option (arguments (0) )
        ) _process_directory_with_package_option (arguments (1) )
        else _translate (arguments (0) ) (arguments (1) )
      case x => false
    }

  private def _process_directory (start : String) : Boolean =
    DirectoryProcessor_ (start, _process_soda_file).process ()

  private def _process_soda_file (file : File) : Boolean =
    _process_soda_file_with (get_input_output_file_names (file.getAbsolutePath) )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair.input_file_name) (pair.output_file_name)

  private def _process_directory_with_package_option (start : String) : Boolean =
    DirectoryProcessor_ (start, _process_soda_file_with_package_option).process ()

  private def _process_soda_file_with_package_option (file : File) : Boolean =
    if ( file.getName == package_file_name
    ) _process_soda_file (file)
    else _process_soda_file_with_package_option_with (
      get_input_output_file_names_with_package_option (file.getAbsolutePath) (file.getParent)
    )

  private def _process_soda_file_with_package_option_with (pair : FileNamePair) : Boolean =
      _translate_append (pair.input_file_name) (pair.output_file_name)

  def get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name.endsWith (_soda_extension)
    ) FileNamePair_ (input_name, input_name.substring (0, input_name.length - _soda_extension.length) + _scala_extension)
    else FileNamePair_ (input_name + _soda_extension, input_name + _scala_extension)

  def get_input_output_file_names_with_package_option (input_name : String) (parent_name : String) : FileNamePair =
    if ( input_name.endsWith (_soda_extension)
    ) FileNamePair_ (input_name , parent_name + file_separator + package_scala_file_name )
    else FileNamePair_ (input_name + _soda_extension, input_name + _scala_extension)

  private def _translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input ( _read_input_with_prelude (input_file_name) ) (output_file_name)

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter_ ().write_file (output_file_name) (content = _translator.translate (input) )

  private def _translate_append (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_append_with_input ( _read_input (input_file_name) ) (output_file_name)

  private def _translate_append_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter_ ().append_file (output_file_name) (content = _translator.translate (input) )

  private def _read_input_with_prelude (input_file_name : String) : String =
    if ( _is_a_prelude_file (input_file_name)
    ) _read_input (input_file_name) + prelude_file_body
    else _get_prelude (input_file_name) + _read_input (input_file_name)

  private def _read_input (input_file_name : String) : String =
    SimpleFileReader_ ().read_file (input_file_name)

  private def _get_prelude (input_file_name : String) : String =
    _get_prelude_with (_get_prelude_file (input_file_name) )

  private def _get_prelude_with (prelude_file : File) : String =
    if ( prelude_file.exists
    ) (SimpleFileReader_ ().read_file (prelude_file.getAbsolutePath) ) + prelude_separation
    else default_prelude

  private def _get_prelude_file (input_file_name : String) : File =
    new File ( new File (input_file_name) .getParentFile , prelude_file_name )

  private def _is_a_prelude_file (input_file_name : String) : Boolean =
    prelude_file_name == ( ( new File (input_file_name) ) .getName)

  private def _is_package_option (s : String) : Boolean =
    (s == package_option_1) || (s == package_option_2)

}

case class TranslatorToScala_ () extends TranslatorToScala

trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair
