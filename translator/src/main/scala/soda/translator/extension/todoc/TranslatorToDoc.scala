package soda.translator.extension.todoc

/*
 * This package contains classes for documentation generation.
 */





/**
 * This generates documentation from Soda source code.
 */

trait TranslatorToDoc
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.io.DirectoryProcessor_
  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   soda.translator.parser.BlockProcessor_
  import   java.io.File

  private lazy val _soda_extension : String = ".soda"

  private lazy val _doc_extension : String = ".tex"

  private lazy val _default_argument = "."

  private lazy val _tc = TranslationConstantToDoc_ ()

  private lazy val _translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToDoc_ ()
      )
    )

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments.length match  {
      case 0 => _process_directory (_default_argument)
      case 1 => _process_directory (arguments (0) )
      case 2 => _translate (arguments (0) ) (arguments (1) )
      case x => false
    }

  def translate_content (input : String) : String =
    _tc.doc_header + _translator.translate (input) + _tc.doc_footer

  private def _process_directory (start : String) : Boolean =
    DirectoryProcessor_ (start, _process_soda_file).process ()

  private def _process_soda_file (file : File) : Boolean =
    _process_soda_file_with (_get_input_output_file_names (file.getAbsolutePath) )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair.input_file_name) (pair.output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name.endsWith (_soda_extension)
    ) FileNamePair_ (input_name,
      input_name.substring (0, input_name.length - _soda_extension.length) + _doc_extension)
    else FileNamePair_ (input_name + _soda_extension, input_name + _doc_extension)

  private def _translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input (
      SimpleFileReader_ ().read_file (input_file_name) ) (
      output_file_name
    )

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter_ ().write_file (
      output_file_name) (
      content = translate_content (input)
    )

}

case class TranslatorToDoc_ () extends TranslatorToDoc

trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair
