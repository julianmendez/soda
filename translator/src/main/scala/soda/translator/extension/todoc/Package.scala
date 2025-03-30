package soda.translator.extension.todoc

/*
 * This package contains classes for documentation generation.
 */







trait DocBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.CommentAnnotation
  import   soda.translator.parser.annotation.CommentAnnotation_
  import   soda.translator.parser.tool.CommentDelimiterRemover

  private lazy val _tc = TranslationConstantToDoc .mk

  private def _prepend (prefix : String) (content : Seq [String] ) : Seq [String] =
    if ( content .isEmpty
    ) Seq [String] (prefix)
    else Seq [String] (prefix + content .head) .++ (content .tail)

  private def _append (suffix : String) (content : Seq [String] ) : Seq [String] =
    content .:+ (suffix)

  private def _add_end_and_begin (lines : Seq [String] ) : Seq [String] =
    _append (_tc .doc_closing_comment_translation) (
      _prepend (_tc .doc_opening_comment_translation) (lines)
    )

  private def _translate_comment (block : CommentAnnotation) : CommentAnnotation =
    CommentAnnotation .mk (
      BlockBuilder .mk .build (
        _add_end_and_begin (
          CommentDelimiterRemover .mk
            .remove_comment_delimiters (block .lines)
        )
      )
    )

  private def _translate_source_code (block : AnnotatedBlock) : CommentAnnotation =
    CommentAnnotation .mk (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case CommentAnnotation_ (block) => _translate_comment (CommentAnnotation_ (block) )
      case _otherwise => _translate_source_code (annotated_block)
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class DocBlockTranslator_ () extends DocBlockTranslator

object DocBlockTranslator {
  def mk : DocBlockTranslator =
    DocBlockTranslator_ ()
}


/**
 * This class generates documentation from Soda snippets.
 */

trait MicroTranslatorToDoc
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum
  import   soda.translator.block.BlockTranslatorPipeline_

  private lazy val _function_definition = BlockAnnotationEnum .mk .function_definition

  private lazy val _test_declaration = BlockAnnotationEnum .mk .test_declaration

  lazy val functions_and_tests = Seq (_function_definition , _test_declaration)

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        DocBlockTranslator .mk
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline .translate (block)

}

case class MicroTranslatorToDoc_ () extends MicroTranslatorToDoc

object MicroTranslatorToDoc {
  def mk : MicroTranslatorToDoc =
    MicroTranslatorToDoc_ ()
}


/**
 * This class contains constants that are specific for the documentation generation.
 */

trait TranslationConstantToDoc
{



  import   soda.translator.parser.SodaConstant

  lazy val soda_constant = SodaConstant .mk

  lazy val doc_new_line = "\n"

  lazy val doc_header =
    doc_document_class +
    doc_new_line +
    doc_new_line +
    doc_packages_text +
    doc_new_line +
    doc_new_line +
    doc_language_definitions_text +
    doc_new_line +
    doc_new_line +
    doc_begin_document +
    doc_new_line +
    doc_new_line +
    doc_begin_lstlisting +
    doc_new_line

  lazy val doc_footer =
    doc_new_line +
    doc_end_lstlisting +
    doc_new_line +
    doc_new_line +
    doc_end_document +
    doc_new_line +
    doc_new_line

  lazy val doc_opening_comment_translation =
    doc_new_line +
    doc_end_lstlisting +
    doc_new_line +
    doc_new_line

  lazy val doc_closing_comment_translation =
    doc_new_line +
    doc_new_line +
    doc_begin_lstlisting +
    doc_new_line

  lazy val doc_document_class = "\\documentclass[12pt,a4paper]{article}"

  lazy val doc_begin_document = "\\begin{document}"

  lazy val doc_end_document = "\\end{document}"

  lazy val doc_begin_lstlisting = "\\begin{lstlisting}"

  lazy val doc_end_lstlisting = "\\end{lstlisting}"

  lazy val soda_reserved_words_csv =
    (soda_constant .soda_reserved_words_words_only .++ (
      soda_constant .soda_reserved_words_annotations_only) ) .mkString (", ")

  lazy val doc_packages : Seq [String] =
    Seq (
      "\\usepackage[utf8]{inputenc}",
      "\\usepackage{xcolor}",
      "\\usepackage{hyperref}",
      "\\usepackage{listings}"
    )

  lazy val doc_packages_text : String =
    doc_packages .mkString ("\n")

  lazy val doc_language_definitions : Seq [String] =
    Seq (
      "\\lstdefinelanguage{Soda}{",
      "    morekeywords={" + soda_reserved_words_csv + "},",
      "    sensitive=true,",
      "    morecomment=[s]{" + soda_constant .comment_opening_symbol + "}{" +
        soda_constant .comment_closing_symbol + "},",
      "   morestring=[b]\"",
      "}",
      "",
      "\\lstset{frame=tb,",
      "    language=Soda,",
      "    aboveskip=3mm,",
      "    belowskip=3mm,",
      "    showstringspaces=false,",
      "    columns=flexible,",
      "    basicstyle={\\small\\ttfamily},",
      "    numbers=none,",
      "    numberstyle=\\tiny\\color{gray},",
      "    keywordstyle=\\color{blue},",
      "    commentstyle=\\color{gray},",
      "    stringstyle=\\color{teal},",
      "    breaklines=true,",
      "    breakatwhitespace=true,",
      "    tabsize=3",
      "}"
    )

  lazy val doc_language_definitions_text : String =
    doc_language_definitions .mkString ("\n")

  lazy val doc_directive_identifier : String = "latex"

}

case class TranslationConstantToDoc_ () extends TranslationConstantToDoc

object TranslationConstantToDoc {
  def mk : TranslationConstantToDoc =
    TranslationConstantToDoc_ ()
}


trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair

object FileNamePair {
  def mk (input_file_name : String) (output_file_name : String) : FileNamePair =
    FileNamePair_ (input_file_name, output_file_name)
}

/**
 * This generates documentation from Soda source code.
 */

trait TranslatorToDoc
  extends
    soda.translator.extension.common.Extension
{



  import   soda.translator.block.DefaultBlockSequenceTranslator
  import   soda.translator.io.DirectoryProcessor
  import   soda.translator.io.SimpleFileReader
  import   soda.translator.io.SimpleFileWriter
  import   soda.translator.parser.BlockProcessor
  import   java.io.File

  private lazy val _soda_extension : String = ".soda"

  private lazy val _doc_extension : String = ".tex"

  private lazy val _default_argument = "."

  private lazy val _tc = TranslationConstantToDoc .mk

  private lazy val _translator =
    BlockProcessor .mk (
      DefaultBlockSequenceTranslator .mk (
        MicroTranslatorToDoc .mk
      )
    )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair .input_file_name) (pair .output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (_soda_extension)
    ) FileNamePair .mk (input_name) (
      input_name .substring (0 , input_name .length - _soda_extension .length) + _doc_extension)
    else FileNamePair .mk (input_name + _soda_extension) (input_name + _doc_extension)

  private def _process_soda_file (file : File) : Boolean =
    _process_soda_file_with (_get_input_output_file_names (file .getAbsolutePath) )

  private def _process_directory (start : String) : Boolean =
    (DirectoryProcessor .mk (start) (_process_soda_file) ) .process ()

  def translate_content (input : String) : String =
    _tc .doc_header + _translator .translate (input) + _tc .doc_footer

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter .mk .write_file (
      output_file_name) (
      content = translate_content (input)
    )

  private def _translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input (
      SimpleFileReader .mk .read_file (input_file_name) ) (
      output_file_name
    )

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments .length match  {
      case 0 => _process_directory (_default_argument)
      case 1 => _process_directory (arguments (0) )
      case 2 => _translate (arguments (0) ) (arguments (1) )
      case _otherwise => false
    }

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

}

case class TranslatorToDoc_ () extends TranslatorToDoc

object TranslatorToDoc {
  def mk : TranslatorToDoc =
    TranslatorToDoc_ ()
}

