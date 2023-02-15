package soda.translator.extension.todoc

/*
 * This package contains classes for documentation generation.
 */



trait Package

trait DocBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.CommentAnnotation
  import   soda.translator.parser.annotation.CommentAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToDoc_ ()

  private lazy val _comment_line_prefix = _sc.comment_line_symbol + _sc.space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case CommentAnnotation_ (block) => _translate_comment (CommentAnnotation_ (block) )
      case x => _translate_source_code (x)
    }

  private def _translate_comment (block : CommentAnnotation) : CommentAnnotation =
    CommentAnnotation_ (
      BlockBuilder_ ().build (
        _append (
          _tc.doc_closing_comment_translation) (
          _prepend (_tc.doc_opening_comment_translation) (
            _remove_comment_delimiter (
              _remove_comment_line_prefix (block.lines)
            )
          )
        )
      )
    )

  private def _translate_source_code (block : AnnotatedBlock) : CommentAnnotation =
    CommentAnnotation_ (
      block
    )

  private def _prepend (prefix : String) (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) Seq [String] (prefix)
    else Seq[String] (prefix + content.head).++ (content.tail)

  private def _append (suffix : String) (content : Seq [String] ) : Seq [String] =
    content.:+ (suffix)

  private def _remove_comment_delimiter (content : Seq [String] ) : Seq [String] =
    _remove_last_delimiter (
      _remove_first_delimiter (content)
    )

  private def _remove_first_delimiter (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) content
    else _prepend (
      _remove_prefix_in_line (_sc.documentation_comment_opening_symbol) (
        _remove_prefix_in_line (_sc.comment_opening_symbol) (content.head)
     )
    ) (content.tail)

  private def _remove_last_delimiter (content : Seq [String] ) : Seq [String] =
    ( _remove_last_delimiter_on_first_line (content.reverse) ).reverse

  private def _remove_last_delimiter_on_first_line (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) content
    else _prepend (_remove_suffix_in_line (_sc.comment_closing_symbol) (content.head) ) (content.tail)

  private def _remove_comment_line_prefix (content : Seq [String] ) : Seq [String] =
    content.map (  line => _remove_prefix_in_line (_comment_line_prefix) (line) )

  private def _remove_prefix_in_line (prefix : String) (line : String) : String =
    _remove_prefix_in_line_at (line.indexOf (prefix) ) (prefix) (line)

  private def _remove_prefix_in_line_at (index : Int) (prefix : String) (line : String) : String =
    if ( index >= 0
    ) line.substring (index + prefix.length)
    else line

  private def _remove_suffix_in_line (suffix : String) (line : String) : String =
    _remove_suffix_in_line_at (line.lastIndexOf (suffix) ) (line)

  private def _remove_suffix_in_line_at (index : Int) (line : String) : String =
    if ( index >= 0
    ) line.substring (0, index)
    else line

}

case class DocBlockTranslator_ () extends DocBlockTranslator


/**
 * This class generates documentation from Soda snippets.
 */

trait MicroTranslatorToDoc
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_
  import   soda.translator.blocktr.TokenizedBlockTranslator_
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToDoc_ ()

  private lazy val _function_definition = BlockAnnotationEnum_ ().function_definition

  private lazy val _test_declaration = BlockAnnotationEnum_ ().test_declaration

  lazy val functions_and_tests = Seq (_function_definition, _test_declaration)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline.translate (block)

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        DocBlockTranslator_ ()
      )
    )

}

case class MicroTranslatorToDoc_ () extends MicroTranslatorToDoc


/**
 * This class contains constants that are specific for the documentation generation.
 */

trait TranslationConstantToDoc
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val doc_space = " "

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
    (soda_constant.soda_reserved_words_words_only.++ (soda_constant.soda_reserved_words_annotations_only) ).mkString (", ")

  lazy val doc_packages : Seq [String] =
    Seq (
      "\\usepackage[utf8]{inputenc}",
      "\\usepackage{xcolor}",
      "\\usepackage{hyperref}",
      "\\usepackage{listings}"
    )

  lazy val doc_packages_text : String =
    doc_packages.mkString ("\n")

  lazy val doc_language_definitions : Seq [String] =
    Seq (
      "\\lstdefinelanguage{Soda}{",
      "    morekeywords={" + soda_reserved_words_csv + "},",
      "    sensitive=true,",
      "    morecomment=[s]{" + soda_constant.comment_opening_symbol + "}{" + soda_constant.comment_closing_symbol + "},",
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
    doc_language_definitions.mkString ("\n")

}

case class TranslationConstantToDoc_ () extends TranslationConstantToDoc


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

