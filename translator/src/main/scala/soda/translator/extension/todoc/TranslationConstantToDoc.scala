package soda.translator.extension.todoc

/**
 * This class contains constants that are specific for the documentation generation.
 */

trait TranslationConstantToDoc
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val doc_space = " "

  lazy val doc_new_line = "\n"

  lazy val doc_header = "\\begin{document}"

  lazy val doc_footer = "\\end{document}"

  lazy val doc_opening_comment_translation = "\n\\end{lstlisting}\n\n"

  lazy val doc_closing_comment_translation = "\n\n\\begin{lstlisting}\n"

  lazy val doc_opening_source_translation = ""

  lazy val doc_closing_source_translation = ""

}

case class TranslationConstantToDoc_ () extends TranslationConstantToDoc
