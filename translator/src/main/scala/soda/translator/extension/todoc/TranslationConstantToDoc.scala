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
      "\\usepackage{listings}",
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
