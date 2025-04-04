package soda.translator.extension.todoc

/*
 * This package contains tests for the translator to documents.
 */



case class DocFullTranslationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   org.scalatest.Assertion
  import   java.nio.file.Files
  import   java.nio.file.Paths

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val base = "/soda/translator/documentation/"

  lazy val soda_suffix = ".soda"

  lazy val doc_suffix = ".tex"

  lazy val manual = "Manual"

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained = TranslatorToDoc .mk .translate_content (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = base + file_name + soda_suffix) (
      expected_file_name = base + file_name + doc_suffix)

  test ("should translate the manual") (
    test_translation (manual)
  )

}


case class MicroTranslatorToDocSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator
  import   soda.translator.parser.BlockProcessor

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor .mk (
      DefaultBlockSequenceTranslator .mk (
        MicroTranslatorToDoc .mk
      )
    )

  lazy val original_snippet_1 = "" +
    "/*" +
    "\n * This is an example" +
    "\n*/" +
    "\n"

  lazy val translated_snippet_1 = "" +
    "\n\\end{lstlisting}" +
    "\n" +
    "\n" +
    "\n This is an example" +
    "\n" +
    "\n" +
    "\n" +
    "\n\\begin{lstlisting}" +
    "\n" +
    "\n"

  test ("Document generation of a snippet 1") (
    check (
      obtained = instance .translate (original_snippet_1)
    ) (
      expected = translated_snippet_1
    )
  )

  lazy val original_snippet_2 = "" +
    "\n/** These are the Soda docs. */\n"

  lazy val translated_snippet_2 = "" +
    "\n\\end{lstlisting}" +
    "\n" +
    "\nThese are the Soda docs." +
    "\n" +
    "\n" +
    "\n\\begin{lstlisting}" +
    "\n" +
    "\n"

  test ("Document generation of a snippet 2") (
    check (
      obtained = instance .translate (original_snippet_2)
    ) (
      expected = translated_snippet_2
    )
  )

  lazy val original_snippet_3 = "" +
    "\n/* This is a multiline comment." +
    "\n * Each line starts with an asterisk '*'." +
    "\n * *Only* the first asterisk '*' is removed." +
    "\n But it is not mandatory." +
    "\n It is only for clarity. */\n"

  lazy val translated_snippet_3 = "" +
    "\n\\end{lstlisting}" +
    "\n" +
    "\nThis is a multiline comment." +
    "\n Each line starts with an asterisk '*'." +
    "\n *Only* the first asterisk '*' is removed." +
    "\n But it is not mandatory." +
    "\n It is only for clarity." +
    "\n" +
    "\n" +
    "\n\\begin{lstlisting}" +
    "\n" +
    "\n"

  test ("Document generation of a snippet 3") (
    check (
      obtained = instance .translate (original_snippet_3)
    ) (
      expected = translated_snippet_3
    )
  )

}

