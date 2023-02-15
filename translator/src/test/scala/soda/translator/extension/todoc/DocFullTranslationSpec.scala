package soda.translator.extension.todoc

/*
 * This package contains tests for the translator to documents.
 */

case class DocFullTranslationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   org.scalatest.Assertion
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_
  import   java.nio.file.Files
  import   java.nio.file.Paths

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val base = "/soda/translator/documentation/"

  lazy val soda_suffix = ".soda"

  lazy val doc_suffix = ".tex"

  lazy val manual = "Manual"

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = base + file_name + soda_suffix) (expected_file_name = base + file_name + doc_suffix)

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained = TranslatorToDoc_ ().translate_content (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def read_file (file_name : String) : String =
    new String (
      Files.readAllBytes (
        Paths.get (getClass.getResource (file_name).toURI)
      )
    )

  test ("should translate the manual") (
    test_translation (manual)
  )

}
