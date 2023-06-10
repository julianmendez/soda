package soda.translator.extension.tocoq

/*
 * This package contains test for the translator to Coq.
 */
trait Package

case class CoqFullTranslationSpec ()
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

  lazy val Base = "/soda/translator/example/"

  lazy val SodaSuffix = ".soda"

  lazy val CoqSuffix = ".v"

  lazy val SwapExample = "algorithms/SwapExample"

  lazy val TriangularNumber = "forcoq/mathematics/TriangularNumberForCoq"

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToCoq_()
          )
        ) .translate (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = Base + file_name + SodaSuffix) (expected_file_name = Base + file_name + CoqSuffix)

  test ("should translate the swap example") (
    test_translation (SwapExample)
  )

  test ("should translate the example of triangular numbers") (
    test_translation (TriangularNumber)
  )

}


case class MicroTranslatorToCoqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToCoq_ ()
      )
    )

  test ("Coq translation of a constant") (
    check (
      obtained = instance.translate ("x = 0")
    ) (
      expected = " Definition x := 0\n.\n"
    )
  )

  test ("Coq translation of a function") (
    check (
      obtained = instance.translate ("f (a: nat) = 0")
    ) (
      expected = " Definition f (a: nat) := 0\n.\n"
    )
  )

}

