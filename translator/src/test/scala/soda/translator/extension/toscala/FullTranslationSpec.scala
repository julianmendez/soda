package soda.translator.extension.toscala

case class FullTranslationSpec ()
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

  lazy val ScalaSuffix = ".scala"

  lazy val SwapExample = "algorithms/SwapExample"

  lazy val FiboExample = "mathematics/FiboExample"

  lazy val FactorialConcise = "mathematics/FactorialConcise"

  lazy val FactorialVerbose = "mathematics/FactorialVerbose"

  lazy val Fairness = "ethicalissues/fairness/Fairness"

  lazy val PiIterator = "mathematics/PiIterator"

  lazy val ScalaReservedWordEscaping = "algorithms/ScalaReservedWordEscaping"

  lazy val InANutshell = "inanutshell/InANutshell"

  lazy val ManualInput = "/soda/translator/documentation/Manual.soda"

  lazy val ManualExpected = "/soda/translator/documentation/Manual.scala"

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = Base + file_name + SodaSuffix) (expected_file_name = Base + file_name + ScalaSuffix)

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToScala_()
          )
        ).translate (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def read_file (file_name : String) : String =
    new String (
      Files.readAllBytes (
        Paths.get (getClass.getResource (file_name).toURI)
      )
    )

  test ("should translate the swap example") (
    test_translation (SwapExample)
  )

  test ("should translate the Fibonacci example") (
    test_translation (FiboExample)
  )

  test ("should translate the Factorial Concise example") (
    test_translation (FactorialConcise)
  )

  test ("should translate the Factorial Verbose example") (
    test_translation (FactorialVerbose)
  )

  test ("should translate the Fairness example") (
    test_translation (Fairness)
  )

  test ("should translate the example that calculates pi") (
    test_translation (PiIterator)
  )

  test ("should translated Soda code that uses Scala reserved words as variables and functions") (
    test_translation (ScalaReservedWordEscaping)
  )

  test ("should translate the manual In A Nutshell") (
    test_translation (InANutshell)
  )

  test ("should translate the manual") (
    test_translation_with (ManualInput) (ManualExpected)
  )

}
