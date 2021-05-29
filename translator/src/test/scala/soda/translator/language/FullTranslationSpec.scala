package soda.translator.language


case class FullTranslationSpec () extends org.scalatest.funsuite.AnyFunSuite {
  import org.scalatest.Assertion
  import java.nio.file.{ Files, Paths }

  lazy val Base = "/soda/translator/example/"

  lazy val SodaSuffix = ".soda"
  lazy val ScalaSuffix = ".scala"

  lazy val SwapExample = "SwapExample"
  lazy val FiboExample = "FiboExample"
  lazy val FactorialConcise = "FactorialConcise"
  lazy val FactorialVerbose = "FactorialVerbose"
  lazy val Fairness = "Fairness"
  lazy val PiIterator = "PiIterator"
  lazy val ScalaReservedWordEscaping = "ScalaReservedWordEscaping"

  lazy val ManualInput = "/soda/translator/documentation/Manual.soda"
  lazy val ManualExpected = "/soda/translator/documentation/Manual.scala"

  def test_translation (file_name: String ): Assertion =
    {
      lazy val input_file_name = Base + file_name + SodaSuffix
      lazy val expected_file_name = Base + file_name + ScalaSuffix
      test_translation (input_file_name, expected_file_name ) }

  def test_translation (input_file_name: String, expected_file_name: String ): Assertion =
    {
      lazy val input_file = read_file (input_file_name )
      lazy val expected = read_file (expected_file_name )
      lazy val obtained = MicroTranslator () .translate_program (input_file )
      assert (obtained == expected ) }

  def read_file (file_name: String ): String =
    {
      lazy val document_resource = getClass.getResource (file_name )
      lazy val document_URI = document_resource.toURI
      lazy val document_path = Paths.get (document_URI )
      new String (Files.readAllBytes (document_path )  ) }

  test ("should translate the swap example") {
    test_translation (SwapExample )
  }

  test ("should translate the Fibonacci example") {
    test_translation (FiboExample )
  }

  test ("should translate the Factorial Concise example") {
    test_translation (FactorialConcise )
  }

  test ("should translate the Factorial Verbose example") {
    test_translation (FactorialVerbose )
  }

  test ("should translate the Fairness example") {
    test_translation (Fairness )
  }

  test ("should translate the example that calculates pi") {
    test_translation (PiIterator )
  }

  test ("should translated Soda code that uses Scala reserved words as variables and functions") {
    test_translation (ScalaReservedWordEscaping )
  }

  test ("should translate the manual") {
    test_translation (ManualInput, ManualExpected )
  }
}
