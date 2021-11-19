package soda.translator.language


case class FullTranslationSpec ()  extends org.scalatest.funsuite.AnyFunSuite {
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
  lazy val InANutshell = "inanutshell/InANutshell"

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
      lazy val obtained = MicroTranslatorToScala_ () .translate_program (input_file )
      assert (obtained == expected ) }

  def read_file (file_name: String ): String =
    {
      lazy val document_resource = getClass.getResource (file_name )
      lazy val document_URI = document_resource.toURI
      lazy val document_path = Paths.get (document_URI )
      new String (Files.readAllBytes (document_path )  ) }

  test ("should translate the swap example")
    {
      lazy val result = test_translation (SwapExample )
      result }

  test ("should translate the Fibonacci example")
    {
      lazy val result = test_translation (FiboExample )
      result }

  test ("should translate the Factorial Concise example")
    {
      lazy val result = test_translation (FactorialConcise )
      result }

  test ("should translate the Factorial Verbose example")
    {
      lazy val result = test_translation (FactorialVerbose )
      result }

  test ("should translate the Fairness example")
    {
      lazy val result = test_translation (Fairness )
      result }

  test ("should translate the example that calculates pi")
    {
      lazy val result = test_translation (PiIterator )
      result }

  test ("should translated Soda code that uses Scala reserved words as variables and functions")
    {
      lazy val result = test_translation (ScalaReservedWordEscaping )
      result }

  test ("should translate the manual In A Nutshell")
    {
      lazy val result = test_translation (InANutshell )
      result }

  test ("should translate the manual")
    {
      lazy val result = test_translation (ManualInput, ManualExpected )
      result }

}
