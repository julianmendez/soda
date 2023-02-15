package soda.translator.extension.toscala

/*
 * This package contains tests for the translator to Scala.
 */

case class MainSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val instance = TranslatorToScala_ ()

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should get the input and output file names without path") (
    check (
      obtained = instance.get_input_output_file_names("my_file.soda")
    ) (
      expected = FileNamePair_("my_file.soda", "my_file.scala")
    )
  )

  test ("should get the input and output file names with path") (
    check (
      obtained = instance.get_input_output_file_names("/path/to/file.soda")
    ) (
      expected = FileNamePair_("/path/to/file.soda", "/path/to/file.scala")
    )
  )

}
