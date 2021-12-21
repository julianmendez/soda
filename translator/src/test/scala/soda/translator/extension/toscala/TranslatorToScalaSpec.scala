package soda.translator.extension.toscala

case class MainSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val instance = TranslatorToScala_ ()

  test ("should get the input and output file names without path")
    {
      lazy val expected = FileNamePair_ ("my_file.soda", "my_file.scala")
      lazy val obtained = instance.get_input_output_file_names ("my_file.soda")
      assert (obtained == expected ) }

  test ("should get the input and output file names with path")
    {
      lazy val expected = FileNamePair_ ("/path/to/file.soda", "/path/to/file.scala")
      lazy val obtained = instance.get_input_output_file_names ("/path/to/file.soda")
      assert (obtained == expected ) }

}
