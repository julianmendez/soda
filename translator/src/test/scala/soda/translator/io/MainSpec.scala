package soda.translator.io

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

case class MainSpec () extends AnyFunSuite {

  test ("should get the input and output file names without path") {
    lazy val instance = Main ()
    lazy val expected = instance.FileNamePair ("my_file.soda", "my_file.scala")
    lazy val obtained = instance.get_input_output_file_names ("my_file.soda")
    assert (obtained == expected )
  }

  test ("should get the input and output file names with path") {
    lazy val instance = Main ()
    lazy val expected = instance.FileNamePair ("/path/to/file.soda", "/path/to/file.scala")
    lazy val obtained = instance.get_input_output_file_names ("/path/to/file.soda")
    assert (obtained == expected )
  }

}
