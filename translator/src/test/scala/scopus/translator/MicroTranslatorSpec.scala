package scopus.translator

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

case class MicroTranslatorSpec() extends AnyFunSuite {

  lazy val InputFileName00 = "/scopus/translator/example/SwapExample.scopus"
  lazy val ExpectedFileName00 = "/scopus/translator/example/SwapExample.scala"

  lazy val InputFileName01 = "/scopus/translator/example/FiboExample.scopus"
  lazy val ExpectedFileName01 = "/scopus/translator/example/FiboExample.scala"

  lazy val InputFileName02 = "/scopus/translator/example/FactorialConcise.scopus"
  lazy val ExpectedFileName02 = "/scopus/translator/example/FactorialConcise.scala"

  lazy val InputFileName03 = "/scopus/translator/example/FactorialVerbose.scopus"
  lazy val ExpectedFileName03 = "/scopus/translator/example/FactorialVerbose.scala"

  lazy val InputFileName04 = "/scopus/translator/example/Fairness.scopus"
  lazy val ExpectedFileName04 = "/scopus/translator/example/Fairness.scala"

  lazy val InputFileName05 = "/scopus/translator/documentation/Manual.scopus"
  lazy val ExpectedFileName05 = "/scopus/translator/documentation/Manual.scala"

  lazy val InputFileName06 = "/scopus/translator/example/PiIterator.scopus"
  lazy val ExpectedFileName06 = "/scopus/translator/example/PiIterator.scala"


  def test_translation(input_file_name: String, expected_file_name: String): Assertion = {
    lazy val input_file = read_file(input_file_name)
    lazy val expected = read_file(expected_file_name)
    lazy val obtained = MicroTranslator().translate_program(input_file)
    assert(obtained == expected)
  }

  def test_translation_in_scope(input_file_name: String, expected_file_name: String): Assertion = {
    lazy val input_file = read_file(input_file_name)
    lazy val expected = read_file(expected_file_name)
    lazy val obtained = MicroTranslator().translate_program(input_file)
    assert(obtained == expected)
  }

  def read_file(file_name: String): String = {
    lazy val document_resource = getClass.getResource(file_name)
    lazy val document_URI = document_resource.toURI
    lazy val document_path = Paths.get(document_URI)
    new String(Files.readAllBytes(document_path))
  }

  test("should translate the swap example") {
    test_translation(InputFileName00, ExpectedFileName00)
  }

  test("should translate the swap example with the translated translator") {
    test_translation_in_scope(InputFileName00, ExpectedFileName00)
  }

  test("should translate the Fibonacci example") {
    test_translation(InputFileName01, ExpectedFileName01)
  }

  test("should translate the Factorial examples") {
    test_translation(InputFileName02, ExpectedFileName02)
    test_translation(InputFileName03, ExpectedFileName03)
  }

  test("should translate the Fairness example") {
    test_translation(InputFileName04, ExpectedFileName04)
  }

  test("should translate the manual") {
    test_translation(InputFileName05, ExpectedFileName05)
  }

  test("should translate the example that calculates pi") {
    test_translation(InputFileName06, ExpectedFileName06)
  }

}
