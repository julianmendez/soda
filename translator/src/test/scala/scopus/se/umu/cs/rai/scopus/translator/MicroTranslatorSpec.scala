package scopus.se.umu.cs.rai.scopus.translator

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions

case class MicroTranslatorSpec() extends AnyFunSuite {

  val InputFileName00 = "/scopus/se/umu/cs/rai/scopus/translator/example/SwapExample.scopus"
  val ExpectedFileName00 = "/scopus/se/umu/cs/rai/scopus/translator/example/SwapExample.scala"

  val InputFileName01 = "/scopus/se/umu/cs/rai/scopus/translator/example/FiboExample.scopus"
  val ExpectedFileName01 = "/scopus/se/umu/cs/rai/scopus/translator/example/FiboExample.scala"

  val InputFileName02 = "/scopus/se/umu/cs/rai/scopus/translator/example/FactorialConcise.scopus"
  val ExpectedFileName02 = "/scopus/se/umu/cs/rai/scopus/translator/example/FactorialConcise.scala"

  val InputFileName03 = "/scopus/se/umu/cs/rai/scopus/translator/example/FactorialVerbose.scopus"
  val ExpectedFileName03 = "/scopus/se/umu/cs/rai/scopus/translator/example/FactorialVerbose.scala"

  val InputFileName04 = "/scopus/se/umu/cs/rai/scopus/translator/example/Fairness.scopus"
  val ExpectedFileName04 = "/scopus/se/umu/cs/rai/scopus/translator/example/Fairness.scala"

  val InputFileName05 = "/scopus/se/umu/cs/rai/scopus/translator/documentation/Manual.scopus"
  val ExpectedFileName05 = "/scopus/se/umu/cs/rai/scopus/translator/documentation/Manual.scala"

  val InputFileName06 = "/scopus/se/umu/cs/rai/scopus/translator/example/PiIterator.scopus"
  val ExpectedFileName06 = "/scopus/se/umu/cs/rai/scopus/translator/example/PiIterator.scala"


  def test_translation(input_file_name: String, expected_file_name: String): Assertion = {
    val input_file = read_file(input_file_name)
    val expected_file = read_file(expected_file_name)
    val obtained_file = MicroTranslator().translate_program(input_file)
    assert(obtained_file === expected_file)
  }

  def test_translation_in_scope(input_file_name: String, expected_file_name: String): Assertion = {
    val input_file = read_file(input_file_name)
    val expected_file = read_file(expected_file_name)
    val obtained_file = MicroTranslator().translate_program(input_file)
    assert(obtained_file === expected_file)
  }

  def read_file(file_name: String): String = {
    val document_resource = getClass.getResource(file_name)
    val document_URI = document_resource.toURI
    val document_path = Paths.get(document_URI)
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
