package se.umu.cs.rai.scopus.translator

import java.nio.file.{Files, Paths}

import org.scalatest.Assertion
import org.scalatest.funspec.AnyFunSpec

case class MicroTranslatorSpec() extends AnyFunSpec {

  val InputFileName00 = "/scopus/se/umu/cs/rai/scopus/translator/example/SwapExample.scopus"
  val ExpectedFileName00 = "/scala/se/umu/cs/rai/scopus/translator/example/SwapExample.scala"

  val InputFileName01 = "/scopus/se/umu/cs/rai/scopus/translator/example/FiboExample.scopus"
  val ExpectedFileName01 = "/scala/se/umu/cs/rai/scopus/translator/example/FiboExample.scala"

  val InputFileName02 = "/scopus/se/umu/cs/rai/scopus/translator/example/fairness/Applicant.scopus"
  val ExpectedFileName02 = "/scala/se/umu/cs/rai/scopus/translator/example/fairness/Applicant.scala"

  val InputFileName03 = "/scopus/se/umu/cs/rai/scopus/translator/example/fairness/Fairness.scopus"
  val ExpectedFileName03 = "/scala/se/umu/cs/rai/scopus/translator/example/fairness/Fairness.scala"

  val InputFileName04 = "/scopus/se/umu/cs/rai/scopus/translator/example/equity/FairBox.scopus"
  val ExpectedFileName04 = "/scala/se/umu/cs/rai/scopus/translator/example/equity/FairBox.scala"

  def testTranslation(inputFileName: String, expectedFileName: String): Assertion = {
    val inputFile = readFile(inputFileName)
    val expectedFile = readFile(expectedFileName)
    val obtainedFile = MicroTranslator().translateProgram(inputFile)
    assert(obtainedFile === expectedFile)
  }

  def testTranslationInScope(inputFileName: String, expectedFileName: String): Assertion = {
    val inputFile = readFile(inputFileName)
    val expectedFile = readFile(expectedFileName)
    val obtainedFile = MicroTranslator().translateProgram(inputFile)
    assert(obtainedFile === expectedFile)
  }

  def readFile(fileName: String): String = {
    val documentResource = getClass.getResource(fileName)
    val documentURI = documentResource.toURI
    val documentPath = Paths.get(documentURI)
    new String(Files.readAllBytes(documentPath))
  }

  it("should translate the swap example") {
    testTranslation(InputFileName00, ExpectedFileName00)
  }

  it("should translate the swap example with the translated translator") {
    testTranslationInScope(InputFileName00, ExpectedFileName00)
  }

  it("should translate the Fibonacci example") {
    testTranslation(InputFileName01, ExpectedFileName01)
  }

  it("should translate the Fibonacci example with the translated translator") {
    testTranslationInScope(InputFileName01, ExpectedFileName01)
  }

  it("should translate the Fairness example") {
    testTranslation(InputFileName02, ExpectedFileName02)
    testTranslation(InputFileName03, ExpectedFileName03)
  }

  it("should translate the Equity example") {
    testTranslation(InputFileName04, ExpectedFileName04)
  }

}
