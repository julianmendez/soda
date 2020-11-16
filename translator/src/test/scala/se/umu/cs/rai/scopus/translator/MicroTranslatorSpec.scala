package se.umu.cs.rai.scopus.translator

import java.nio.file.{Files, Paths}

import org.scalatest.Assertion
import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

case class MicroTranslatorSpec() extends AnyFunSpec {

  val InputFileName00 = "/se/umu/cs/rai/scopus/translator/example/SwapExample.scopus"
  val ExpectedFileName00 = "/se/umu/cs/rai/scopus/translator/example/SwapExample.scala"

  val InputFileName01 = "/se/umu/cs/rai/scopus/translator/example/FiboExample.scopus"
  val ExpectedFileName01 = "/se/umu/cs/rai/scopus/translator/example/FiboExample.scala"

  val InputFileName02 = "/se/umu/cs/rai/scopus/translator/example/Factorial.scopus"
  val ExpectedFileName02 = "/se/umu/cs/rai/scopus/translator/example/Factorial.scala"

  val InputFileName03 = "/se/umu/cs/rai/scopus/translator/example/Fairness.scopus"
  val ExpectedFileName03 = "/se/umu/cs/rai/scopus/translator/example/Fairness.scala"

  val InputFileName04 = "/se/umu/cs/rai/scopus/translator/documentation/Manual.scopus"
  val ExpectedFileName04 = "/se/umu/cs/rai/scopus/translator/documentation/Manual.scala"

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

  it("should translate the Factorial example") {
    testTranslation(InputFileName02, ExpectedFileName02)
  }

  it("should translate the Fairness example") {
    testTranslation(InputFileName03, ExpectedFileName03)
  }

  it("should translate the manual") {
    testTranslation(InputFileName04, ExpectedFileName04)
  }

}
