package se.umu.cs.rai.scopus.translator

import java.nio.file.{Files, Paths}

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

case class MicroTranslatorSpec() extends AnyFunSuite {

  val InputFileName00 = "/se/umu/cs/rai/scopus/translator/example/SwapExample.scopus"
  val ExpectedFileName00 = "/se/umu/cs/rai/scopus/translator/example/SwapExample.scala"

  val InputFileName01 = "/se/umu/cs/rai/scopus/translator/example/FiboExample.scopus"
  val ExpectedFileName01 = "/se/umu/cs/rai/scopus/translator/example/FiboExample.scala"

  val InputFileName02 = "/se/umu/cs/rai/scopus/translator/example/FactorialConcise.scopus"
  val ExpectedFileName02 = "/se/umu/cs/rai/scopus/translator/example/FactorialConcise.scala"

  val InputFileName03 = "/se/umu/cs/rai/scopus/translator/example/FactorialVerbose.scopus"
  val ExpectedFileName03 = "/se/umu/cs/rai/scopus/translator/example/FactorialVerbose.scala"

  val InputFileName04 = "/se/umu/cs/rai/scopus/translator/example/Fairness.scopus"
  val ExpectedFileName04 = "/se/umu/cs/rai/scopus/translator/example/Fairness.scala"

  val InputFileName05 = "/se/umu/cs/rai/scopus/translator/documentation/Manual.scopus"
  val ExpectedFileName05 = "/se/umu/cs/rai/scopus/translator/documentation/Manual.scala"

  val InputFileName06 = "/se/umu/cs/rai/scopus/translator/example/PiIterator.scopus"
  val ExpectedFileName06 = "/se/umu/cs/rai/scopus/translator/example/PiIterator.scala"


  def testTranslation(inputFileName: String, expectedFileName: String): Assertion = {
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

  def testTranslationInScope(inputFileName: String, expectedFileName: String): Assertion = {
    val inputFile = readFile(inputFileName)
    val expectedFile = readFile(expectedFileName)
    val obtainedFile = MicroTranslator().translateProgram(inputFile)
    assert(obtainedFile === expectedFile)
  }

  test("should translate the swap example") {
    testTranslation(InputFileName00, ExpectedFileName00)
  }

  test("should translate the swap example with the translated translator") {
    testTranslationInScope(InputFileName00, ExpectedFileName00)
  }

  test("should translate the Fibonacci example") {
    testTranslation(InputFileName01, ExpectedFileName01)
  }

  test("should translate the Factorial examples") {
    testTranslation(InputFileName02, ExpectedFileName02)
    testTranslation(InputFileName03, ExpectedFileName03)
  }

  test("should translate the Fairness example") {
    testTranslation(InputFileName04, ExpectedFileName04)
  }

  test("should translate the manual") {
    testTranslation(InputFileName05, ExpectedFileName05)
  }

  test("should translate the example that calculates pi") {
    testTranslation(InputFileName06, ExpectedFileName06)
  }

}
