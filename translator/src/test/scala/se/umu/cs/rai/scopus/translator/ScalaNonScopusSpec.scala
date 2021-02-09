package se.umu.cs.rai.scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

/**
 * This tests how translation is done for Scala reserved words that are not Scopus reserved words.
 */
case class ScalaNonScopusSpec() extends AnyFunSuite {


  test("Scala reserved words are replaced"){
    val program = "" +
      "\nval x = 0" +
      "\nwhile (x != 0)"
    val expected = "" +
      "\nval __scopus__val x = 0" +
      "\n__scopus__while (x != 0)" +
      "\n"
    val obtained = MicroTranslator().translateProgram(program)
    assert (obtained == expected)
  }

}
