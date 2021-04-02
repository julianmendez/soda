package scopus.translator.language

import org.scalatest.funsuite.AnyFunSuite

/**
 * This tests how translation is done for Scala reserved words that are not Scopus reserved words.
 */
case class ScalaNonScopusSpec (  ) extends AnyFunSuite {


  test ("Scala reserved words are replaced")  {
    lazy val program = "" +
      "\nval x = 0" +
      "\nwhile (x != 0)"
    lazy val expected = "" +
      "\nlazy val __scopus__val x = 0" +
      "\n__scopus__while ( x != 0 )" +
      "\n"
    lazy val obtained = MicroTranslator (  ) .translate_program ( program )
    assert ( obtained == expected )
  }

}