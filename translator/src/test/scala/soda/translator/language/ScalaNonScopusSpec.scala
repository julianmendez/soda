package soda.translator.language


/**
 * This tests how translation is done for Scala reserved words that are not Soda reserved words.
 */
trait ScalaNonSodaSpec  extends org.scalatest.funsuite.AnyFunSuite {

  test ("Scala reserved words are replaced") {
    lazy val program = "" +
      "\nval x = 0" +
      "\nwhile (x != 0)"
    lazy val expected = "" +
      "\nlazy val __soda__val x = 0" +
      "\n__soda__while (x != 0 )" +
      "\n"
    lazy val obtained = MicroTranslator_ () .translate_program (program )

    assert (obtained == expected )
  }
}

case class ScalaNonSodaSpec_ () extends ScalaNonSodaSpec
