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

  test ("some synonyms are Scala reserved words") {
    lazy val program = "" +
      "\n* A0[B0 <: C0]" +
      "\nclass C0[D0 >: E0]" +
      "\nclass A1[B1 subtype C1]" +
      "\n* C1[D1 supertype E1]"
    lazy val expected = "" +
      "\ntrait A0 [B0 <: C0]" +
      "\ntrait C0 [D0 >: E0]" +
      "\ntrait A1 [B1 <: C1]" +
      "\ntrait C1 [D1 >: E1]" +
      "\n"
    lazy val obtained = MicroTranslator_ () .translate_program (program )

    assert (obtained == expected )
  }
}

case class ScalaNonSodaSpec_ () extends ScalaNonSodaSpec
